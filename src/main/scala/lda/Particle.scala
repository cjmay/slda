package lda

import scala.annotation.tailrec
import scala.collection.mutable.{ ArrayBuffer => ArrayBuffer }
import scala.collection.mutable.{ HashMap => HashMap }
import scala.util.{ Random => Random }

import globals.Constants
import stream._

/** A memory- and time-efficient way to represent particles, as detailed in
 section 4 of Canini Shi Griffiths. Manages all the logic of particle
 manipulation, copying, reading, etc */
class ParticleStore (val T: Int, val alpha: Double, val beta: Double,
                     val numParticles: Int, val ess: Double,
                     val rejuvBatchSize: Int,
                     var rejuvSeq: ReservoirSampler[Array[String]]) {
  var currId = 0  // NOTE: This must come before initParticles, otherwise it
                  // resets the id count
  var (assgStore,particles) = initParticles()

  /** Creates all the particles (requested by `numParticles` parameter), as
   as the corresponding entry in the AssignmentStore that tracks the topic
   assignments of the words for each particle. */
  private def initParticles (): (AssignmentStore, Array[Particle]) = {
    var store = new AssignmentStore()
    var particles = Array.fill[Particle](numParticles)(null)
    for (i <- 0 to numParticles-1) {
      val id = newAssignStoreId()
      store.newParticle(id, Constants.NoParent)
      particles(i) = new Particle(T, 1.0/numParticles, alpha, beta, rejuvSeq,
                                  store, id)
    }
    (store, particles)
  }

  /** Reweights particles proportional to the probability they account for the
   data. UNNORMALIZED. Does this for every particle. Corresponds to line 4 of
   Algorithm 4 in Canini paper */
  def unnormalizedReweightAll (currword: String, currVocabSize: Int): Unit = {
    particles.foreach { particle =>
      particle.unnormalizedReweight(currword, currVocabSize) }
  }

  /** Takes weights of particles, normalizes them, writes them back; note:
   SIDE-EFFECTS. */
  def normalizeWeights (): Unit = {
    var weights = particleWeightArray()
    Stats.normalize(weights)
    for (i <- 0 to numParticles-1) particles(i).weight = weights(i)
  }

  /** Performs transition for every particle (see comment on particle method)
   */
  def transitionAll (index: Int, words: Array[String], currVocabSize: Int,
                     docId: Int): Unit = {
    particles.foreach { particle =>
      particle.transition(index, words, currVocabSize,docId) }
  }

  /** Performs update necessary for new document */
  def newDocumentUpdateAll (indexIntoSample: Int, doc: Array[String]): Unit = {
    particles.foreach { particle =>
      particle.newDocumentUpdate(indexIntoSample, doc) }
  }

  /** Resamples particles proportional to their probability */
  def resample (unnormalizedWeights: Array[Double]): Unit = {
    particles = multinomialResample(unnormalizedWeights)
  }

  /** Gets inverse 2-norm of particle weights, check against ESS */
  def shouldRejuvenate (): Boolean = {
    val weights = particleWeightArray()
    val statistic = 1/math.pow(Math.norm(weights, 2), 2)
    // NOTE: this is useful for tuning the ESS threshold
    //println(statistic + " " + ess)
    statistic <= ess
  }

  /** Prepares to perform, and performs rejuvenation MCMC step */
  def rejuvenate (wordIds: Array[(Int,Int)], currVocabSize: Int): Unit = {
		print(".")
    //val now = System.currentTimeMillis
    resample(particleWeightArray())
    assgStore.prune
    // TODO: HACKY TIMING CODE, REMOVE LATER
    //println("\t" + (System.currentTimeMillis - now))

    // pick rejuvenation sequence in the reservoir
    rejuvenateAll(wordIds, rejuvBatchSize, currVocabSize)
    uniformReweightAll()
  }

  /** performs rejuvenation MCMC step for every particle */
  def rejuvenateAll (wordIds: Array[(Int,Int)], batchSize: Int,
                     currVocabSize: Int): Unit = {
    particles.foreach {
      p =>
        p.rejuvenate(wordIds, batchSize, currVocabSize) }
  }

  /** Helper method puts the weights of particles into an array, so that
   `particles(i) == weights(i)` */
  def particleWeightArray (): Array[Double] = {
    var weights = Array.fill(numParticles)(0.0)
    for (i <- 0 to numParticles-1) weights(i) = particles(i).weight
    weights
  }

  /** Reweights all particles so that they're uniform */
  def uniformReweightAll (): Unit = {
    particles.foreach { p => p.weight = 1.0 / numParticles }
  }

  def printParticles (): Unit = particles.foreach { p => println(p) }

  /** Generates the id by which the particles are to access the assignment
   store. When you query the assignment store for a specific topic assignment
   (or when you set the assignment), this id tells the store which particle to
   do the lookup in. */
  private def newAssignStoreId (): Int = {
    val newid = currId
    currId += 1
    newid
  }

  /** Creates an array of particles resampled proportional to the weights */
  private def multinomialResample (unnormalizedWeights: Array[Double]):
  Array[Particle] = {
    val weightsCdf = Stats.normalizeAndMakeCdf(unnormalizedWeights)
    val resampledParticles = new Array[Particle](numParticles)
    (0 to numParticles-1).foreach {
      i =>
        val indexOfParticleToCopy = Stats.sampleCategorical(weightsCdf)
        val newIdx = newAssignStoreId()
        resampledParticles(i) =
          particles(indexOfParticleToCopy).copy(newIdx)
    }
    resampledParticles
  }

  override def toString (): String = {
    particles(0).docAssgs(0).mkString(" ")
  }
}

/** Directed tree allowing us to represent and copy particles in a way that is
 * both memory efficient and time efficient. This is especially useful when we
 * resample particles, which requires that we copy all the topic assignments.
 *
 * This is detailed in section 4 of the Canini paper. The idea is, there is
 * high redundancy in the particles. We copy the particles during resampling,
 * but initially all of the assignments will be the same. Thus, we need only
 * create reference to a new particle and change its assignments as necessary.
 * Thus, looking up a particle, we need only recurse up the parents until we
 * find the assignment we're looking for.
 */
class AssignmentStore () {

  var assgMap = new AssignmentMap()
  var parent = HashMap[Int,Int]()          // particleId -> id of parent
  var children = HashMap[Int,List[Int]]()  // particleId -> list of children

  /** Gets a paticle's topic assignment at a specific wordIdx in a document.
   If we do not find wordIdx in docId, then we recurse up until we do. We
   should always find it at the root, and if we don't then something has gone
   wrong. */
  @tailrec
  final def getTopic (particleId: Int, docId: Int, wordIdx: Int): Int = {
    // if word assigned topic in current particle, return.
    // else recurse upwards.
    // if no parent entry for particleId, then error out.
    if (assgMap.wordChangedInParticle(particleId, docId, wordIdx)) {
      return assgMap.getTopic(particleId, docId, wordIdx)
    }
    else {
      return getTopic(parent(particleId), docId, wordIdx)
    }
  }

  /** Checks to see if a particle contains a topic assignment for some word in
   some document */
  def wordChangedInParticle (particleId: Int, docId: Int, wordId: Int):
  Boolean = {
    assgMap.wordChangedInParticle(particleId, docId, wordId)
  }

  /** Sets topic assignment for word at location wordIdx in document docId.
   Additionally, the old value is inserted into the child particles to maintain
   consistency. Note that the old value should be set in the children, unless
   they've already been set, for consistency! Unlike `get` the parent are NOT
   affected. */
  def setTopic (particleId: Int, docId: Int, wordIdx: Int, topic: Int):
  Unit = {
    assgMap.setTopic(particleId, docId, wordIdx, topic)
  }

  /** Creates new topic assignment vector for document */
  def newDocument (particleId: Int, newDocIndex: Int, doc: Array[String],
                   topics: Int, globalVect: GlobalUpdateVector,
                   currDocVect: DocumentUpdateVector):
  Unit =
    assgMap.newDoc(particleId, newDocIndex, doc, topics, globalVect,
                 currDocVect)

  /** Creates new particle.

   If parent node exists, set its children to include current node. Always set
   current node's parents to parentId*/
  def newParticle (particleId: Int, parentId: Int): Unit = {
    assgMap.newParticle(particleId)
    if (parentId != Constants.NoParent) {
      if (children contains parentId)
        children(parentId) = particleId :: children(parentId)
      else
        children(parentId) = List(particleId)
    }
    parent(particleId) = parentId
  }

	// TODO why no-op?!
  /** Deletes or merges nodes that are "inactive." A node is inactive if it is
   no particle has copied it during the resampling step. If an entire subtree
   is inactive, then it can be deleted. If a node is inactive, but has active
   children, then it can be merged with the children.*/
  def prune (): Unit = { }
}

/** Map from a (particle, document, word) -> topic -- that is, a map from a
 word in a particular document of a particular particle. Simple object wrapper
 that abstracts away the fact that this map is a three-layer hash table.

 Normally used by AssignmentStore, which exploits the redundancy of particle
 topic assignments to produce a space-efficient representation of the state
 space of a particular run of LDA. */
class AssignmentMap () {
  //particleId -> docId for reservoir sampler -> word idx -> topic assignments
  var assgMap = HashMap[Int,HashMap[Int,HashMap[Int,Int]]]()

  /** Checks to see if a particle contains a topic assignment for some word in
   some document */
  def wordChangedInParticle (particleId: Int, docId: Int, wordId: Int):
  Boolean = {
    assgMap.contains(particleId) &&
    assgMap(particleId).contains(docId) &&
    assgMap(particleId)(docId).contains(wordId)
  }

  /** Queries particle for topic assignment of a word in document; returns None
   if there is no such word in that document of that particle */
  def getTopic (particleId: Int, docId: Int, wordId: Int): Int = {
    assgMap(particleId)(docId)(wordId)
  }

  /** Sets topic for a word in a document.

   This method is tricky. `particleId` should always exist, because we will
   have a constant number of particles at the outset. `docId` may not exist
   in the assignment map for that particle though b/c documents are only added
   when the child is modified and therefore different from the parent. `wordId`
   will usually not be in the map for similar reasons. This method must add
   both of these things */
  def setTopic (particleId: Int, docId: Int, wordId: Int, topic: Int) = {
    if (!assgMap(particleId).contains(docId))
      assgMap(particleId)(docId) = HashMap[Int,Int](wordId -> topic)
    else
      assgMap(particleId)(docId)(wordId) = topic
  }

  /** Builds new representation of topic assignments */
  def newDoc (particleId: Int, docId: Int, doc: Array[String], topics: Int,
              globalVect: GlobalUpdateVector,
              currDocVect: DocumentUpdateVector): Unit = {
    assgMap(particleId)(docId) = HashMap[Int,Int]()
  }

  def newParticle (particleId: Int): Unit =
    assgMap(particleId) = HashMap[Int,HashMap[Int,Int]]()
}

/** `Particle` is a sample from the space of possible states that a run of LDA
 could possibly be in.

 In our case, the `Particle` stores (1) a set of assignments, (2) a weight for
 this particle, and (3) a document and global update vector. The topic
 assignments in particular represent the "state" or a run of LDA, since they
 basically determine what the run of LDA does next. */
class Particle (val topics: Int, val initialWeight: Double,
                val alpha: Double, val beta: Double,
                val rejuvSeq: ReservoirSampler[Array[String]],
                var assgStore: AssignmentStore, val particleId: Int) {
  /* NOTE: `rejuvSeq` depends on the PfLda class to populate it with the
   documents that it will use for rejuvenation; it DEPENDS ON SIDE-EFFECTS to
   do its job. */
  var globalVect = new GlobalUpdateVector(topics)
  var weight = initialWeight
  var currDocVect = new DocumentUpdateVector(topics)
  var rejuvSeqDocVects = HashMap[Int,DocumentUpdateVector]()
  var docLabels = ArrayBuffer[Int]()  // labels for all the documents
  var rsIdxToLabelsIdx = HashMap[Int,Int]()  // resvr sample -> docLabels idx
  var nextDocId = 0

  /** Generates an unnormalized weight for the particle; returns new wgt. NOTE:
   side-effects on the particle's weight as well! */
  def unnormalizedReweight (word: String, w: Int): Double = {
    val prior = unnormalizedPrior(word, w)
    weight = weight * prior
    weight
  }

  /** "Transitions" particle to next state by sampling topic for `word`,
   which is our new observation. w is the *current* size of the vocabulary;
   returns that topic

   Behind the scenes, this requires two updates: first, we must update the
   global and document-specific update vectors, and then we must update the
   topic assignments if this document happens to be in our reservoir. */
  def transition (idx: Int, words: Array[String], w: Int, docIdx: Int): Int = {
    val word = words(idx)
    val cdf = updatePosterior(word, w)
    val sampledTopic = Stats.sampleCategorical(cdf)
    globalVect.update(word, sampledTopic)
    currDocVect.update(word, sampledTopic)

    if (docIdx != Constants.DidNotAddToSampler) {
      assgStore.setTopic(particleId, docIdx, idx, sampledTopic)
    }

    // set the label of the document
    var mx = 0
    for (t <- 0 until topics) {
      if (currDocVect.timesTopicOccursInDoc(t) > mx)
        mx = t
    }
    docLabels(nextDocId - 1) = mx

    sampledTopic
  }

  def newDocumentUpdate (indexIntoSample: Int, doc: Array[String]): Unit = {
    currDocVect = new DocumentUpdateVector(topics)
    if (indexIntoSample != Constants.DidNotAddToSampler) {
      assgStore.newDocument(particleId, indexIntoSample, doc, topics,
                          globalVect, currDocVect)
      rejuvSeqDocVects(indexIntoSample) = currDocVect
    }

    docLabels += -1
    // this will be used to update counts of the document as we go
    if (indexIntoSample != Constants.DidNotAddToSampler) {
      rsIdxToLabelsIdx(indexIntoSample) = nextDocId
    }

    nextDocId += 1
  }

  /** Rejuvenates particle by MCMC; we currently repeat the update step
   `batchSize` times. w is the *current* size of the vocabulary */
  def rejuvenate (wordIds: Array[(Int,Int)], batchSize: Int, w: Int): Unit = {
    val sample = Stats.sampleWithoutReplacement(wordIds, batchSize)
    for (i <- 0 to batchSize-1)
      sample.foreach{ wordId => resampleRejuvSeqWord(wordId._1, wordId._2, w) }
  }

  /** Resamples a word in the rejuvenation sequence; w is *current* size of
   vocabulary*/
  def resampleRejuvSeqWord (docIdx: Int, wordIdx: Int, w: Int): Unit = {
    val doc = rejuvSeq(docIdx)
    val word = doc(wordIdx)
    val cdf = incrementalPosterior(wordIdx, docIdx, w)
    val sampledTopic = Stats.sampleCategorical(cdf)

    assignNewTopic(docIdx, wordIdx, sampledTopic)
  }

  /** Proper deep copy of the particle */
  def copy (newAssgStoreId: Int): Particle = {
    val copiedParticle = new Particle(topics, initialWeight, alpha, beta,
                                      rejuvSeq, assgStore, newAssgStoreId)
    copiedParticle.globalVect = globalVect.copy
    copiedParticle.weight = weight
    val tmpCurrDocVect = currDocVect
    copiedParticle.currDocVect = currDocVect.copy
    assgStore.newParticle(newAssgStoreId, particleId)
    // copy rejuvSeqDocVects
    rejuvSeqDocVects.foreach { kv =>
      if (kv._2 == tmpCurrDocVect)
        copiedParticle.rejuvSeqDocVects(kv._1) = copiedParticle.currDocVect
      else
        copiedParticle.rejuvSeqDocVects(kv._1) = rejuvSeqDocVects(kv._1).copy()}
    for (e <- docLabels)
      copiedParticle.docLabels += e
    rsIdxToLabelsIdx.foreach { kv =>
      copiedParticle.rsIdxToLabelsIdx(kv._1) = rsIdxToLabelsIdx(kv._1) }
    copiedParticle.rsIdxToLabelsIdx
    copiedParticle.nextDocId = nextDocId
    copiedParticle
  }

  /** Assigns new topics to RESAMPLED words */
  private def assignNewTopic (docIdx: Int, wordIdx: Int, newTopic: Int):
  Unit = {
    val oldTopic = assgStore.getTopic(particleId, docIdx, wordIdx)
    var docUpdateVect = rejuvSeqDocVects(docIdx)
    val doc = rejuvSeq.getSampleSet()(docIdx)
    val word = doc(wordIdx)
    // should use indices to decrement old topic counts?
    globalVect.resampledUpdate(word, oldTopic, newTopic)
    docUpdateVect.resampledUpdate(wordIdx, oldTopic, newTopic)
    assgStore.setTopic(particleId, docIdx, wordIdx, newTopic)

    // set document label if it changes
    val labelId = rsIdxToLabelsIdx(docIdx)
    var mx = 0
    for (t <- 0 until topics) {
      if (docUpdateVect.timesTopicOccursInDoc(t) > mx)
        mx = t
    }
    docLabels(labelId) = mx
  }

  /** Results in a number proportional to P(w_i|z_{i-1}, w_{i-1});
   specifically, we note that this probability is proportional to
   P(w_i|z_{i-1}^{(p)}) P(z_{i-1}^{(p)}|d_i). */
  private def unnormalizedPrior (word: String, w: Int): Double = {
    var prOfWord = 0.0
    (0 to topics-1).foreach { t => prOfWord += updateEqn(word, t, w) }
    prOfWord
  }

  /** Generates the normalized posterior distribution P(z_j|Z_{i-1}, w_i);
   w is the *current* size of the vocabulary */
  private def updatePosterior (word: String, w: Int): Array[Double] = {
    var unnormalizedCdf = Array.fill(topics)(0.0)
    (0 to topics-1).foreach { i =>
      unnormalizedCdf(i) = updateEqn(word, i, w) }
    Stats.normalizeAndMakeCdf(unnormalizedCdf)
  }

  /** Generates normalized incremental update posterior approximation
   distribution P(z_j|Z_{i\j}, w_i); w is *current* size of the vocabulary.
   This normalized distribution is a distribution over all possible z_j in eqn
   3 in Canini et al "Online Inference of Topics ..." */
  private def incrementalPosterior (wordIdx: Int, docId:Int,
                                    w: Int): Array[Double] = {
    var unnormalizedCdf = Array.fill(topics)(0.0)
    (0 to topics-1)foreach { i =>
      unnormalizedCdf(i) = incrementalEqn(wordIdx, docId, i, w) }
    Stats.normalizeAndMakeCdf(unnormalizedCdf)
  }

  /** Applies the o-LDA update equation from "Online Inference of Topics..."
   by Canini, Shi, Griffiths. The relevant equation is eqn (2). w is the
   *current* size of the vocabulary */
  private def updateEqn (word: String, topic: Int, w: Int): Double = {
    val globalUpdate = (globalVect.numTimesWordAssignedTopic(word, topic)
                        + beta) /
    (globalVect.numTimesTopicAssignedTotal(topic) + w * beta)

    val docUpdate = (currDocVect.numTimesTopicOccursInDoc(topic) + alpha) /
    (currDocVect.numWordsInDoc + topics * alpha)
    globalUpdate * docUpdate
  }

  /** For some word and some `topic`, calculates number proportional to
   p(z_j|Z_{i\j}, W_i). this is given as eqn (3) in Canini et al "Online
   Inference of Topics..." */
  private def incrementalEqn (wordIdx: Int, docId: Int, topic: Int,
                              w: Int): Double = {
    // We want to count the number of times a topic has occurred EXCLUDING the
    // current time it was assigned. This method just helps that goal.
    def counterHelper (count: Int, targetTopic: Int): Int = {
      if (targetTopic == topic) Math.max(count - 1, 0)
      else count
    }
    val doc = rejuvSeq.getSampleSet()(docId)
    val word = doc(wordIdx)
    val priorTopic = assgStore.getTopic(particleId, docId, wordIdx)
    val docVect = rejuvSeqDocVects(docId)
    val globalUpdate =
      (counterHelper(globalVect.numTimesWordAssignedTopic(word, topic),
                     priorTopic) + beta) /
    (counterHelper(globalVect.numTimesTopicAssignedTotal(topic),
                   priorTopic) + w * beta)

    val docUpdate =
      (counterHelper(docVect.numTimesTopicOccursInDoc(topic),
                     priorTopic) + alpha) /
    (Math.max(docVect.wordsInDoc - 1, 0) + topics * alpha)
    globalUpdate * docUpdate
  }

  def docAssgs (docIdx: Int): Array[Int] = {
    val assgs = new Array[Int](rejuvSeqDocVects(docIdx).wordsInDoc)
    for (i <- 0 to assgs.size-1) {
      assgs(i) = assgStore.getTopic(particleId, docIdx, i)
    }
    assgs
  }

  /** Note that docId is the index into the reservoir sampler! */
  def docAssgsToArray(docId: Int, length: Int): Array[Int] = {
    var a = new Array[Int](length)
    for (i <- 0 until length)
      a(i) = assgStore.getTopic(particleId, docId, i)
    a
  }

  override def toString(): String = {
    var s = "particle: "
    rejuvSeqDocVects.foreach { kv => s += docAssgs(kv._1).deep.mkString(" ")}
    s
  }
}

/** Tracks update progress for the document-specific ITERATIVE update
 equation of the particle filtered LDA implementation. */
class DocumentUpdateVector (val topics: Int) {
  // in the paper this is called n^{(d_j)}_{z_j, i\j}
  var timesTopicOccursInDoc = Array.fill[Int](topics)(0)
  // in the paper this is called n^{(d_j)}_{., i\j}
  var wordsInDoc = 0

  def numTimesTopicOccursInDoc (topic: Int): Int =
    timesTopicOccursInDoc(topic)

  def numWordsInDoc (): Int = wordsInDoc

  /** Update vector based on observation: word and topic assigned to it */
  def update (word: String, topic: Int): Unit = {
    timesTopicOccursInDoc(topic) += 1
    wordsInDoc += 1
  }

  def resampledUpdate (wordIdx: Int, oldTopic: Int, newTopic: Int): Unit = {
    timesTopicOccursInDoc(oldTopic) -= 1
    timesTopicOccursInDoc(newTopic) += 1
  }

  /** proper deep copy of DocumentUpdateVector */
  def copy (): DocumentUpdateVector = {
    var copiedVect = new DocumentUpdateVector(topics)
    Array.copy(timesTopicOccursInDoc, 0, copiedVect.timesTopicOccursInDoc, 0,
               topics)
    copiedVect.wordsInDoc = wordsInDoc
    copiedVect
  }
}

/** Tracks update progress for the global iterative update equation of the
 particle filtered LDA implementation. */
class GlobalUpdateVector (val topics: Int) {
  // in the paper, this is called n^{(w_j)}_{z_j,i\j}
  var timesWordAssignedTopic = HashMap[(String,Int),Int]()
  // in the paper, this is called n^{(.)}_{z_j, i\j}
  var timesTopicAssignedTotal = Array.fill[Int](topics)(0)

  def numTimesWordAssignedTopic (word: String, topic: Int): Int =
    if (timesWordAssignedTopic contains (word,topic))
      timesWordAssignedTopic((word, topic))
    else 0

  def numTimesTopicAssignedTotal (topic: Int): Int =
    timesTopicAssignedTotal(topic)

  /** Updates vector based on observation: word and topic assigned to it */
  def update (word: String, topic: Int): Unit = {
    if (timesWordAssignedTopic contains (word,topic))
      timesWordAssignedTopic((word, topic)) += 1
    else timesWordAssignedTopic((word,topic)) = 1
    timesTopicAssignedTotal(topic) += 1
  }

  def resampledUpdate (word: String, oldTopic: Int, newTopic: Int): Unit = {
    if (timesWordAssignedTopic((word, oldTopic)) == 1)
      timesWordAssignedTopic.remove((word, oldTopic))
    else timesWordAssignedTopic((word, oldTopic)) -= 1
    if (timesWordAssignedTopic contains (word, newTopic))
      timesWordAssignedTopic((word, newTopic)) += 1
    else
      timesWordAssignedTopic((word, newTopic)) = 1
    timesTopicAssignedTotal(oldTopic) -= 1
    timesTopicAssignedTotal(newTopic) += 1
  }

  /** proper deep copy of GlobalUpdateVector */
  def copy (): GlobalUpdateVector = {
    val copiedVect = new GlobalUpdateVector(topics)
    timesWordAssignedTopic.foreach { kv =>
      copiedVect.timesWordAssignedTopic(kv._1) = kv._2 }
    Array.copy(timesTopicAssignedTotal, 0, copiedVect.timesTopicAssignedTotal,
               0, topics)
    copiedVect
  }
}
