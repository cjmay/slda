package lda

import scala.annotation.tailrec
import scala.collection.mutable.{ ArrayBuffer => ArrayBuffer }
import scala.collection.mutable.{ HashMap => HashMap }
import scala.util.{ Random => Random }

import globals.Constants
import stream._

object Particle {
  type DocumentToken = Triple[Int,Int,String]
}

/** A memory- and time-efficient way to represent particles, as detailed
  * in section 4 of Canini Shi Griffiths. Manages all the logic of
  * particle manipulation, copying, reading, etc
  */
class ParticleStore(val T: Int, val alpha: Double, val beta: Double,
                    val numParticles: Int, val ess: Double,
                    val rejuvBatchSize: Int, val rejuvMcmcSteps: Int,
                    var rejuvSeq: ReservoirSampler[Particle.DocumentToken]) {
  var currParticleId = 0 // NOTE: This must come before initParticles,
                         // otherwise it resets the id count
  var currDocIdx = -1
  var (assgStore,particles) = initParticles()

  /** Creates all the particles (requested by `numParticles` parameter),
    * as the corresponding entry in the AssignmentStore that tracks the
    * topic assignments of the words for each particle.
    */
  private def initParticles(): (AssignmentStore, Array[Particle]) = {
    var store = new AssignmentStore()
    var particles = Array.fill[Particle](numParticles)(null)
    for (i <- 0 to numParticles-1) {
      val id = newParticleId()
      store.newParticle(id, Constants.NoParent)
      particles(i) = new Particle(T, 1.0/numParticles, alpha, beta, rejuvSeq,
                                  store, id)
    }
    (store, particles)
  }

  /** Reweights particles proportional to the probability they account
    * for the data. UNNORMALIZED. Does this for every particle.
    * Corresponds to line 4 of Algorithm 4 in Canini paper
    */
  def unnormalizedReweightAll(word: String, currVocabSize: Int): Unit =
    particles.foreach { p =>
      p.unnormalizedReweight(word, currVocabSize)
    }

  /** Normalizes particle weights to sum to one */
  def normalizeWeights(): Unit = {
    var weights = particleWeightArray
    Stats.normalize(weights)
    for (i <- 0 to numParticles-1) particles(i).weight = weights(i)
  }

  /** Transition every particle (see Particle.transition(...)) */
  def transitionAll(wordIdx: Int, word: String, currVocabSize: Int,
                    docIdx: Int): Unit =
    particles.foreach { p =>
      p.transition(wordIdx, word, currVocabSize, docIdx)
    }

  /** Inform each particle we will be moving on to a new document */
  def newDocumentUpdateAll(): Int = {
    currDocIdx += 1
    particles.foreach { p =>
      p.newDocumentUpdate(currDocIdx)
    }
    currDocIdx
  }

  /** Resample particles proportional to their probability */
  def resample(unnormalizedWeights: Array[Double]): Unit =
    particles = multinomialResample(unnormalizedWeights)

  /** Return whether inverse 2-norm of particle weights is within ESS */
  def shouldResample: Boolean = {
    val weights = particleWeightArray
    val statistic = 1/math.pow(Math.norm(weights, 2), 2)
    // NOTE: this is useful for tuning the ESS threshold
    //println(statistic + " " + ess)
    statistic <= ess
  }

  /** Resamples particles and rejuvenates, sampling the rejuvenation
    * sequence from the specified tokens
    */
  def resampleAndRejuvenate(tokenIds: Array[Int], currVocabSize: Int): Unit = {
    // resample particles and prune tree
    resample(particleWeightArray)
    assgStore.prune()

    // pick rejuvenation sequence in the reservoir
    rejuvenateAll(tokenIds, rejuvBatchSize, rejuvMcmcSteps, currVocabSize)
    uniformReweightAll()
  }

  /** Perform rejuvenation MCMC for every particle, taking a different
    * sample from the specified tokens for the rejuvenation sequence
    * for each particle
    */
  def rejuvenateAll(tokenIds: Array[Int], batchSize: Int, mcmcSteps: Int,
                    currVocabSize: Int): Unit =
    particles.foreach { p =>
      val sample = Stats.sampleWithoutReplacement(tokenIds, batchSize)
      p.rejuvenate(sample, mcmcSteps, currVocabSize)
    }

  /** Run batch MCMC on specified docs to initialize model parameters.
    * Computation will be performed in particle zero; at the end,
    * we will clone particle zero to set the model parameters in other
    * particles, perform rejuvenation to create diversity, and otherwise
    * update data structures to prepare for particle filtering.
    */
  def initialize(docs: Array[Array[String]], mcmcSteps: Int,
                 currVocabSize: Int, reservoirSize: Int): Unit = {
    // Add initial tokens to reservoir
    val p = particles(0)
    val allTokenIds = (0 to docs.size-1).map({docIdx =>
      val doc = docs(docIdx)
      val tokenIds = (0 to doc.size-1).map({wordIdx =>
        rejuvSeq.addItem(
          new Particle.DocumentToken(docIdx, wordIdx, doc(wordIdx)))
      }).toArray
      p.newDocumentUpdateInitial(docIdx, tokenIds, doc)
      tokenIds
    }).toArray

    // Do initial batch iterations
    p.rejuvenate(allTokenIds.flatten, mcmcSteps, currVocabSize)

    // Prepare for particle filtering

    // TODO: what if new reservoir size > initial reservoir size?
    // Reset reservoir to non-trivial size
    rejuvSeq.reset(reservoirSize)
    // Re-add documents to reservoir (some will be rejected)
    val rejuvSeqMap: Array[Int] =
      Array.fill(reservoirSize)(Constants.DidNotAddToSampler)
    var removedTokens: List[(Int,Int)] = List.empty
    for (docIdx <- 0 to docs.size-1) {
      val words = docs(docIdx)
      val oldTokenIds = allTokenIds(docIdx)
      for (wordIdx <- 0 to words.size-1) {
        val oldTokenIdx = oldTokenIds(wordIdx)
        val newTokenIdx = rejuvSeq.addItem(
          new Particle.DocumentToken(docIdx, wordIdx, words(wordIdx)))
        if (newTokenIdx == Constants.DidNotAddToSampler)
          removedTokens = (docIdx, wordIdx) +: removedTokens
        else
          rejuvSeqMap(newTokenIdx) = oldTokenIdx
      }
    }
    // Inform particle 0 of new reservoir
    p.remapRejuvSeq(rejuvSeqMap)
    // Clone particle 0 to other particles
    for (particleNum <- 1 to numParticles-1)
      particles(particleNum) = p.copy(newParticleId())
    // Rejuvenate to create particle diversity
    val newTokenIds = (0 to reservoirSize-1).toArray
    rejuvenateAll(newTokenIds, rejuvBatchSize, rejuvMcmcSteps, currVocabSize)
    uniformReweightAll()
    // Remove elements from assignment store that were removed from
    // reservoir
    for ((docIdx, wordIdx) <- removedTokens)
      assgStore.remove(docIdx, wordIdx)
  }

  /** Helper method puts the weights of particles into an array, so that
   `particles(i) == weights(i)` */
  def particleWeightArray: Array[Double] = {
    var weights = Array.fill(numParticles)(0.0)
    for (i <- 0 to numParticles-1) weights(i) = particles(i).weight
    weights
  }

  /** Reweights all particles so that they're uniform */
  def uniformReweightAll(): Unit =
    particles.foreach { p => p.weight = 1.0 / numParticles }

  def printParticles: Unit = particles.foreach { p => println(p) }

  /** Generates the id by which the particles are to access the assignment
   store. When you query the assignment store for a specific topic assignment
   (or when you set the assignment), this id tells the store which particle to
   do the lookup in. */
  private def newParticleId(): Int = {
    val newid = currParticleId
    currParticleId += 1
    newid
  }

  /** Creates an array of particles resampled proportional to the weights */
  private def multinomialResample(unnormalizedWeights: Array[Double]):
  Array[Particle] = {
    val weightsCdf = Stats.normalizeAndMakeCdf(unnormalizedWeights)
    val resampledParticles = new Array[Particle](numParticles)
    (0 to numParticles-1).foreach {
      i =>
        val indexOfParticleToCopy = Stats.sampleCategorical(weightsCdf)
        resampledParticles(i) =
          particles(indexOfParticleToCopy).copy(newParticleId())
    }
    resampledParticles
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
class AssignmentStore {
  var assgMap = new AssignmentMap()
  var parent = HashMap[Int,Int]()          // particleId -> id of parent
  var children = HashMap[Int,List[Int]]()  // particleId -> list of children

  /** Gets a paticle's topic assignment at a specific wordIdx in a document.
   If we do not find wordIdx in docId, then we recurse up until we do. We
   should always find it at the root, and if we don't then something has gone
   wrong. */
  @tailrec
  final def getTopic(particleId: Int, docId: Int, wordIdx: Int): Int = {
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

  def clearParticle(particleId: Int) =
    assgMap.clearParticle(particleId)

  /** Checks to see if a particle contains a topic assignment for some word in
   some document */
  def wordChangedInParticle(particleId: Int, docId: Int, wordId: Int):
  Boolean = {
    assgMap.wordChangedInParticle(particleId, docId, wordId)
  }

  /** Sets topic assignment for word at location wordIdx in document docId.
   Additionally, the old value is inserted into the child particles to maintain
   consistency. Note that the old value should be set in the children, unless
   they've already been set, for consistency! Unlike `get` the parent are NOT
   affected. */
  // TODO  tailrec?!
  def resampledSetTopic(particleId: Int, docId: Int, wordIdx: Int, topic: Int):
  Unit = {
    val oldTopic = getTopic(particleId, docId, wordIdx)
    if (oldTopic != topic) {
      if (children contains particleId) {
        for (childId <- children(particleId)) {
          if (! wordChangedInParticle(childId, docId, wordIdx))
            resampledSetTopic(childId, docId, wordIdx, oldTopic)
        }
      }
      assgMap.setTopic(particleId, docId, wordIdx, topic)
    }
  }

  def setTopic(particleId: Int, docId: Int, wordIdx: Int, topic: Int):
  Unit = {
    assgMap.setTopic(particleId, docId, wordIdx, topic)
  }

  def remove(docIdx: Int, wordIdx: Int) =
    assgMap.remove(docIdx, wordIdx)

  /** Creates new topic assignment vector for document */
  def newDocument(particleId: Int, newDocIdx: Int): Unit =
    assgMap.newDoc(particleId, newDocIdx)

  /** Creates new particle.

   If parent node exists, set its children to include current node. Always set
   current node's parents to parentId*/
  def newParticle(particleId: Int, parentId: Int): Unit = {
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
  def prune(): Unit = { }
}

/** Map from a (particle, document, word) -> topic -- that is, a map from a
 word in a particular document of a particular particle. Simple object wrapper
 that abstracts away the fact that this map is a three-layer hash table.

 Normally used by AssignmentStore, which exploits the redundancy of particle
 topic assignments to produce a space-efficient representation of the state
 space of a particular run of LDA. */
class AssignmentMap {
  //particleId -> docId for reservoir sampler -> word idx -> topic assignments
  // TODO turn into list of pairs?
  var assgMap = HashMap[Int,HashMap[Int,HashMap[Int,Int]]]()

  /** Checks to see if a particle contains a topic assignment for some
    * word in some document
    */
  def wordChangedInParticle(particleId: Int, docId: Int, wordId: Int):
  Boolean = {
    assgMap.contains(particleId) &&
    assgMap(particleId).contains(docId) &&
    assgMap(particleId)(docId).contains(wordId)
  }

  def clearParticle(particleId: Int) =
    assgMap(particleId) = HashMap[Int,HashMap[Int,Int]]()

  /** Queries particle for topic assignment of a word in document;
    * returns None if there is no such word in that document of that
    * particle
    */
  def getTopic(particleId: Int, docId: Int, wordId: Int): Int = {
    assgMap(particleId)(docId)(wordId)
  }

  /** Sets topic for a word in a document.

   This method is tricky. `particleId` should always exist, because we will
   have a constant number of particles at the outset. `docId` may not exist
   in the assignment map for that particle though b/c documents are only added
   when the child is modified and therefore different from the parent. `wordId`
   will usually not be in the map for similar reasons. This method must add
   both of these things */
  def setTopic(particleId: Int, docId: Int, wordId: Int, topic: Int) = {
    if (!assgMap(particleId).contains(docId))
      assgMap(particleId)(docId) = HashMap[Int,Int](wordId -> topic)
    else
      assgMap(particleId)(docId)(wordId) = topic
  }

  def remove(docIdx: Int, wordIdx: Int) =
    for (particleMap <- assgMap.values) {
      val docMap = particleMap(docIdx)
      docMap -= wordIdx
      if (docMap.isEmpty)
        particleMap -= docIdx
    }

  /** Builds new representation of topic assignments */
  def newDoc(particleId: Int, docIdx: Int): Unit =
    assgMap(particleId)(docIdx) = HashMap[Int,Int]()

  def newParticle(particleId: Int): Unit =
    assgMap(particleId) = HashMap[Int,HashMap[Int,Int]]()
}

/** `Particle` is a sample from the space of possible states that a run of LDA
 could possibly be in.

 In our case, the `Particle` stores (1) a set of assignments, (2) a weight for
 this particle, and (3) a document and global update vector. The topic
 assignments in particular represent the "state" or a run of LDA, since they
 basically determine what the run of LDA does next. */
class Particle(val topics: Int, val initialWeight: Double,
               val alpha: Double, val beta: Double,
               val rejuvSeq: ReservoirSampler[Particle.DocumentToken],
               var assgStore: AssignmentStore, val particleId: Int) {
  /* NOTE: `rejuvSeq` depends on the PfLda class to populate it with the
   documents that it will use for rejuvenation; it DEPENDS ON SIDE-EFFECTS to
   do its job. */
  var globalVect = new GlobalUpdateVector(topics)
  var weight = initialWeight
  var currDocVect = new DocumentUpdateVector(topics)
  var rejuvSeqDocVects: Array[DocumentUpdateVector] =
    Array.fill(rejuvSeq.capacity)(null)
  var docLabels: ArrayBuffer[Int] = ArrayBuffer.empty // TODO

  /** Update data structures for a new rejuvenation sequence that is
    * a subset of the current one, given a mapping from new
    * rejuvenation sequence positions to old rejuvenation sequence
    * positions.
    */
  def remapRejuvSeq(rejuvSeqMap: Array[Int]): Unit =
    rejuvSeqDocVects = rejuvSeqMap.map(rejuvSeqDocVects(_)).toArray

  /** Create pointers and data structures for new document and
    * initialize all topic assignments to random.
    */
  def newDocumentUpdateInitial(docIdx: Int, tokenIds: Array[Int],
                               doc: Array[String]): Unit = {
    val docVect = new DocumentUpdateVector(topics)
    for (tokenIdx <- tokenIds)
      rejuvSeqDocVects(tokenIdx) = docVect
    assgStore.newDocument(particleId, docIdx)

    // Set random initial topic assignments
    for (wordIdx <- 0 to doc.size-1) {
      val word = doc(wordIdx)
      val sampledTopic = Stats.sampleInt(topics)
      globalVect.update(word, sampledTopic)
      docVect.update(sampledTopic)
      assgStore.setTopic(particleId, docIdx, wordIdx, sampledTopic)
    }
  }

  /** Generates an unnormalized weight for the particle; returns new
    * wgt. NOTE: side-effects on the particle's weight as well!
    */
  def unnormalizedReweight(word: String, currVocabSize: Int): Double = {
    // TODO how/why is this correct?
    val prior = unnormalizedPrior(word, currVocabSize)
    weight = weight * prior
    weight
  }

  /** "Transitions" particle to next state by sampling topic for `word`,
    * which is our new observation.  Returns that topic.
    * Behind the scenes, this requires two updates: first, we must
    * update the global and document-specific update vectors, and then
    * we must update the topic assignments if this document happens to
    * be in our reservoir.
    */
  def transition(wordIdx: Int, word: String, currVocabSize: Int,
                 docIdx: Int): Int = {
    val tokenIdx = rejuvSeq.addItem(
      new Particle.DocumentToken(docIdx, wordIdx, word))

    if (tokenIdx != Constants.DidNotAddToSampler) {
      assgStore.remove(docIdx, wordIdx)
      rejuvSeqDocVects(tokenIdx) = currDocVect
    }

    val cdf = updatePosterior(word, currVocabSize)
    val sampledTopic = Stats.sampleCategorical(cdf)
    globalVect.update(word, sampledTopic)
    currDocVect.update(sampledTopic)

    if (tokenIdx != Constants.DidNotAddToSampler)
      assgStore.setTopic(particleId, docIdx, wordIdx, sampledTopic)

    sampledTopic
  }

  /** Create pointers and data structures for new document */
  def newDocumentUpdate(docIdx: Int): Unit = {
    currDocVect = new DocumentUpdateVector(topics)
    assgStore.newDocument(particleId, docIdx)
  }

  /** Rejuvenate particle by MCMC, using specified tokens as
    * rejuvenation sequence and iterating for mcmcSteps.
    */
  def rejuvenate(tokenIds: Array[Int], mcmcSteps: Int,
                 currVocabSize: Int): Unit = {
    for (i <- 1 to mcmcSteps)
      tokenIds.foreach{ tokenIdx =>
        resampleRejuvSeqWord(tokenIdx, currVocabSize)
      }
  }

  /** Resample a word in the rejuvenation sequence */
  def resampleRejuvSeqWord(tokenIdx: Int, currVocabSize: Int): Unit = {
    val (docIdx, wordIdx, word) = rejuvSeq(tokenIdx)
    val cdf = incrementalPosterior(tokenIdx, currVocabSize)
    val sampledTopic = Stats.sampleCategorical(cdf)

    assignNewTopic(tokenIdx, sampledTopic)
  }

  /** Proper deep copy of the particle */
  def copy(newParticleId: Int): Particle = {
    val copiedParticle = new Particle(topics, initialWeight, alpha, beta,
                                      rejuvSeq, assgStore, newParticleId)
    copiedParticle.globalVect = globalVect.copy
    copiedParticle.weight = weight
    val tmpCurrDocVect = currDocVect
    copiedParticle.currDocVect = currDocVect.copy
    assgStore.newParticle(newParticleId, particleId)
    // copy rejuvSeqDocVects
    for (tokenIdx <- 0 to rejuvSeqDocVects.size-1) {
      copiedParticle.rejuvSeqDocVects(tokenIdx) =
        if (rejuvSeqDocVects(tokenIdx) == tmpCurrDocVect)
          copiedParticle.currDocVect
        else
          rejuvSeqDocVects(tokenIdx).copy
    }
    copiedParticle
  }

  /** Assign new topic to token in rejuvenation sequence */
  private def assignNewTopic(tokenIdx: Int, newTopic: Int) = {
    val (docIdx, wordIdx, word) = rejuvSeq(tokenIdx)
    val oldTopic = assgStore.getTopic(particleId, docIdx, wordIdx)
    val docVect = rejuvSeqDocVects(tokenIdx)
    globalVect.resampledUpdate(word, oldTopic, newTopic)
    docVect.resampledUpdate(oldTopic, newTopic)
    assgStore.resampledSetTopic(particleId, docIdx, wordIdx, newTopic)
  }

  /** Results in a number proportional to P(w_i|z_{i-1}, w_{i-1});
   specifically, we note that this probability is proportional to
   P(w_i|z_{i-1}^{(p)}) P(z_{i-1}^{(p)}|d_i). */
  private def unnormalizedPrior(word: String, currVocabSize: Int): Double = {
    var prOfWord = 0.0
    (0 to topics-1).foreach { t => prOfWord += updateEqn(word, t, currVocabSize) }
    prOfWord
  }

  /** Generates the normalized posterior distribution
    * P(z_j|Z_{i-1}, w_i);
    */
  private def updatePosterior(word: String,
                              currVocabSize: Int): Array[Double] = {
    var unnormalizedCdf = Array.fill(topics)(0.0)
    (0 to topics-1).foreach { i =>
      unnormalizedCdf(i) = updateEqn(word, i, currVocabSize) }
    Stats.normalizeAndMakeCdf(unnormalizedCdf)
  }

  /** Generates normalized incremental update posterior approximation
    * distribution P(z_j|Z_{i\j}, w_i).  This normalized distribution is
    * a distribution over all possible z_j in eqn 3 in Canini et al,
    * "Online Inference of Topics ..."
    */
  private def incrementalPosterior(tokenIdx: Int,
                                   currVocabSize: Int): Array[Double] = {
    var unnormalizedCdf = Array.fill(topics)(0.0)
    (0 to topics-1)foreach { i =>
      unnormalizedCdf(i) = incrementalEqn(tokenIdx, i, currVocabSize) }
    Stats.normalizeAndMakeCdf(unnormalizedCdf)
  }

  /** Applies the o-LDA update equation from
    * "Online Inference of Topics..." by Canini, Shi, Griffiths.
    * The relevant equation is eqn (2).
    */
  private def updateEqn(word: String, topic: Int,
                        currVocabSize: Int): Double = {
    val globalUpdate = (globalVect.numTimesWordAssignedTopic(word, topic)
                        + beta) /
    (globalVect.numTimesTopicAssignedTotal(topic) + currVocabSize * beta)

    val docUpdate = (currDocVect.numTimesTopicOccursInDoc(topic) + alpha) /
    (currDocVect.numWordsInDoc + topics * alpha)
    globalUpdate * docUpdate
  }

  /** For some word and some `topic`, calculates number proportional to
    * p(z_j|Z_{i\j}, W_i). this is given as eqn (3) in Canini et al,
    * "Online Inference of Topics..."
    */
  private def incrementalEqn(tokenIdx: Int, topic: Int,
                             currVocabSize: Int): Double = {
    /** Return the count, less one if priorTopic is the same as topic
      * (but never return less than zero).
      */
    def counterHelper(count: Int, priorTopic: Int): Int =
      if (priorTopic == topic) Math.max(count - 1, 0)
      else count

    val (docIdx, wordIdx, word) = rejuvSeq(tokenIdx)
    val priorTopic = assgStore.getTopic(particleId, docIdx, wordIdx)
    val docVect = rejuvSeqDocVects(tokenIdx)

    val globalUpdate =
      (counterHelper(globalVect.numTimesWordAssignedTopic(word, topic),
                     priorTopic) + beta) /
      (counterHelper(globalVect.numTimesTopicAssignedTotal(topic),
                     priorTopic) + currVocabSize * beta)
    val docUpdate =
      (counterHelper(docVect.numTimesTopicOccursInDoc(topic),
                     priorTopic) + alpha) /
      (Math.max(docVect.wordsInDoc - 1, 0) + topics * alpha)

    globalUpdate * docUpdate
  }
}

/** Tracks update progress for the document-specific ITERATIVE update
  * equation of the particle filtered LDA implementation.
  */
class DocumentUpdateVector(val topics: Int) {
  // in the paper this is called n^{(d_j)}_{z_j, i\j}
  var timesTopicOccursInDoc = Array.fill[Int](topics)(0)
  // in the paper this is called n^{(d_j)}_{., i\j}
  var wordsInDoc = 0

  def numTimesTopicOccursInDoc(topic: Int): Int =
    timesTopicOccursInDoc(topic)

  def numWordsInDoc: Int = wordsInDoc

  /** Update vector based on observation: word and topic assigned to it */
  def update(topic: Int): Unit = {
    timesTopicOccursInDoc(topic) += 1
    wordsInDoc += 1
  }

  def resampledUpdate(oldTopic: Int, newTopic: Int): Unit = {
    timesTopicOccursInDoc(oldTopic) -= 1
    timesTopicOccursInDoc(newTopic) += 1
  }

  /** proper deep copy of DocumentUpdateVector */
  def copy: DocumentUpdateVector = {
    var copiedVect = new DocumentUpdateVector(topics)
    Array.copy(timesTopicOccursInDoc, 0, copiedVect.timesTopicOccursInDoc, 0,
               topics)
    copiedVect.wordsInDoc = wordsInDoc
    copiedVect
  }
}

/** Tracks update progress for the global iterative update equation of the
 particle filtered LDA implementation. */
class GlobalUpdateVector(val topics: Int) {
  // in the paper, this is called n^{(w_j)}_{z_j,i\j}
  var timesWordAssignedTopic = HashMap[(String,Int),Int]()
  // in the paper, this is called n^{(.)}_{z_j, i\j}
  var timesTopicAssignedTotal = Array.fill[Int](topics)(0)

  def numTimesWordAssignedTopic(word: String, topic: Int): Int =
    if (timesWordAssignedTopic contains (word,topic))
      timesWordAssignedTopic((word, topic))
    else 0

  def numTimesTopicAssignedTotal(topic: Int): Int =
    timesTopicAssignedTotal(topic)

  /** Updates vector based on observation: word and topic assigned to it */
  def update(word: String, topic: Int): Unit = {
    if (timesWordAssignedTopic contains (word,topic))
      timesWordAssignedTopic((word, topic)) += 1
    else timesWordAssignedTopic((word,topic)) = 1
    timesTopicAssignedTotal(topic) += 1
  }

  def resampledUpdate(word: String, oldTopic: Int, newTopic: Int): Unit = {
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
  def copy: GlobalUpdateVector = {
    val copiedVect = new GlobalUpdateVector(topics)
    timesWordAssignedTopic.foreach { kv =>
      copiedVect.timesWordAssignedTopic(kv._1) = kv._2 }
    Array.copy(timesTopicAssignedTotal, 0, copiedVect.timesTopicAssignedTotal,
               0, topics)
    copiedVect
  }
}
