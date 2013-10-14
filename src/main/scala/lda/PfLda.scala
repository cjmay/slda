package lda

import java.io.PrintWriter
import scala.collection.mutable.{ HashMap => HashMap }
import scala.util.matching.Regex
import scala.util.{ Random => Random }

import globals.Constants
import stream._
import wrangle._

/** Particle filter-based Gibbs sampler for LDA.
 *
 * @param T Number of topics
 * @param alpha Symmetric Dirichlet prior
 * @param beta Symmetric Dirichlet prior
 * @param reservoirSize Number tokens in reservoir
 * @param numParticles Number of particles to maintain
 * @param ess Controls threshold for rejuvenation. Higher = more often.
 * @param rejuvBatchSize number of words to rejuv per rejuvenation step
 * @param rejuvMcmcSteps number of steps to run rejuv MCMC before stopping
 */
class PfLda(val T: Int, val alpha: Double, val beta: Double,
            val reservoirSize: Int, val numParticles: Int, ess: Double,
            val rejuvBatchSize: Int, val rejuvMcmcSteps: Int) {
  val Blacklist = Text.stopWords(DataConsts.TNG_STOP_WORDS)
  var vocabToId = HashMap[String,Int]()
  var currVocabSize = 0
  var currWordIdx = 0
  var particles: ParticleStore = null // TODO ugly
  var rejuvSeq: ReservoirSampler[Particle.DocumentToken] = null // TODO ugly

  private def simpleFilter(str: String): Boolean = {
    val patt = new Regex("\\W");
    (patt.findAllIn(str).size == 0) && !Blacklist(str.toLowerCase)
  }

  def initialize(docs: Array[String], mcmcSteps: Int): Unit = {
    val docsTokens = docs.map(makeBOW(_)).toArray

    val totalNumTokens = docsTokens.map(_.size).sum
    rejuvSeq = new ReservoirSampler(totalNumTokens)
    particles = new ParticleStore(T, alpha, beta, numParticles, ess,
                                  rejuvBatchSize, rejuvMcmcSteps, rejuvSeq)

    particles.initialize(docsTokens, mcmcSteps, currVocabSize, reservoirSize)
  }

  def makeBOW(doc: String) = Text.bow(doc, simpleFilter(_))

  /** Ingest set of documents, updating LDA run as we go */
  def ingestDocs(docs: Array[String]): Unit =
    docs.foreach{ doc => ingestDoc(doc) }

  /** Ingest one document, update LDA as we go.
    * For each new word, we reweight the particles. Then we sample a
    * topic assignment from the posterior. Then if the 2norm of the
    * weight vector lies below a certain threshold, we resample the
    * topics
    */
  def ingestDoc(doc: String): Int = {
    val words = makeBOW(doc)

    val docIdx = newDocumentUpdate()
    val now = System.currentTimeMillis
    (0 to words.length-1).foreach{ i => processWord(i, words, docIdx) }
    if (words.length != 0) {
      println("TIMEPERWORD " + ((System.currentTimeMillis - now)/words.length))
      println("NUMWORDS " + words.length)
    }
    println

    docIdx
  }

  /** Process the ith entry in `words`; copied pretty much verbatim from
    * Algorithm 4 of Canini, et al "Online Inference of Topics..."
    */
  private def processWord(i: Int, words: Array[String], docIdx: Int): Unit = {
    val currword = words(i)
    addWordIfNotSeen(currword) // side-effects; must be before particle updates!
    currWordIdx += 1

    // TODO why reweight before transition?
    particles.unnormalizedReweightAll(currword, currVocabSize)
    particles.transitionAll(i, words(i), currVocabSize, docIdx)
    particles.normalizeWeights()

    if (particles.shouldResample) {
      println("REJUVENATE " + currWordIdx)
      particles.resampleAndRejuvenate((0 to rejuvSeq.occupied-1).toArray,
        currVocabSize)
    }
  }

  /** Inform particle store we will be moving on to a new document,
    * return new document idx
    */
  private def newDocumentUpdate(): Int =
    particles.newDocumentUpdateAll()

  /** Adds `word` to the current vocab map if not seen; uses current
    * currVocabSize as the id, i.e., if `word` is the nth seen so far,
    * then n happens to be == currVocabSize
    */
  private def addWordIfNotSeen(word: String): Unit = {
    if (!(vocabToId contains word)) {
      vocabToId(word) = currVocabSize
      currVocabSize += 1
    }
  }

  def writeTopics(filename: String): Unit = {
    Io.makeDirIfNE(DataConsts.RESULTS_DIR)
    println("WRITE TOPICS")
    val wrdWIdx = vocabToId.toArray[(String,Int)]
    val particleObjs = particles.particles
    val pw = new PrintWriter(DataConsts.RESULTS_DIR + filename)

    for (p <- 0 to particleObjs.length-1) {
      pw.write("PARTICLE " + p + "\n")
      val countVctr = particleObjs(p).globalVect
      for (t <- 0 to T-1) {
        val percs = new Array[(Double,String)](wrdWIdx.size)
        for (i <- 0 to wrdWIdx.size-1) {
          // grab each word, compute how much it comprises a given topic
          val (w,id) = wrdWIdx(i)
          val prctg = countVctr.numTimesWordAssignedTopic(w, t).toDouble /
            countVctr.numTimesTopicAssignedTotal(t)
          percs(i) = (prctg, w);
        }
        pw.write("topic " + t + "\n")
        pw.write("\t" + percs.sorted.reverse.deep.mkString("\n\t") + "\n")
      }
      pw.write("\n")
    }
    pw.close()
  }

  /** print total percentage a word as it occurs in each particular
    * topic
    */
  def printTopics: Unit = {
    val wrdWIdx = vocabToId.toArray[(String,Int)]
    val particleObjs = particles.particles


    for (p <- 0 to particleObjs.length-1) {
      println("PARTICLE " + p)
      val countVctr = particleObjs(p).globalVect
      for (t <- 0 to T-1) {
        val percs = new Array[(Double,String)](wrdWIdx.size)
        for (i <- 0 to wrdWIdx.size-1) {
          // grab each word, compute how much it comprises a given topic
          val (w,id) = wrdWIdx(i)
          val prctg = countVctr.numTimesWordAssignedTopic(w, t).toDouble /
            countVctr.numTimesTopicAssignedTotal(t)
          percs(i) = (prctg, w);
        }
        println("topic " + t)
        println("\t" + percs.sorted.reverse.deep.mkString("\n\t"))
      }
      println
    }
  }
}
