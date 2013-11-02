package lda

import java.io.PrintWriter
import scala.collection.mutable.HashSet
import scala.util.matching.Regex

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
  var vocab: HashSet[String] = HashSet.empty
  var currTokenNum = -1 // Just used for diagnostics
  var particles: ParticleStore = null
  var rejuvSeq: ReservoirSampler[Particle.DocumentToken] = null
  var inferentialSampler: InferentialGibbsSampler = null

  /** Return true iff string is nonempty and not in Blacklist */
  private def simpleFilter(str: String): Boolean = {
    val patt = new Regex("\\W");
    (patt.findAllIn(str).size == 0) && !Blacklist(str.toLowerCase)
  }

  /** Initialize model without providing any test documents 
    * (convenience interface)
    */
  def initialize(docs: Array[String], mcmcSteps: Int,
      evaluate: (Iterable[Int]) => Unit): Unit =
    initialize(Array.empty, 0, (docLabels: Iterable[Int]) => {})

  /** Initialize model with batch MCMC, providing test documents
    * for evaluation.  Must be called before ingestDoc/ingestDocs.
    */
  def initialize(docs: Array[String], mcmcSteps: Int,
      evaluate: (Iterable[Int]) => Unit,
      inferDocs: Array[String], inferMcmcSteps: Int,
      inferEvaluate: (Iterable[Int]) => Unit): Unit = {
    val docsTokens = docs.map(makeBOW(_)).toArray
    vocab ++= docsTokens.flatten.toStream

    val totalNumTokens = docsTokens.map(_.size).sum
    currTokenNum += totalNumTokens
    rejuvSeq = new ReservoirSampler(totalNumTokens)
    particles = new ParticleStore(T, alpha, beta, numParticles, ess,
                                  rejuvBatchSize, rejuvMcmcSteps, rejuvSeq)

    val inferDocsTokens = inferDocs.map(makeBOW(_)).toArray
    inferentialSampler = new InferentialGibbsSampler(T, alpha, beta,
      inferMcmcSteps, inferDocsTokens, inferEvaluate)

    particles.initialize(docsTokens, mcmcSteps, vocab.size, reservoirSize,
      evaluate)
  }

  /** Infer topic assignments on test documents and evaluate against
    * gold standard.
    */
  def infer: Unit =
    inferentialSampler.infer(
      particles.maxPosteriorParticle.getGlobalVect,
      vocab.size)

  /** Tokenize doc and remove stop words */
  def makeBOW(doc: String): Array[String] = Text.bow(doc, simpleFilter(_))

  /** Ingest one document, update LDA as we go.
    * For each new word, we reweight the particles. Then we sample a
    * topic assignment from the posterior. Then if the 2norm of the
    * weight vector lies below a certain threshold, we resample the
    * topics
    */
  def ingestDoc(doc: String, evaluate: (Iterable[Int]) => Unit): Int = {
    val words = makeBOW(doc)

    val docIdx = particles.newDocumentUpdateAll()
    val now = System.currentTimeMillis
    (0 to words.length-1).foreach{ i => processWord(i, words, docIdx) }
    if (words.length != 0) {
      println("TIMEPERWORD " + ((System.currentTimeMillis - now)/words.length))
      println("NUMWORDS " + words.length)
      particles.eval(evaluate)
    }
    println

    docIdx
  }

  /** Process the ith entry in `words`; copied pretty much verbatim from
    * Algorithm 4 of Canini, et al "Online Inference of Topics..."
    */
  private def processWord(i: Int, words: Array[String], docIdx: Int): Unit = {
    val word = words(i)
    vocab += word
    currTokenNum += 1

    // TODO why reweight before transition?
    particles.unnormalizedReweightAll(word, vocab.size)
    particles.transitionAll(i, words(i), vocab.size, docIdx)
    particles.normalizeWeights()

    if (particles.shouldResample) {
      println("REJUVENATE " + currTokenNum)
      particles.resampleAndRejuvenate((0 to rejuvSeq.occupied-1).toArray,
        vocab.size)
    }
  }

  def writeTopics(filename: String): Unit = {
    Io.makeDirIfNE(DataConsts.RESULTS_DIR)
    println("WRITE TOPICS")
    val particleObjs = particles.particles
    val pw = new PrintWriter(DataConsts.RESULTS_DIR + filename)

    for (p <- 0 to particleObjs.length-1) {
      pw.write("PARTICLE " + p + "\n")
      val countVctr = particleObjs(p).globalVect
      for (t <- 0 to T-1) {
        val percs: Array[(Double,String)] = vocab.toArray.map({ w =>
          // grab each word, compute how much it comprises a given topic
          val prctg = countVctr.numTimesWordAssignedTopic(w, t).toDouble /
            countVctr.numTimesTopicAssignedTotal(t)
          (prctg, w)
        })
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
    val particleObjs = particles.particles

    for (p <- 0 to particleObjs.length-1) {
      println("PARTICLE " + p)
      val countVctr = particleObjs(p).globalVect
      for (t <- 0 to T-1) {
        val percs: Array[(Double,String)] = vocab.toArray.map({ w =>
          // grab each word, compute how much it comprises a given topic
          val prctg = countVctr.numTimesWordAssignedTopic(w, t).toDouble /
            countVctr.numTimesTopicAssignedTotal(t)
          (prctg, w)
        })
        println("topic " + t)
        println("\t" + percs.sorted.reverse.deep.mkString("\n\t"))
      }
      println
    }
  }
}
