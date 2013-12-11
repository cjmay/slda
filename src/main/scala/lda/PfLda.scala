package lda

import java.io.PrintWriter

import globals.Constants
import stream._
import wrangle._
import evaluation._

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
            val vocab: Set[String],
            val reservoirSize: Int, val numParticles: Int, ess: Double,
            val rejuvBatchSize: Int, val rejuvMcmcSteps: Int) {
  var currTokenNum = -1 // Just used for diagnostics
  var particles: ParticleStore = null
  var rejuvSeq: ReservoirSampler[Particle.DocumentToken] = null

  /** Initialize model with batch MCMC.
    * Must be called before ingestDoc/ingestDocs.
    */
  def initialize(docs: Array[Array[String]], mcmcSteps: Int): Unit = {
    val totalNumTokens = docs.map(_.size).sum
    currTokenNum += totalNumTokens
    rejuvSeq = new ReservoirSampler(totalNumTokens)
    particles = new ParticleStore(T, alpha, beta, vocab.size,
                                  numParticles, ess,
                                  rejuvBatchSize, rejuvMcmcSteps, rejuvSeq)

    particles.initialize(docs, mcmcSteps, reservoirSize)
  }

  /** Ingest one document, update LDA as we go.
    * For each new word, we reweight the particles. Then we sample a
    * topic assignment from the posterior. Then if the 2norm of the
    * weight vector lies below a certain threshold, we resample the
    * topics
    */
  def ingestDoc(words: Array[String], evaluator: DualEvaluator): Int = {
    val docIdx = particles.newDocumentUpdateAll()
    val now = System.currentTimeMillis
    (0 until words.length).foreach { i =>
      processWord(i, words, docIdx, evaluator)
    }
    if (words.length != 0) {
      println("TIMEPERWORD " + ((System.currentTimeMillis - now)/words.length))
      println("NUMWORDS " + words.length)
    }

    println

    docIdx
  }

  def evaluate(evaluator: DualEvaluator): Unit = {
    // NOTE: if we do this immediately after ingestDoc or processWord
    // we will probably just pick a random particle because weights
    // have probably been reset to uniform (as last step in
    // resampling process)
    val p = particles.maxPosteriorParticle

    evaluator.outOfSampleEval(p.globalVect)
  }

  /** Process the ith entry in `words`; copied pretty much verbatim from
    * Algorithm 4 of Canini, et al "Online Inference of Topics..."
    */
  private def processWord(i: Int, words: Array[String], docIdx: Int,
      evaluator: DualEvaluator): Unit = {
    val word = words(i)
    currTokenNum += 1

    particles.unnormalizedReweightAll(word)

    if (i == 0) evaluate(evaluator)

    particles.transitionAll(i, words(i), docIdx)
    particles.normalizeWeights()

    if (particles.shouldResample) {
      println("REJUVENATE " + currTokenNum)
      particles.resampleAndRejuvenate((0 until rejuvSeq.occupied).toArray)
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

  def printTopics: Unit = {
    val particleObjs = particles.particles

    println("PRINT TOPICS")
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
