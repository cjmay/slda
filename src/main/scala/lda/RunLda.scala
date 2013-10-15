package lda

import scala.util.Random

import evaluation._
import wrangle._

object Sim3PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 1768 // reservoir size in tokens
  val numParticles = 100
  val ess = 20 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 177 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 2000
}

object Rel3PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 1575 // reservoir size in tokens
  val numParticles = 100
  val ess = 20 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 158 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 2000
}

object Diff3PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 1670 // reservoir size in tokens
  val numParticles = 100
  val ess = 10 // effective sample size threshold
  val rejuvBatchSize = 10 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 167 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 2000
}

object Subset20PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 0 // reservoir size in tokens
  val numParticles = 100
  val ess = 10 // effective sample size threshold
  val rejuvBatchSize = 10 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 0 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 2000
}

object Slash6PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 0 // reservoir size in tokens
  val numParticles = 100
  val ess = 10 // effective sample size threshold
  val rejuvBatchSize = 10 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 0 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 2000
}

object Slash7PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 0 // reservoir size in tokens
  val numParticles = 100
  val ess = 20 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 0 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 2000
}

object RunLda {
  def main (args: Array[String]) {
    println("loading corpus...")
    var (corpus, labels, cats) = wrangle.TNG.sim3
    // if we don't shuffle them, and if we don't shuffle them with the
    // same seed, our NMI suffers greatly
    corpus = (new Random(10)).shuffle(corpus.toSeq).toArray
    labels = (new Random(10)).shuffle(labels.toSeq).toArray
    println("building model...")
    val model = new PfLda(cats, Sim3PfParams.alpha, Sim3PfParams.beta,
                          Sim3PfParams.reservoirSize, Sim3PfParams.numParticles,
                          Sim3PfParams.ess, Sim3PfParams.rejuvBatchSize,
                          Sim3PfParams.rejuvMcmcSteps)

    // TODO: batch size: documents...? not tokens?
    // TODO: what if batch size is bigger than corpus?
    println("initializing...")
    model.initialize(
      (0 to Sim3PfParams.initialBatchSize-1).map(corpus(_)).toArray,
      Sim3PfParams.initialBatchMcmcSteps)

    println("running particle filter...")
    for (i <- Sim3PfParams.initialBatchSize to corpus.length-1) {
      println("DOCUMENT " + i + " / " + corpus.length)
      //val now = System.nanoTime
      model.ingestDoc(corpus(i))
      // TODO: REMOVE HACKY TIMING CODE FOR BENCHMARKING IMPROVEMENTS
      //println(i + " " + (System.nanoTime - now))
      if (i % 100 == 0) {
        Evaluation.writeOut(model, labels.slice(0, i),
                            DataConsts.SIM_3_LABELS.slice(0, i),
                            DataConsts.RESULTS_DIR +  i.toString() + ".txt")
      }
    }
    model.writeTopics("results.txt")

    val mis = Evaluation.nmi(model, labels, wrangle.DataConsts.SIM_3_LABELS)
    println(mis.deep)
  }
}
