package lda

import scala.util.Random

import evaluation._
import wrangle._

trait RunLdaParams {
  val alpha: Double
  val beta: Double
  val reservoirSize: Int
  val numParticles: Int
  val ess: Double
  val rejuvBatchSize: Int
  val rejuvMcmcSteps: Int
  val initialBatchSize: Int
  val initialBatchMcmcSteps: Int
  val labelTypes: List[String]
  val corpus: Array[String]
  val labels: Array[String]
  val cats: Int
}

object Sim3PfParams extends RunLdaParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 1768 // reservoir size in tokens
  val numParticles = 100
  val ess = 20.0 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 177 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 200
  val labelTypes = wrangle.DataConsts.SIM_3_LABELS
  val (corpus, labels, cats) = wrangle.TNG.sim3
}

object Rel3PfParams extends RunLdaParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 1575 // reservoir size in tokens
  val numParticles = 100
  val ess = 20.0 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 158 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 200
  val labelTypes = wrangle.DataConsts.REL_3_LABELS
  val (corpus, labels, cats) = wrangle.TNG.rel3
}

object Diff3PfParams extends RunLdaParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val reservoirSize = 1670 // reservoir size in tokens
  val numParticles = 100
  val ess = 20.0 // effective sample size threshold
  val rejuvBatchSize = 10 // |R(i)|
  val rejuvMcmcSteps = 20
  val initialBatchSize = 167 // number of docs for batch MCMC init
  val initialBatchMcmcSteps = 200
  val labelTypes = wrangle.DataConsts.DIFF_3_LABELS
  val (corpus, labels, cats) = wrangle.TNG.diff3
}

object RunLda {
  def main (args: Array[String]) {
    val params = Diff3PfParams

    println("loading corpus...")
    // if we don't shuffle them, and if we don't shuffle them with the
    // same seed, our NMI suffers greatly
    val corpus = (new Random(10)).shuffle(params.corpus.toSeq).toArray
    val labels = (new Random(10)).shuffle(params.labels.toSeq).toArray

    println("initializing model...")
    val model = new PfLda(params.cats, params.alpha, params.beta,
                          params.reservoirSize, params.numParticles,
                          params.ess, params.rejuvBatchSize,
                          params.rejuvMcmcSteps)

    val evaluate = (docIdx: Int) =>
      Evaluation.writeOut(model, labels.slice(0, docIdx+1),
                          params.labelTypes,
                          DataConsts.RESULTS_DIR +  docIdx.toString() + ".txt")

    // TODO: batch size: documents...? not tokens?
    // TODO: what if batch size is bigger than corpus?
    model.initialize(
      (0 to params.initialBatchSize-1).map(corpus(_)).toArray,
      params.initialBatchMcmcSteps,
      evaluate)

    println("running particle filter...")
    for (i <- params.initialBatchSize to corpus.length-1) {
      println("DOCUMENT " + i + " / " + corpus.length)
      //val now = System.nanoTime
      model.ingestDoc(corpus(i))
      // TODO: REMOVE HACKY TIMING CODE FOR BENCHMARKING IMPROVEMENTS
      //println(i + " " + (System.nanoTime - now))
      if (i % 10 == 0)
        evaluate(i)
    }
    model.writeTopics("results.txt")

    val mis = Evaluation.nmi(model, labels, params.labelTypes)
    println(mis.deep)
  }
}
