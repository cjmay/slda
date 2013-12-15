package lda

import scala.util.matching.Regex

import globals.Constants
import evaluation._
import wrangle._
import scala.util.matching.Regex

abstract class RunLdaParams {
  val alpha: Double = 0.1
  val beta: Double = 0.1
  val reservoirSize: Int = 10000
  val numParticles: Int = 5
  val ess: Double = 1.0
  val rejuvBatchSize: Int = 60
  val rejuvMcmcSteps: Int = 40
  val initialBatchSize: Int = 100
  val initialBatchMcmcSteps: Int = 150
  val topics: Int = 10
  val seed: Long = 0
  val fixInitialSample: Boolean = true
  val fixInitialModel: Boolean = false
}

object GigaParams extends RunLdaParams { }

object RunLda {
  def main (args: Array[String]) {
    val params: RunLdaParams = GigaParams

    if (params.fixInitialModel && ! params.fixInitialSample)
      println("warning: fixInitialModel implies fixInitialSample")

    // If we want to fix the random seed for the data shuffle or
    // fix the seed for the Gibbs initialization---which implies
    // a fixed seed for the data shuffle---we do so here
    if (params.fixInitialSample || params.fixInitialModel)
      Stats.setSeed(params.seed)

    println("loading corpus...")
    val data = new GigawordWrangler()

    // If we fixed a random seed for the data shuffle but want a random
    // Gibbs initialization, reinitialize seed randomly
    if (params.fixInitialSample && ! params.fixInitialModel)
      Stats.setDefaultSeed()

    val vocab = data.getVocab
    println("vocab size " + vocab.size)

    val inferDocs = data.testDocsIterable
    var inferentialSampler = new InferentialGibbsSampler(params.topics,
      params.alpha, params.beta, vocab.size, inferDocs)
    val evaluator = new DualEvaluator(inferentialSampler)

    println("initializing model...")
    val model = new PfLda(params.topics, params.alpha, params.beta,
                          vocab,
                          params.reservoirSize, params.numParticles,
                          params.ess, params.rejuvBatchSize,
                          params.rejuvMcmcSteps)

    val initialBatchSize = params.initialBatchSize
    val trainDocsIter = data.trainDocs
    val initDocs = Stats.shuffle(
      (0 until initialBatchSize).map(i => trainDocsIter.next)).toArray
    model.initialize(initDocs, params.initialBatchMcmcSteps)

    // If we fixed a random seed earlier and haven't reinitialized it
    // yet, reinitialize it randomly now
    if (params.fixInitialModel)
      Stats.setDefaultSeed()

    println("running particle filter...")
    var i = initialBatchSize
    while (trainDocsIter.hasNext) {
      val doc = trainDocsIter.next
      println("DOCUMENT " + i)
      model.ingestDoc(doc, evaluator)
      i += 1
    }
    model.evaluate(evaluator)
    model.printTopics
  }
}
