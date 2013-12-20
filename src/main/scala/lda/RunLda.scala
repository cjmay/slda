package lda

import scala.util.matching.Regex

import globals.Constants
import evaluation._
import wrangle._
import stream._
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
  val inferDocsSize: Int = 100
  val topics: Int = 10
  val seed: Long = 0
  val fixInitialSample: Boolean = true
  val fixInitialModel: Boolean = false
  val dataDir: String
  def makeEvaluator(testDocs: Iterator[(Int,Array[String])],
      vocab: Set[String]): Evaluator
}

object GigaParams extends RunLdaParams {
  val dataDir = DataConsts.GIGAWORD_DATA_DIR

  override def makeEvaluator(testDocs: Iterator[(Int,Array[String])],
      vocab: Set[String]): Evaluator = {
    val streamHeadBuffer = new StreamHeadBuffer(testDocs, inferDocsSize)
    var inferentialSampler = new InferentialGibbsSampler(topics, alpha, beta,
      vocab.size)
    new GigawordEvaluator(inferentialSampler, streamHeadBuffer)
  }
}

object TNGParams extends RunLdaParams {
  val dataDir = DataConsts.TNG_DATA_DIR

  override def makeEvaluator(testDocs: Iterator[(Int,Array[String])],
      vocab: Set[String]): Evaluator = {
    var inferentialSampler = new InferentialGibbsSampler(topics, alpha, beta,
      vocab.size)
    new StaticEvaluator(inferentialSampler, testDocs.map(_._2).toArray)
  }
}

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
    val data = new GzipDataset(params.dataDir)

    // If we fixed a random seed for the data shuffle but want a random
    // Gibbs initialization, reinitialize seed randomly
    if (params.fixInitialSample && ! params.fixInitialModel)
      Stats.setDefaultSeed()

    val vocab = data.getVocab
    println("vocab size " + vocab.size)

    val evaluator = params.makeEvaluator(data.testDocs, vocab)

    println("initializing model...")
    val model = new PfLda(params.topics, params.alpha, params.beta,
                          vocab,
                          params.reservoirSize, params.numParticles,
                          params.ess, params.rejuvBatchSize,
                          params.rejuvMcmcSteps)

    val initialBatchSize = params.initialBatchSize
    val trainDocsIter = data.trainDocs
    val initDocPairs = (0 until initialBatchSize).map(i => trainDocsIter.next).toArray
    val initDocs = Stats.shuffle(initDocPairs.map(p => p._2)).toArray
    model.initialize(initDocs, params.initialBatchMcmcSteps)

    // If we fixed a random seed earlier and haven't reinitialized it
    // yet, reinitialize it randomly now
    if (params.fixInitialModel)
      Stats.setDefaultSeed()

    println("running particle filter...")
    evaluator.updateDatasetPos(initDocPairs.last._1)
    var i = initialBatchSize
    while (trainDocsIter.hasNext) {
      val (datasetIdx, doc) = trainDocsIter.next
      println("DOCUMENT " + i)
      model.ingestDoc(doc, evaluator)
      evaluator.updateDatasetPos(datasetIdx)
      i += 1
    }
    model.evaluate(evaluator)
    model.printTopics
  }
}
