package lda

import evaluation._
import wrangle._

abstract class RunLdaParams {
  val alpha: Double = 0.1
  val beta: Double = 0.1
  val reservoirSize: Int = 1000
  val numParticles: Int = 100
  val ess: Double = 20.0
  val rejuvBatchSize: Int = 30
  val rejuvMcmcSteps: Int = 20
  val initialBatchSize: Int
  val initialBatchMcmcSteps: Int = 150
  val corpus: Array[String]
  val labels: Array[String]
  val cats: List[String]
}

object Sim3PfParams extends RunLdaParams {
  val initialBatchSize = 177 // number of docs for batch MCMC init
  val (corpus, labels, cats) = wrangle.TNG.sim3
}

object Rel3PfParams extends RunLdaParams {
  val initialBatchSize = 158 // number of docs for batch MCMC init
  val (corpus, labels, cats) = wrangle.TNG.rel3
}

object Diff3PfParams extends RunLdaParams {
  val initialBatchSize = 167 // number of docs for batch MCMC init
  val (corpus, labels, cats) = wrangle.TNG.diff3
}

object RunLda {
  def main (args: Array[String]) {
    var argsOffset = 0
    var orderedPfData = true

    Stats.setDefaultSeed()

    while (argsOffset < args.size) {
      if (args(argsOffset) == "-s") {
        Stats.setSeed(args(argsOffset+1).toLong)
        argsOffset += 2
      } else if (args(argsOffset) == "-r") {
        orderedPfData = false
        argsOffset += 1
      } else {
        System.err.println("Unknown argument: " + args(argsOffset))
        argsOffset += 1
      }
    }

    val params: RunLdaParams = Diff3PfParams

    println("loading corpus...")
    val (corpus, labels) = shuffleInit(params.corpus, params.labels,
      if (orderedPfData) params.initialBatchSize else params.corpus.size)

    println("initializing model...")
    val model = new PfLda(params.cats.size, params.alpha, params.beta,
                          params.reservoirSize, params.numParticles,
                          params.ess, params.rejuvBatchSize,
                          params.rejuvMcmcSteps)

    val evaluate = (docIdx: Int) =>
      Evaluation.writeOut(model, labels.slice(0, docIdx+1),
                          params.cats,
                          DataConsts.RESULTS_DIR + docIdx.toString() + ".txt")

    // TODO: batch size: documents...? not tokens?
    // TODO: what if batch size is bigger than corpus?
    model.initialize(
      (0 to params.initialBatchSize-1).map(corpus(_)).toArray,
      params.initialBatchMcmcSteps,
      evaluate)

    Stats.setDefaultSeed()

    println("running particle filter...")
    for (i <- params.initialBatchSize to corpus.length-1) {
      println("DOCUMENT " + i + " / " + corpus.length)
      //val now = System.nanoTime
      model.ingestDoc(corpus(i))
      // TODO: REMOVE HACKY TIMING CODE FOR BENCHMARKING IMPROVEMENTS
      //println(i + " " + (System.nanoTime - now))
      //if (docIdx % 10 == 0)
        evaluate(i)
    }
    model.writeTopics("results.txt")

    val mis = Evaluation.nmi(model, labels, params.cats)
    println(mis.deep)
  }

  private def shuffleInit(corpus: Array[String], labels: Array[String],
      initialBatchSize: Int): (Array[String], Array[String]) = {
    val docLabelPairs = corpus.zip(labels)
    val initDocLabelPairs =
      docLabelPairs.slice(0, initialBatchSize)
    val shuffledInitDocLabelPairs =
      Stats.shuffle(initDocLabelPairs.toSeq).toArray
    val restDocLabelPairs =
      docLabelPairs.slice(initialBatchSize, docLabelPairs.size)
    val partShuffledDocLabelPairs =
      shuffledInitDocLabelPairs ++ restDocLabelPairs
    (partShuffledDocLabelPairs.map(p => p._1),
      partShuffledDocLabelPairs.map(p => p._2))
  }
}
