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
  val testCorpus: Array[String]
  val testLabels: Array[String]
  val cats: List[String]
  val seed: Long
  val fixInitialSample: Boolean = true
  val fixInitialModel: Boolean = true
  val inferMcmcSteps: Int = 2
}

object Sim3PfParams extends RunLdaParams {
  val initialBatchSize = 177 // number of docs for batch MCMC init
  val seed = 43L
  val (corpus, labels, testCorpus, testLabels, cats) = wrangle.TNG.sim3
}

object Rel3PfParams extends RunLdaParams {
  val initialBatchSize = 158 // number of docs for batch MCMC init
  val seed = 23L
  val (corpus, labels, testCorpus, testLabels, cats) = wrangle.TNG.rel3
}

object Diff3PfParams extends RunLdaParams {
  val initialBatchSize = 167 // number of docs for batch MCMC init
  val seed = 21L
  val (corpus, labels, testCorpus, testLabels, cats) = wrangle.TNG.diff3
}

object RunLda {
  def main (args: Array[String]) {
    val params: RunLdaParams = Diff3PfParams

    if (params.fixInitialModel && ! params.fixInitialSample)
      println("warning: fixInitialModel implies fixInitialSample")

    // If we want to fix the random seed for the data shuffle or
    // fix the seed for the Gibbs initialization---which implies
    // a fixed seed for the data shuffle---we do so here
    if (params.fixInitialSample || params.fixInitialModel)
      Stats.setSeed(params.seed)

    println("loading corpus...")
    val docLabelPairs = Stats.shuffle(params.corpus.zip(params.labels).toSeq)
    val (corpus, labels) =
      (docLabelPairs.map(p => p._1).toArray,
       docLabelPairs.map(p => p._2).toArray)

    // If we fixed a random seed for the data shuffle but want a random
    // Gibbs initialization, reinitialize seed randomly
    if (params.fixInitialSample && ! params.fixInitialModel)
      Stats.setDefaultSeed()

    println("initializing model...")
    val model = new PfLda(params.cats.size, params.alpha, params.beta,
                          params.reservoirSize, params.numParticles,
                          params.ess, params.rejuvBatchSize,
                          params.rejuvMcmcSteps)

    model.makeInferentialSampler(params.testCorpus, params.inferMcmcSteps,
      (docLabels: Iterable[Int]) =>
        println(Evaluation.nmi(
          docLabels, params.testLabels,
          params.cats.size, params.cats)))

    val evaluate = {(docLabels: Iterable[Int]) =>
      println(Evaluation.nmi(
        docLabels, labels.take(docLabels.size),
        params.cats.size, params.cats))
      model.infer
    }

    // TODO: what if batch size is bigger than corpus?
    model.initialize(
      (0 to params.initialBatchSize-1).map(corpus(_)).toArray,
      params.initialBatchMcmcSteps,
      evaluate)

    // If we fixed a random seed earlier and haven't reinitialized it
    // yet, reinitialize it randomly now
    if (params.fixInitialModel)
      Stats.setDefaultSeed()

    println("running particle filter...")
    for (i <- params.initialBatchSize to corpus.length-1) {
      println("DOCUMENT " + i + " / " + corpus.length)
      model.ingestDoc(corpus(i), evaluate)
    }
    model.writeTopics("results.txt")
  }
}
