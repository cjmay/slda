package lda

import evaluation._
import wrangle._

object Sim3PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val smplSize = 1768 // number of documents (total: 1768)
  val numParticles = 100
  val ess = 20 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
}

object Rel3PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val smplSize = 1575 // number of documents (total: 1575)
  val numParticles = 100
  val ess = 20 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
}

object Diff3PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val smplSize = 1670 // number of documents (total: 1670)
  val numParticles = 100
  val ess = 10 // effective sample size threshold
  val rejuvBatchSize = 10 // |R(i)|
  val rejuvMcmcSteps = 20
}

object Subset20PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val smplSize = 0 // number of documents
  val numParticles = 100
  val ess = 10 // effective sample size threshold
  val rejuvBatchSize = 10 // |R(i)|
  val rejuvMcmcSteps = 20
}

object Slash6PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val smplSize = 0 // number of documents
  val numParticles = 100
  val ess = 10 // effective sample size threshold
  val rejuvBatchSize = 10 // |R(i)|
  val rejuvMcmcSteps = 20
}

object Slash7PfParams {
  val alpha = 0.1 // topic distribution prior
  val beta = 0.1 // word distribution prior
  val smplSize = 0 // number of documents
  val numParticles = 100
  val ess = 20 // effective sample size threshold
  val rejuvBatchSize = 30 // |R(i)|
  val rejuvMcmcSteps = 20
}

object RunLda {
  def main (args: Array[String]) {
    println("loading corpus...")
    val (corpus, labels, cats) = wrangle.TNG.sim3
    println("building model...")
    val model = new PfLda(cats, Sim3PfParams.alpha, Sim3PfParams.beta,
                          Sim3PfParams.smplSize, Sim3PfParams.numParticles,
                          Sim3PfParams.ess, Sim3PfParams.rejuvBatchSize,
                          Sim3PfParams.rejuvMcmcSteps)

    println("running model...")
    println("DOCUMENT\t\t\tTIME CONSUMPTION PER WORD (MILLISECONDS)")
    for (i <- 0 to corpus.length-1) {
      print(i + " / " + corpus.length)
      //val now = System.nanoTime
      //println("doc " + i + " / " + (corpus.length-1))
      model.ingestDoc(corpus(i))
      // TODO: REMOVE HACKY TIMING CODE FOR BENCHMARKING IMPROVEMENTS
      //println(i + " " + (System.nanoTime - now))
    }
    model.writeTopics("results.txt")

    val mis = Evaluation.nmi(model, labels, wrangle.DataConsts.SIM_3_LABELS)
    println(mis.deep)
  }
}
