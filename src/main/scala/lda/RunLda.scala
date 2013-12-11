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
  val numParticles: Int = 20
  val ess: Double = 4.0
  val rejuvBatchSize: Int = 30
  val rejuvMcmcSteps: Int = 20
  val initialBatchSize: Int = 100
  val initialBatchMcmcSteps: Int = 150
  val topics: Int = 10
  val seed: Long = 0
  val fixInitialSample: Boolean = true
  val fixInitialModel: Boolean = false
  val inferMcmcSteps: Int = 5
  val inferJoint: Boolean = false
	val dataDir: String = "/export/common/data/corpora/LDC/LDC2012T21/data/xml"
  val trainDataRegex: Regex = """nyt_eng_1994\d\d\.xml\.gz""".r
  val testDataRegex: Regex = """nyt_eng_1995\d\d\.xml\.gz""".r
}

object GigaParams extends RunLdaParams { }

object RunLda {
  val OOV = "_OOV_"
  val blacklist = Text.stopWords(DataConsts.TNG_STOP_WORDS)

  private def substituteOOV(word: String, vocab: Set[String]): String =
    if (vocab.contains(word)) word else OOV

  /** Return true iff string is nonempty and not in blacklist */
  private def simpleFilter(str: String): Boolean = {
    val patt = new Regex("\\W");
    (patt.findAllIn(str).size == 0) && !blacklist(str.toLowerCase)
  }

  /** Tokenize doc and remove stop words */
  private def makeBOW(doc: String): Array[String] =
    Text.bow(doc, simpleFilter(_))

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
    val trainData = new GigawordReader(params.dataDir, params.trainDataRegex)
    val testData = new GigawordReader(params.dataDir, params.testDataRegex)

    val trainDocs = trainData.shuffledDocs
    
    // If we fixed a random seed for the data shuffle but want a random
    // Gibbs initialization, reinitialize seed randomly
    if (params.fixInitialSample && ! params.fixInitialModel)
      Stats.setDefaultSeed()

    // Compute vocabulary as OOV symbol plus all non-singletons in
    // training data
    val vocab = Set(OOV) ++
      corpus.
      flatten.
      groupBy(identity).
      mapValues(_.size).
      filter(p => p._2 > 1).
      keySet
    println("vocab size " + vocab.size)

    // Replace singletons with OOV
    for (docIdx <- 0 until corpus.size) {
      val doc = corpus(docIdx)
      for (wordIdx <- 0 until doc.size) {
        doc(wordIdx) = substituteOOV(doc(wordIdx), vocab)
      }
    }

    println("training size " + corpus.map(_.size).sum)
    println("training num OOV "
      + corpus.map(d => d.filter(_ == OOV).size).sum)

    val inferDocsTokens =
      params.testCorpus.map(makeBOW(_).map(substituteOOV(_, vocab)))
    var inferentialSampler = new InferentialGibbsSampler(params.cats.size,
      params.alpha, params.beta, vocab.size,
      params.inferMcmcSteps, inferDocsTokens,
      params.inferJoint)
    val evaluator = new DualEvaluator(params.cats.size, params.cats,
      labels, params.initialBatchSize, params.testLabels, inferentialSampler)

    println("testing size " + inferDocsTokens.map(_.size).sum)
    println("testing num OOV "
      + inferDocsTokens.map(d => d.filter(_ == OOV).size).sum)

    println("initializing model...")
    val model = new PfLda(params.topics, params.alpha, params.beta,
                          vocab,
                          params.reservoirSize, params.numParticles,
                          params.ess, params.rejuvBatchSize,
                          params.rejuvMcmcSteps)

    val initialBatchSize = params.initialBatchSize
    val initDocs = Array.fill(initialBatchSize)(Array.fill(0)(""))
    val trainDocsIter = trainDocs.iterator
    for (i <- 0 until initialBatchSize) {
      if (! trainDocsIter.hasNext)
        throw new RuntimeException("initialBatchSize > no. training documents")
      initDocs(i) = trainDocsIter.next
    }
    model.initialize(initDocs, params.initialBatchMcmcSteps)

    val inferDocsTokens = testData.shuffledDocs.toArray
    var inferentialSampler = new InferentialGibbsSampler(params.topics,
      params.alpha, params.beta, inferDocsTokens)
    val evaluator = new DualEvaluator(params.topics, inferentialSampler)

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
    }
    model.evaluate(evaluator)
    model.writeTopics("results.txt")
  }
}
