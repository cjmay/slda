/** Provides different techniques for evaluating a run of LDA.
 */

package evaluation

import lda._
import stream._

trait Evaluator {
  def outOfSampleEval(globalVect: GlobalUpdateVector): Unit
  def updateDatasetPos(datasetIdx: Int): Unit
}

class GigawordEvaluator(inferentialSampler: InferentialGibbsSampler,
    streamHeadBuffer: StreamHeadBuffer) extends Evaluator {
  override def outOfSampleEval(globalVect: GlobalUpdateVector): Unit = {
    val (beforePerplexity, beforeLL, beforeNumWords) = inferentialSampler.perplexity(streamHeadBuffer.beforeIterator, globalVect)
    println("BEFORE OUT-OF-SAMPLE PERPLEXITY " + beforePerplexity)
    println("BEFORE OUT-OF-SAMPLE LOG-LIKELIHOOD " + beforeLL)
    println("BEFORE OUT-OF-SAMPLE NUMWORDS " + beforeNumWords)

    val (afterPerplexity, afterLL, afterNumWords) = inferentialSampler.perplexity(streamHeadBuffer.afterIterator, globalVect)
    println("AFTER OUT-OF-SAMPLE PERPLEXITY " + afterPerplexity)
    println("AFTER OUT-OF-SAMPLE LOG-LIKELIHOOD " + afterLL)
    println("AFTER OUT-OF-SAMPLE NUMWORDS " + afterNumWords)

    val (samplePerplexity, sampleLL, sampleNumWords) = inferentialSampler.perplexity(streamHeadBuffer.sampleIterator, globalVect)
    println("SAMPLE OUT-OF-SAMPLE PERPLEXITY " + samplePerplexity)
    println("SAMPLE OUT-OF-SAMPLE LOG-LIKELIHOOD " + sampleLL)
    println("SAMPLE OUT-OF-SAMPLE NUMWORDS " + sampleNumWords)
  }

  override def updateDatasetPos(datasetIdx: Int): Unit =
    streamHeadBuffer.add(datasetIdx)
}

class StaticEvaluator(inferentialSampler: InferentialGibbsSampler,
    docs: Array[Array[String]]) extends Evaluator {
  def outOfSampleEval(globalVect: GlobalUpdateVector): Unit = {
    val (perplexity, ll, numWords) = inferentialSampler.perplexity(docs.iterator, globalVect)
    println("OUT-OF-SAMPLE PERPLEXITY " + perplexity)
    println("OUT-OF-SAMPLE LOG-LIKELIHOOD " + ll)
    println("OUT-OF-SAMPLE NUMWORDS " + numWords)
  }

  def updateDatasetPos(datasetIdx: Int): Unit = { }
}
