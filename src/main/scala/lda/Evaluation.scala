/** Provides different techniques for evaluating a run of LDA.
 */

package evaluation

import lda._


class Evaluator(inferentialSampler: InferentialGibbsSampler) {
  def outOfSampleEval(globalVect: GlobalUpdateVector): Unit = {
    val (beforePerplexity, beforeLL, beforeNumWords) = inferentialSampler.beforePerplexity(globalVect)
    println("BEFORE OUT-OF-SAMPLE PERPLEXITY " + beforePerplexity)
    println("BEFORE OUT-OF-SAMPLE LOG-LIKELIHOOD " + beforeLL)
    println("BEFORE OUT-OF-SAMPLE NUMWORDS " + beforeNumWords)

    val (afterPerplexity, afterLL, afterNumWords) = inferentialSampler.afterPerplexity(globalVect)
    println("AFTER OUT-OF-SAMPLE PERPLEXITY " + afterPerplexity)
    println("AFTER OUT-OF-SAMPLE LOG-LIKELIHOOD " + afterLL)
    println("AFTER OUT-OF-SAMPLE NUMWORDS " + afterNumWords)

    val (samplePerplexity, sampleLL, sampleNumWords) = inferentialSampler.samplePerplexity(globalVect)
    println("SAMPLE OUT-OF-SAMPLE PERPLEXITY " + samplePerplexity)
    println("SAMPLE OUT-OF-SAMPLE LOG-LIKELIHOOD " + sampleLL)
    println("SAMPLE OUT-OF-SAMPLE NUMWORDS " + sampleNumWords)
  }
}
