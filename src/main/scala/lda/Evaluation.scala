/** Provides different techniques for evaluating a run of LDA.
 */

package evaluation

import lda._


class DualEvaluator(inferentialSampler: InferentialGibbsSampler) {
  def outOfSampleEval(globalVect: GlobalUpdateVector): Unit = {
    val (perplexity, ll) = inferentialSampler.perplexity(globalVect)
    println("OUT-OF-SAMPLE PERPLEXITY " + perplexity)
    println("OUT-OF-SAMPLE LOG-LIKELIHOOD " + ll)
  }
}
