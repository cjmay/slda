/** Provides different techniques for evaluating a run of LDA.
 */

package evaluation

import java.io.{ PrintWriter, File }
import scala.math

import lda._


object Evaluation {
  def mi(ourLabels: Iterable[Int], theirLabels: Array[String],
         topics: Int, theirLabelTypes: Iterable[String]):
  Double = {
    val n = ourLabels.size.toDouble
    var s = 0.0
    val outcomes = ourLabels.zip(theirLabels)
    for (t <- 0 until topics) {
      for (theirlabel <- theirLabelTypes) {
        val inter =
          outcomes.filter { x => x._1 == t && x._2 == theirlabel}.size
        if (inter > 0) {
          val allTheirs = theirLabels.filter { x => x == theirlabel }.size
          val allOurs = ourLabels.filter { x => x == t }.size
          val logterm = (n * inter) / (allTheirs.toDouble * allOurs)
          s += (inter / n) * (math.log(logterm) / math.log(2))
        }
      }
    }

    s
  }

  def entropies(ourLabels: Iterable[Int], theirLabels: Array[String],
                topics: Int, theirLabelTypes: Iterable[String]):
  Double = {
    val n = ourLabels.size.toDouble
    var ent1 = 0.0
    for (theirLabel <- theirLabelTypes) {
      val pred = theirLabels.filter { x => x == theirLabel }.size / n
      ent1 += pred * (math.log(pred) / math.log(2))
    }
    ent1 = -ent1

    var ent2 = 0.0
    for (t <- 0 until topics) {
      val pred = ourLabels.filter { x => x == t }.size / n
      ent2 += pred * (math.log(pred) / math.log(2))
    }
    ent2 = -ent2

    (ent1 + ent2)
  }

  def nmi(ourLabels: Iterable[Int], theirLabels: Array[String],
          topics: Int, labelTypes: Iterable[String]): Double =
    (2.0 * mi(ourLabels, theirLabels, topics, labelTypes) /
       entropies(ourLabels, theirLabels, topics, labelTypes))
}
