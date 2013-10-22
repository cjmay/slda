/** Wrangles 20newgroups corpus for LDA
 */

package wrangle

import scala.io.Source
import scala.annotation.tailrec
import java.io.File

import lda.Stats


/** Handles basic IO for our little package */
object Io {
  def files (dir: String): Array[java.io.File] = {
    val f = new File(dir)
    val files = f.listFiles
    if (files == null)
      throw new RuntimeException("No files in data directory: " + dir)
    else files
  }

  def makeDirIfNE (dir: String): Unit = {
    val f = new File(dir)
    if (!f.exists())
      f.mkdir()
  }

  /** Transforms contents of a file into a single String */
  def fileToString (f: java.io.File): String = {
    val source = Source.fromFile(f, "MacRoman")
    val lines = source.mkString
    source.close()
    lines
  }

  /** Returns all files in corpus as an array of Strings */
  def rawCorpus (dir: String): Array[String] = {
    val fs = files(dir)
    @tailrec
    def loop (i: Int, acc: Array[String]): Array[String] = {
      if (i >= fs.length) acc
      else {
        acc(i) = fileToString(fs(i))
        loop(i+1, acc)
      }
    }
    loop(0, new Array[String](fs.length))
  }

  def rawCorpus (dirs: List[String]): Array[String] = {
    def loop (li: List[String]): Array[String] = li match {
      case Nil => Array()
      case hd::Nil => rawCorpus(hd)
      case hd::tl => rawCorpus(hd) ++ loop(tl)
    }
    loop(dirs)
  }
}

/** Simple functions for processing text */
object Text {
  val WHITESPACE = "\\s+"

  /** Tokenizes a document, removing everything not a stopwords filter */
  def tokenize (s: String, filter: String => Boolean): Array[String] =
    s.split(WHITESPACE).filter(filter)

  /** Generates a Set that contains stop words from a file */
  def stopWords (fname: String): Set[String] = {
    Io.fileToString(new File(fname)).split(WHITESPACE).toSet
  }

  /** Converts documents into a single array of words
   *
   * Takes `docs`, our array of documents, breaks each doc into an array
   * of words, and then smashes all those arrays together into a single
   * array.
   *
   * Additionally, we return an array that maps each word to the document
   * it came from, ie, the word `accuDocs(i)` will have come from document
   * `accuAssig[i]`
   *
   * @return accuDocs array of words
   * @return accuAssig array of Ints -- accuAssig(i) is document # of ith wrd
   */
  def bow (docs: Array[String], filter: String => Boolean):
  (Array[String], Array[Int]) = {
    @tailrec
    def loop (i: Int, accuDocs: Array[String], accuAssig: Array[Int]):
    (Array[String], Array[Int]) = {
      if (i == docs.length) (accuDocs, accuAssig)
      else {
        val nextDocs = tokenize(docs(i), filter)
        val nextAssig = Array.fill(nextDocs.length)(i)
        loop(i + 1, accuDocs ++ nextDocs, accuAssig ++ nextAssig)
      }
    }
    loop(0, Array.empty, Array.empty)
  }

  /** Wrapper simply returns the tokenized document */
  def bow (doc: String, filter: String => Boolean): Array[String] = {
    val (words, docs) = bow(Array(doc), filter)
    words
  }
}

object DataConsts {
  val RESULTS_DIR = "results/"
  val DATA_DIR = "data/"
  val TNG_TRAIN_DIR = DATA_DIR + "20news-bydate-train/"
  val TNG_WHITELIST = DATA_DIR + "TNG_WHITELIST"
  val TNG_STOP_WORDS = DATA_DIR + "TNG_STOP_WORDS"
}

/** Wrangles the 20 Newsgroups dataset
 */
object TNG {
  def orderedMixture(categories: List[String]):
  (Array[String], Array[String], List[String]) = {
    val categoriesArray = categories.toArray
    val documentsByCategory = categoriesArray.map({category =>
      Io.rawCorpus(wrangle.DataConsts.TNG_TRAIN_DIR + category)
    })
    val numDocumentsPerCategory = documentsByCategory.map(_.length)
    val numDocuments = numDocumentsPerCategory.sum

    val documents = Array.fill(numDocuments)("")
    val documentLabels = Array.fill(numDocuments)("")
    val categoryIndices = Array.fill(categoriesArray.length)(0)

    var cdf = Stats.normalizeAndMakeCdf(
      numDocumentsPerCategory.map(_.toDouble))

    @tailrec
    def sampleCategory(): Int = {
      val catIdx = Stats.sampleCategorical(cdf)
      if (categoryIndices(catIdx) == numDocumentsPerCategory(catIdx))
        sampleCategory()
      else
        catIdx
    }

    for (docIdx <- 0 until numDocuments) {
      val catIdx = sampleCategory()

      documents(docIdx) = documentsByCategory(catIdx)(categoryIndices(catIdx))
      documentLabels(docIdx) = categoriesArray(catIdx)

      categoryIndices(catIdx) += 1
    }

    (documents, documentLabels, categories)
  }

  def sim3 =
    orderedMixture(
      List("comp.graphics", "comp.os.ms-windows.misc", "comp.windows.x"))

  def rel3 =
    orderedMixture(
      List("talk.politics.misc", "talk.politics.guns", "talk.politics.mideast"))

  def diff3 =
    orderedMixture(
      List("alt.atheism", "rec.sport.baseball", "sci.space"))
}
