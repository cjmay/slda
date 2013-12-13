/** Wrangles 20newgroups corpus for LDA
 */

package wrangle

import scala.util.matching.Regex
import scala.collection.Iterator
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.annotation.tailrec
import java.io.File
import java.io.FilenameFilter
import lda.Stats

import edu.jhu.agiga.AgigaPrefs
import edu.jhu.agiga.AgigaDocument
import edu.jhu.agiga.StreamingDocumentReader


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
  val DATA_DIR = "data/"
  val WHITELIST = DATA_DIR + "TNG_WHITELIST"
  val STOP_WORDS = DATA_DIR + "TNG_STOP_WORDS"

}

class GigawordReader(dirname: String, filenameRegex: Regex) {
  val OOV = "_OOV_"
  val blacklist = Text.stopWords(DataConsts.STOP_WORDS)
  val prefs = new AgigaPrefs()
  prefs.setAll(false)
  prefs.setWord(true)
  val files = new File(dirname).listFiles(new RegexFilter(filenameRegex))
  val wordCounts = new HashMap[String,Int]()
  var numDocs = Array.fill(files.length)(0)
  for (i <- 0 until files.length) {
    val file = files(i)
		System.err.println(file.getPath())
    val reader = new StreamingDocumentReader(file.getPath(), prefs)
    while (reader.hasNext) {
			System.err.print(".")
      val sentenceIterator = reader.next.getSents.iterator
      while (sentenceIterator.hasNext) {
        val tokenIterator = sentenceIterator.next.getTokens.iterator
        while (tokenIterator.hasNext) {
          val word = tokenIterator.next.getWord
          if (simpleFilter(word)) updateCounts(word)
        }
      }
    }
		System.err.println
    numDocs(i) = reader.getNumDocs
  }
  val vocab = Set(OOV) ++ wordCounts.filter(p => p._2 > 1).keySet

  def shuffledDocs: Seq[Array[String]] = {
    val indices = Stats.shuffle(
      (0 until numDocs.size).map(
        i => (0 until numDocs(i)).map(
          j => (i, j)
        ).toArray
      ).flatten)
    for ((i, j) <- indices)
      yield {
        val file = files(i)
        val reader = new StreamingDocumentReader(file.getPath(), prefs)
        for (k <- 0 until j) reader.next
        docToWords(reader.next)
      }
  }

  def docs: Seq[Array[String]] =
    for (i <- 0 until files.size; doc <- readerToDocs(fileToReader(files(i))))
      yield docToWords(doc)

  def fileToReader(file: File): StreamingDocumentReader =
    new StreamingDocumentReader(file.getPath(), prefs)

  def readerToDocs(reader: StreamingDocumentReader): Seq[AgigaDocument] =
    readerToDocsHelper(reader, Seq.empty)

  @tailrec
  private def readerToDocsHelper(reader: StreamingDocumentReader,
      seq: Seq[AgigaDocument]): Seq[AgigaDocument] =
    if (reader.hasNext)
      readerToDocsHelper(reader, reader.next +: seq)
    else
      seq

  def docToWords(doc: AgigaDocument): Array[String] = {
    val words = new ArrayBuffer[String]()
    val sentenceIterator = doc.getSents.iterator
    while (sentenceIterator.hasNext) {
      val tokenIterator = sentenceIterator.next.getTokens.iterator
      while (tokenIterator.hasNext) {
        val word = tokenIterator.next.getWord
        if (simpleFilter(word)) words += word
      }
    }
    words.toArray
  }

  def getVocab: Set[String] = vocab

  /** Return true iff string is nonempty and not in blacklist */
  private def simpleFilter(str: String): Boolean = {
    val patt = new Regex("\\W");
    (patt.findAllIn(str).size == 0) && !blacklist(str.toLowerCase)
  }

  private def updateCounts(word: String): Unit =
    if (wordCounts.contains(word))
      wordCounts(word) += 1
    else
      wordCounts(word) = 1
}

class RegexFilter(regex: Regex) extends FilenameFilter {
  override def accept(dir: File, name: String): Boolean =
    regex.findFirstIn(name) match {
      case Some(s) => true
      case None => false
    }
}
