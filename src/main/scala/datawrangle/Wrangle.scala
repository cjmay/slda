/** Wrangles 20newgroups corpus for LDA
 */

package wrangle

import scala.util.matching.Regex
import scala.collection.Iterator
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.pull.XMLEventReader
import scala.xml.pull.EvElemStart
import scala.xml.pull.EvElemEnd
import scala.xml.pull.EvText
import scala.annotation.tailrec
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.FilenameFilter
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.BufferedInputStream
import java.util.zip.GZIPInputStream
import java.util.zip.GZIPOutputStream
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

/*
class GigawordReader
  val OOV = "_OOV_"
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
          if (simpleFilter(word)) updateWordCounts(word)
        }
      }
    }
    System.err.println
    numDocs(i) = reader.getNumDocs
  }
  val vocab = Set(OOV) ++ wordCounts.filter(p => p._2 > 1).keySet

  def getVocab: Set[String] = vocab

  private def updateWordCounts(word: String): Unit =
    if (wordCounts.contains(word))
      wordCounts(word) += 1
    else
      wordCounts(word) = 1
}
*/

object GigawordReader {
  def getMatchingFiles(dirname: String, filenameRegex: Regex): Array[File] = 
    new File(dirname).listFiles(new RegexFilter(filenameRegex))

  def gzippedSource(file: File): Source =
    Source.fromInputStream(
			new GZIPInputStream(
				new BufferedInputStream(
					new FileInputStream(file))))

  def gzippedWriter(file: File): BufferedWriter =
    new BufferedWriter(
      new OutputStreamWriter(
        new GZIPOutputStream(
          new FileOutputStream(file)),
        "UTF-8"))

  def main(args: Array[String]): Unit = {
    val tokenizer = new GigawordTokenizer()
    val inputDirname = args(0)
    val outputDirname = args(1)
    for (file <- getMatchingFiles(inputDirname, """.*\.gz""".r)) {
      val src = gzippedSource(file)
			val xmlEventReader = new XMLEventReader(src)
			var inText = false
			val docTokens = new ArrayBuffer[ArrayBuffer[String]]()
			while (xmlEventReader.hasNext) {
				xmlEventReader.next match {
					case EvElemStart(_, label, _, _) =>
						if (label.toLowerCase == "doc")
							docTokens.append(new ArrayBuffer[String]())
						else if (label.toLowerCase == "p")
							inText = true
					case EvElemEnd(_, label) =>
						if (label.toLowerCase == "p")
							inText = false
					case EvText(text) =>
						if (inText)
							docTokens.last ++= tokenizer.tokenize(text)
					case _ => {}
				}
			}

      val outputDir = new File(outputDirname)
      outputDir.mkdirs
      val outputFile = new File(outputDir, file.getName)
      val writer = gzippedWriter(outputFile)
      // TODO train/test
      for (tokens <- docTokens)
        writer.write(tokens.mkString(" ") + "\n")
      writer.close
    }
  }
}

class GigawordTokenizer {
  val blacklist = Text.stopWords(DataConsts.STOP_WORDS)
  val badness = """\W""".r

  /** Return true iff string is nonempty and not in blacklist */
  def simpleFilter(str: String): Boolean = {
    (badness.findAllIn(str).size == 0) && !blacklist(str.toLowerCase)
  }

  def tokenize(s: String): Array[String] =
    s.toLowerCase().split(Text.WHITESPACE).filter(simpleFilter)
}

class RegexFilter(regex: Regex) extends FilenameFilter {
  override def accept(dir: File, name: String): Boolean =
    regex.findFirstIn(name) match {
      case Some(s) => true
      case None => false
    }
}
