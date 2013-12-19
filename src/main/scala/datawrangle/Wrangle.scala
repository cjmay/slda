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
  val WHITELIST = "data/TNG_WHITELIST"
  val STOP_WORDS = "data/TNG_STOP_WORDS"
  val GZIP_FILE_REGEX = """.*\.gz""".r
  val GIGAWORD_DATA_DIR = "data/gigaword/nyt_eng.split"
  val TNG_DATA_DIR = "data/tng.split"
  val OOV = "_OOV_"
}

object GigawordDatasetSplitter {
  def main(args: Array[String]): Unit = {
    val trainFrac = args(0).toDouble
    val inputDirname = args(1)
    val outputDirname = args(2)
    val trainDir = new File(outputDirname, "train")
    val testDir = new File(outputDirname, "test")

    val files =
      GzipDataset.sortedFiles(GigawordReader.getMatchingFiles(inputDirname))
    var docIdx = 0
    for (file <- files) {
      val trainWriter = GzipDataset.makeWriter(trainDir, file)
      val testWriter = GzipDataset.makeWriter(testDir, file)

      val src = GzipDataset.gzippedSource(file)
      for (line <- src.getLines()) {
        val train = Stats.sampleBernoulli(trainFrac)
        val writer = if (train) trainWriter else testWriter
        writer.write(docIdx.toString + " " + line + "\n")
        docIdx += 1
      }

      trainWriter.close
      testWriter.close
    }
  }
}

object GigawordReader {
  def main(args: Array[String]): Unit = {
    val tokenizer = new Tokenizer()
    val inputDirname = args(0)
    val outputDirname = args(1)
    for (file <- GzipDataset.getMatchingFiles(inputDirname)) {
      val src = GzipDataset.gzippedSource(file)
      val xmlEventReader = new XMLEventReader(src)
      var inText = false
      val tokens = new ArrayBuffer[String]()

      val outputDir = new File(outputDirname)
      outputDir.mkdirs
      val outputFile = new File(outputDir, file.getName)
      val writer = GzipDataset.gzippedWriter(outputFile)

      while (xmlEventReader.hasNext) {
        xmlEventReader.next match {
          case EvElemStart(_, label, _, _) =>
            if (label.toLowerCase == "doc")
              tokens.clear()
            else if (label.toLowerCase == "p")
              inText = true
          case EvElemEnd(_, label) =>
            if (label.toLowerCase == "doc")
              writer.write(tokens.mkString(" ") + "\n")
            else if (label.toLowerCase == "p")
              inText = false
          case EvText(text) =>
            if (inText)
              tokens ++= tokenizer.tokenize(text)
          case _ => {}
        }
      }

      writer.close
    }
  }
}

object GzipDataset {
  def makeWriter(outputDir: File, inputFile: File): BufferedWriter = {
    outputDir.mkdirs()
    gzippedWriter(new File(outputDir, inputFile.getName))
  }

  def sortedFiles(files: Array[File]): Array[File] =
    files.sortBy(f => f.getName)

  def getMatchingFiles(dirname: String): Array[File] = 
    new File(dirname).listFiles(new RegexFilter(DataConsts.GZIP_FILE_REGEX))

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
}

class GzipDataset(dataDir: String) {
  val trainDir = new File(dataDir, "train").getPath
  val trainFiles = sortedFiles(
    getMatchingFiles(trainDir, DataConsts.GZIP_FILE_REGEX))

  val testDir = new File(dataDir, "test").getPath
  val testFiles = sortedFiles(
    getMatchingFiles(testDir, DataConsts.GZIP_FILE_REGEX))

  val wordCounts = new HashMap[String,Int]()

  for (file <- trainFiles) {
    println("* " + file.getPath)
    val src = gzippedSource(file)
    for (line <- src.getLines()) {
      val doc = line.split(Text.WHITESPACE)
      val docIdx = doc(0).toInt
      val tokens = doc.drop(1)
      for (token <- tokens)
        updateWordCounts(token)
    }
  }

  val vocab = Set(DataConsts.OOV) ++ wordCounts.filter(p => p._2 > 1).keySet

  def replaceOOV(token: String): String =
    if (vocab.contains(token)) token else DataConsts.OOV

  def fileDocs(file: File): Iterator[(Int,Array[String])] =
    for (line <- gzippedSource(file).getLines())
      yield {
        val doc = line.split(Text.WHITESPACE)
        val docIdx = doc(0).toInt
        val tokens = doc.drop(1)
        (docIdx, tokens.map(replaceOOV))
      }

  def trainDocs: Iterator[(Int,Array[String])] =
    for (file <- trainFiles.toIterator; doc <- fileDocs(file))
      yield doc

  def testDocs: Iterator[(Int,Array[String])] =
    for (file <- testFiles.toIterator; doc <- fileDocs(file))
      yield doc

  def getVocab: Set[String] = vocab

  private def updateWordCounts(word: String): Unit =
    if (wordCounts.contains(word))
      wordCounts(word) += 1
    else
      wordCounts(word) = 1
}

class Tokenizer {
  val blacklist = Text.stopWords(DataConsts.STOP_WORDS)
  val badness = """\W|_""".r

  /** Return true iff string is nonempty and is not in blacklist */
  def simpleFilter(str: String): Boolean =
    !str.isEmpty && !blacklist(str)

  /** Return lower-cased, punctuation-stripped string */
  def normalize(str: String): String =
    badness.replaceAllIn(str, "").toLowerCase

  def tokenize(s: String): Array[String] =
    s.toLowerCase().split(Text.WHITESPACE).map(normalize).filter(simpleFilter)
}

class RegexFilter(regex: Regex) extends FilenameFilter {
  override def accept(dir: File, name: String): Boolean =
    regex.findFirstIn(name) match {
      case Some(s) => true
      case None => false
    }
}
