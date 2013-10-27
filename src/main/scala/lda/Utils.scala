package lda

import scala.annotation.tailrec
import scala.util.Random

import stream.ReservoirSampler

object Stats {
  var sampler = new Random()

  def setDefaultSeed(): Unit = {
    sampler = new Random()
  }

  def setSeed(seed: Long): Unit = {
    sampler = new Random(seed)
  }

  def sampleWithoutReplacement [T:Manifest](a: Array[T], k: Int): Array[T] = {
    val sample = new ReservoirSampler[T](k)
    sample.addAll(a)
    sample.getSampleSet
  }

  def shuffle[T](seq: Seq[T]): Seq[T] =
    sampler.shuffle(seq)

  def normalize(arr: Array[Double]): Array[Double] = {
    arr.length match {
      case 0 => arr
      case 1 => { arr(0) = 1; arr }
      case _ => {
        val sum = arr.reduceLeft(_+_)
        (0 to arr.length-1).foreach { i => arr(i) = arr(i) / sum }
        arr
      }
    }
  }
  
  /** Normalizes array of Double and makes it into a CDF; does this by
   side-effect, so it's good for low-mem situations */
  def normalizeAndMakeCdf (arr: Array[Double]): Array[Double] = {
    // WARNING SIDE EFFECTS ON `arr`
    @tailrec
    def loop (i: Int, sum: Double, acc: Array[Double]): Array[Double] =
      if (i >= acc.length) {
        acc(acc.length-1) = 1 // make sure last element is 1 and not eg 0.999
        acc
      } else {
        val curr = (acc(i) / sum) + acc(i-1)
        acc(i) = curr
        loop(i+1, sum, acc)
      }
    arr.length match {
      case 0 => arr
      case 1 => { arr(0) = 1; arr }
      case _ => {
        val sum = arr.reduceLeft(_+_)
        arr(0) = arr(0) / sum
        loop(1, sum, arr)
      }
    }
  }

  def sampleInt(bound: Int): Int =
    sampler.nextInt(bound)
  
  /** Samples from simple categorical distribution; takes a normalized
   probability measure and returns a randomly-sampled index */
  def sampleCategorical (cdf: Array[Double]): Int = {
    val r = sampler.nextDouble()
    @tailrec
    def loop (currIdx: Int): Int =
      if (cdf(currIdx-1) <= r && r < cdf(currIdx)) currIdx
      else loop(currIdx+1)
    (cdf.length, r < cdf(0)) match {
      case (0, _) => throw new RuntimeException("CDF has 0 elements!")
      case (1, _) => 0
      case (_, true) => 0
      case (_, _) => loop(1)
    }
  }
}

object Math {
  def norm (vector: Array[Double], d: Int): Double = {
    math.pow(vector.reduceLeft{ (acc,n) => math.pow(n,d) + acc }, 1.0/d)
  }

  def max [T <% Ordered[T]](a: T, b: T): T = List(a, b).max

  def min [T <% Ordered[T]](a: T, b: T): T = List(a, b).min
}
