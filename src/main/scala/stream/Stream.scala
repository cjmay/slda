/** Data structures and algorithms for processing streams. For example, this
 * package includes sampling algorithms, like reservoir sampling and heavy
 * hitters.
 */

package stream

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.util.{ Random => Random }

import globals.Constants
import lda.Stats

trait ImmutableAssociativeStreamSampler[T] {
  def capacity: Int // total number of slots availble in sampler
  def occupied: Int // total number of slots occupied in sampler
  def apply(i: Int): T
  def getSampleSet: Array[T]
}

/** Associative stream samplers are backed by a regular associative array,
 * meaning their elements are not key-addressable. Reservoir sampling, for
 * example, often produces an associative array of randomly sampled elements,
 * in random order.
 */
trait AssociativeStreamSampler[T] extends ImmutableAssociativeStreamSampler[T] {
  def add(item: T): AssociativeStreamSampler[T]
  def addAll(items: Array[T]): AssociativeStreamSampler[T]
  def reset(newK: Int): Unit
}

trait ImmutableMappingStreamSampler[T] {
  def capacity: Int // total number of slots availble in sampler
  def occupied: Int // total number of slots occupied in sampler
  def apply(item: T): Int
  def getSampleSet: Map[T, Int]
}

/** Mapping stream samplers are backed by a map or hash table, and thus their
 * elements are key-addressable. The Misra-Gries approach to the heavy hitters
 * problem, for example, will return a map of elements that are believed to have
 * occurred more than k times.
 */
trait MappingStreamSampler[T] extends ImmutableMappingStreamSampler[T] {
  def add(item: T): MappingStreamSampler[T]
  def addAll(items: Array[T]): MappingStreamSampler[T]
  def reset(newK: Int): Unit
}

/** Simple implementation of reservoir sampling.
 *
 * The classical treatment is given by Vitter in Random Sampling With a
 * Reservoir, but it's very well known at this point.
 *
 * WARNING: HIGHLY STATEFUL
 */
class ReservoirSampler[T: Manifest](tempK: Int) extends
AssociativeStreamSampler[T] {
  var k = tempK
  var sample = new Array[T](k)
  var currIdx = 0

  override def reset(newK: Int): Unit = {
    sample = new Array[T](newK)
    k = newK
    currIdx = 0
  }

  /** Add returns a ReservoirSampler so we can chain `add` calls together */
  override def add(item: T): ReservoirSampler[T] = {
    addItem(item)
    this
  }

  /** Add item to reservoir and return triple containing index in
    * reservoir (or `DidNotAddToSampler`), a boolean indicating whether
    * an element was ejected from the reservoir, and the ejected
    * element (or the item that was to be added)
    */
  def addItem(item: T): (Int,Boolean,T) = {
    val triple = if (currIdx >= k) {
      // IMPORTANT: `nextInt()` not inclusive, so the `+1` is required
      val slotToReplace = Stats.sampleInt(currIdx+1)
      if (slotToReplace < k) {
        val ejectedElement = sample(slotToReplace)
        sample(slotToReplace) = item
        (slotToReplace, true, ejectedElement)
      } else {
        (Constants.DidNotAddToSampler, false, item)
      }
    } else {
      sample(currIdx) = item
      (currIdx, false, item)
    }
    
    currIdx += 1
    triple
  }

  override def addAll(items: Array[T]): ReservoirSampler[T] = {
    items.foreach { item => add(item) }
    this
  }

  override def apply(i: Int): T = {
    if (i >= currIdx)
      throw new RuntimeException("reservoir sample has seen <= " + i +
                                 " objects!")
    else sample(i)
  }

  /** Output an array with all the elements in the sample; ie, if our sample
   has < k elements in it, we only output the elements we have */
  override def getSampleSet: Array[T] = {
    if (currIdx < k) {
      var out = new Array[T](currIdx)
      Array.copy(sample, 0, out, 0, currIdx)
      return out
    }
    else sample
  }

  /** Capacity of sampler, ie, maximum number of slots available total */
  override def capacity: Int = k

  /** Number of elemtents in reservoir */
  override def occupied: Int =
    if (currIdx >= k) k
    else currIdx

  override def toString: String = getSampleSet.deep.mkString(" ")
}

/** Implements the Misra-Gries Frequent algorithm for the heavy hitters problem.
 *
 * For details see "Space-optimal Heavy Hitters with Strong Error Bounds",
 * by Berinde, Indyk, Cormode, and Strauss.
 *
 * WARNING: HIGHLY STATEFUL
 */
class FrequentSampler[T: Manifest](tempK: Int) extends
MappingStreamSampler[T] {
  var k = tempK
  var sample = Map[T, Int]()
  
  override def reset(newK: Int): Unit = {
    sample = Map[T, Int]()
    k = newK
  }

  override def add(item: T): FrequentSampler[T] = {
    if (sample.contains(item))
      sample += item -> (sample(item) + 1)
    else if (sample.size < k)
      sample += item -> 1
    else {
      for ((key,value) <- sample) {
        if (value == 1) sample -= key
        else sample += key -> (value - 1)
      }
    }

    this
  }
  
  override def addAll(items: Array[T]): FrequentSampler[T] = {
    @tailrec
    def loop(i: Int): Unit = {
      if (i >= items.length) Unit
      else {
        add(items(i))
        loop(i+1)
      }
    }
    loop(0)
    this
  }
  
  override def apply(item: T): Int =
    if (sample contains item) sample(item)
    else 0
  
  override def getSampleSet: Map[T, Int] = sample
  
  override def capacity: Int = k
  override def occupied: Int = sample.size
}

/** Implements the SpaceSaving algorithm for the heavy hitters problem.
 *
 * For details see "Space-optimal Heavy Hitters with Strong Error Bounds",
 * by Berinde, Indyk, Cormode, and Strauss.
 *
 * WARNING: HIGHLY STATEFUL
 */
class SpaceSavingSampler[T: Manifest](tempK: Int) extends
MappingStreamSampler[T] {
  var k = tempK
  var sample = Map[T, Int]()
  
  override def reset(newK: Int): Unit = {
    sample = Map[T, Int]()
    k = newK
  }

  override def add(item: T): SpaceSavingSampler[T] = {
    if (sample.contains(item))
      sample += item -> (sample(item) + 1)
    else if (sample.size < k)
      sample += item -> 1
    else {
      val y = sample.minBy(_._2)._1
      sample += item -> (sample(y) + 1)
      sample -= y
    }

    this
  }
  
  override def addAll(items: Array[T]): SpaceSavingSampler[T] = {
    @tailrec
    def loop(i: Int): Unit = {
      if (i >= items.length) Unit
      else {
        add(items(i))
        loop(i+1)
      }
    }
    loop(0)
    this
  }
  
  override def apply(item: T): Int =
    if (sample contains item) sample(item)
    else 0
  
  override def getSampleSet: Map[T, Int] = sample
  override def capacity: Int = k
  override def occupied: Int = sample.size
}

/** Immutable view of an associative stream sampler comprising k
  * samples, with replacement, from that sampler
  */
class BootstrapSampler[T: Manifest](
    inner: ImmutableAssociativeStreamSampler[T], k: Int)
    extends ImmutableAssociativeStreamSampler[T] {
  val sampleIndices =
    Stats.sampleWithReplacement((0 until inner.occupied).toArray, k)

  override def capacity: Int = k

  override def occupied: Int = k

  override def apply(i: Int): T =
    if (i >= k)
      throw new RuntimeException("bootstrapped sample has seen <= " + i +
                                 " objects!")
    else inner(sampleIndices(i))

  override def getSampleSet: Array[T] =
    (0 until k).map(apply(_)).toArray

  def getSampleIndices: Array[Int] = sampleIndices
}
