package safesql

import scala.collection.concurrent.Map
import collection.JavaConverters._

/**
  * A sequence generator. Keeps the sequence number for
  * each key.
  */
class SequenceGenerator[T] {

  val keyMap: Map[T, Long] = new java.util.concurrent.ConcurrentHashMap[T,Long]().asScala

  def getSequenceForKey(key: T): Long = {
    keyMap.get(key) match {
      case Some(sequence)  => keyMap.put(key, sequence + 1).get
      case None => keyMap.put(key, 1); 0
    }
  }

  def getNameAndSequenceForKey(key: T): String = {
    s"$key${getSequenceForKey(key)}"
  }
}

object SequenceGenerator {
  def apply[T]() = {
    new SequenceGenerator[T]()
  }
}
