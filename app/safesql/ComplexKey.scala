package safesql

/**
  * Created by junyan on 4/2/16.
  */
trait ComplexKey[T] {

  def firstKeyPart: T

  def toPredicates(keyPartColumns: IndexedSeq[String]): DBPredicates
}
