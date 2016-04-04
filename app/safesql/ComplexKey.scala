package safesql

/**
  * Created by junyan on 4/2/16.
  */
trait ComplexKey {
  def toPredicates(keyPartColumns: IndexedSeq[String]): DBPredicates
}
