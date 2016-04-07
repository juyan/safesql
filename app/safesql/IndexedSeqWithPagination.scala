package safesql

/**
  * Created by junyan on 4/6/16.
  */
class IndexedSeqWithPagination[+A](val start: Int, val count: Int, val total: Option[Int], val ele: IndexedSeq[A])
  extends IndexedSeq[A] {

  override def length: Int = ele.length

  override def apply(idx: Int) = ele.apply(idx)
}
