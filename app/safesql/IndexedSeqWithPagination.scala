package safesql

/**
  * Created by junyan on 4/6/16.
  */
case class IndexedSeqWithPagination[+A](
   start: Int,
   count: Int,
   total: Option[Int],
   elements: IndexedSeq[A]
) extends IndexedSeq[A] {

  override def length: Int = elements.length

  override def apply(idx: Int) = elements.apply(idx)
}

object IndexedSeqWithPagination {
  def apply[A](elements: IndexedSeq[A], pagingContext: PagingContext) = {
    IndexedSeqWithPagination[A](pagingContext.start, elements.length, None, elements)
  }
}
