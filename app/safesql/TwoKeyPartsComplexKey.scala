package safesql

/**
  * Created by junyan on 4/2/16.
  */
class TwoKeyPartsComplexKey[T1,T2]
(
   val keyPart1 : DBParameter,
   val keyPart2 : Option[DBParameter]
) extends ComplexKey {

  override def toPredicates(keyPartColumns: IndexedSeq[String]): DBPredicates = {
    val keyPart1Column = keyPartColumns(0)
    val keyPart2Column = keyPartColumns(1)
    val firstKeyPartPredicate = DBPredicate(keyPart1Column, keyPart1, DBPredicateRelation.EQUALS)
    val maybeSecondKeyPartPredicate = keyPart2.map { key =>
      DBPredicate(keyPart2Column, key, DBPredicateRelation.EQUALS)
    }
    DBPredicates(firstKeyPartPredicate, maybeSecondKeyPartPredicate)
  }

  def firstKeyPart: T1 = keyPart1.value.asInstanceOf[T1]

  def secondKeyPart: Option[T2] = keyPart2.map(_.value.asInstanceOf[T2])

}

object TwoKeyPartsComplexKey {
  def apply[T1,T2](keyPart1: DBParameter, keyPart2: DBParameter) = {
    new TwoKeyPartsComplexKey[T1, T2](keyPart1, Some(keyPart2))
  }

  def apply[T1,T2](keyPart1: DBParameter, keyPart2: Option[DBParameter]) = {
    new TwoKeyPartsComplexKey[T1, T2](keyPart1, keyPart2)
  }
}
