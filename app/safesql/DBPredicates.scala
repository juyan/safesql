package safesql

import anorm.NamedParameter
import utils.Predef._


/**
  * The predicates for database query backed by a binary tree,
  * with non-leaf node as logic operators and leaves as a single predicate.
  *
  * @param leftNode The left leaf node.
  * @param rightNode The right leaf node.
  * @param relation The logical relation.
  */
class DBPredicates(leftNode: Either[DBPredicates, DBPredicate],
                    rightNode: Option[Either[DBPredicates, DBPredicate]],
                    relation: Option[DBPredicatesRelation.Value]
) {

  private def nodeToStatement(node: Either[DBPredicates, DBPredicate], sequenceGenerator: SequenceGenerator[String]):
    (String, Seq[NamedParameter]) = {

    node match {
      case Left(predicates) => ("df", List())
      case Right(predicate) => predicate.toStatement(sequenceGenerator).mapRight(Seq(_))
    }
  }

  def isSinglePredicate : Boolean = {
    rightNode.isEmpty || relation.isEmpty
  }

  def toStatement(sequenceGenerator: SequenceGenerator[String]): (String, Seq[NamedParameter]) = {
    val (leftStatement, leftParams) = nodeToStatement(leftNode, sequenceGenerator)
    val rightNodeData = rightNode.map(nodeToStatement(_, sequenceGenerator))
    if (isSinglePredicate) (leftStatement, leftParams)
    else (s"$leftStatement ${relation.get.toString} ${rightNodeData.get._1}", leftParams ++ rightNodeData.get._2)
  }
}

object DBPredicates {
  def apply(predicate1: DBPredicate, predicate2: DBPredicate, relation: DBPredicatesRelation.Value) = {
    new DBPredicates(
      leftNode = Right(predicate1),
      rightNode = Some(Right(predicate2)),
      relation = Some(relation)
    )
  }

  def apply(predicates1: DBPredicates, predicates2: Option[DBPredicates], relation: DBPredicatesRelation.Value) : DBPredicates = {
    new DBPredicates(
      leftNode = Left(predicates1),
      rightNode = predicates2.map(Left(_)),
      relation = Some(relation)
    )
  }

  def apply(predicate1: DBPredicate, maybePredicate2: Option[DBPredicate]) = {
    new DBPredicates(
      leftNode = Right(predicate1),
      rightNode = maybePredicate2.map(Right(_)),
      relation = Some(DBPredicatesRelation.AND)
    )
  }


  def apply(predicate1: DBPredicate, maybePredicates: Option[DBPredicates], relation: DBPredicatesRelation.Value) = {
    new DBPredicates(
      leftNode = Right(predicate1),
      rightNode = maybePredicates.map(Left(_)),
      relation = Some(relation)
    )
  }

  def apply(predicate: DBPredicate) = {
    new DBPredicates(
      leftNode = Right(predicate),
      rightNode = None,
      relation = None
    )
  }

  def apply(predicates: Iterable[DBPredicates], relation: DBPredicatesRelation.Value) : DBPredicates = {
    if (predicates.size == 1) {
      predicates.head
    }
    else if (predicates.isEmpty) {
      throw new IllegalArgumentException("Empty iterable of predicate passed to construct a predicates")
    }
    else {
      val firstPredicates = predicates.head
      predicates.tail.foldLeft[DBPredicates](firstPredicates) { (result, predicates) =>
        val merged = DBPredicates(result, Some(predicates), DBPredicatesRelation.OR)
        merged
      }
    }
  }
}

object DBPredicatesRelation extends Enumeration {
  type DBPredicatesRelation = Value

  val AND = Value("AND")
  val OR = Value("OR")
}
