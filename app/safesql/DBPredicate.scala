package safesql

import anorm.{NamedParameter, ParameterValue}

/**
  * The predicates for database query backed by a binary tree,
  * with non-leaf node as logic operators and leaves as a single predicate.
  *
  * @param leftNode The left leaf node.
  * @param rightNode The right leaf node.
  * @param relation The logical relation.
  */
class DBPredicates (leftNode: Either[DBPredicates, DBPredicate],
  rightNode: Option[Either[DBPredicates, DBPredicate]],
  relation: Option[DBPredicatesRelation.Value]
) {
  private def nodeToStatement(node: Either[DBPredicates, DBPredicate]): String = {
    node match {
      case Left(predicates) => s"(${predicates.toStatement})"
      case Right(predicate) => s"(${predicate.toStatement})"
    }
  }

  private def getParamsFromNode(node: Either[DBPredicates, DBPredicate]): Seq[NamedParameter] = {
    node match {
      case Left(predicates) => predicates.params
      case Right(predicate) => Seq(NamedParameter(predicate.variable._1, predicate.variable._2))
    }
  }

  def isSinglePredicate : Boolean = {
    rightNode.isEmpty || relation.isEmpty
  }

  def toStatement: String = {
    val leftStatement = nodeToStatement(leftNode)
    val rightStatement = rightNode.map(nodeToStatement)
    if (isSinglePredicate) s"$leftStatement"
    else s"$leftStatement ${relation.get.toString} ${rightStatement.get}"
  }

  def params: Seq[NamedParameter] = {
    val leftParams = getParamsFromNode(leftNode)
    val rightParams = rightNode.map(getParamsFromNode).getOrElse(Seq())
    leftParams ++ rightParams
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

/**
  * A DB predicate.
  *
  * @param columnName The name of the column.
  * @param variable The variable associated with the column, which is a tuple of variable name and value.
  * @param relation The mathematical relation between the column and the variable.
  */
class DBPredicate (
  val columnName: String,
  val variable: (String, ParameterValue),
  val relation: DBPredicateRelation.Value
) {

  def toStatement: String = {
    val variableName = variable._1
    if (relation == DBPredicateRelation.IN_SEQUENCE) {
      s"$columnName ${relation.toString} ({$variableName})"
    } else {
      s"$columnName ${relation.toString} {$variableName}"
    }
  }
}

object DBPredicate {
  def apply(columnName: String, variable: (String, ParameterValue), relation: DBPredicateRelation.Value) = {
    new DBPredicate(
      columnName = columnName,
      variable = variable,
      relation = relation
    )
  }
}

object DBPredicatesRelation extends Enumeration {
  type DBPredicatesRelation = Value

  val AND = Value("AND")
  val OR = Value("OR")
}

object DBPredicateRelation extends Enumeration {
  type DBPredicateRelation = Value

  val EQUALS = Value("=")
  val NOT_EQUAL = Value("!=")
  val GREATER_THAN = Value(">")
  val LESS_THAN = Value("<")
  val GREATER_OR_EQUAL_TO = Value(">=")
  val LESS_OR_EQUAL_TO = Value("<=")
  val IN_SEQUENCE = Value("IN")
}

class NumericOp(val value: ParameterValue,  val incrementalType: NumericOpsType.Value)

/**
  * Special numeric ops to construct a SQL UPDATE query like:
  * "UPDATE FOO SET bar = bar + 1"
  */
object NumericOpsType extends Enumeration {
  type NumericOpsType = Value
  val PLUS = Value("+")
  val MINUS = Value("-")
}
