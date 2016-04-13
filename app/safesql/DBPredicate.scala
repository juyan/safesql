package safesql

import anorm.NamedParameter

/**
  * A DB predicate.
  *
  * @param columnName The name of the column.
  * @param variable The variable associated with the column, which is a tuple of variable name and value.
  * @param relation The mathematical relation between the column and the variable.
  */
class DBPredicate (
  val columnName: String,
  val variable: DBParameter,
  val relation: DBPredicateRelation.Value
) {

  def toStatement(generator: SequenceGenerator[String]): (String, NamedParameter) = {
    val variableName = generator.getNameAndSequenceForKey(columnName)
    val namedParameter = NamedParameter(variableName, variable.DBValue)
    if (relation == DBPredicateRelation.IN_SEQUENCE) {
      (s"$columnName ${relation.toString} ({$variableName})", namedParameter)
    } else {
      (s"$columnName ${relation.toString} {$variableName}", namedParameter)
    }
  }
}

object DBPredicate {
  def apply(columnName: String, variable: DBParameter, relation: DBPredicateRelation.Value) = {
    new DBPredicate(
      columnName = columnName,
      variable = variable,
      relation = relation
    )
  }
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

