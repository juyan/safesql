package safesql

import anorm.ParameterValue

/**
  * Created by junyan on 4/10/16.
  */

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