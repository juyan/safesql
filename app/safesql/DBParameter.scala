package safesql

import anorm.{ParameterValue, ToStatement}

/**
  * Created by junyan on 4/2/16.
  */
class DBParameter(val value: Any, val DBValue: ParameterValue) {
}

object DBParameter {
  def apply(value: Any, dbValue: ParameterValue): DBParameter = new DBParameter(value, dbValue)

  implicit class LongToDBLong(val long: Long) {
    def toDBParameter: DBParameter = {
      DBParameter(long, ParameterValue.toParameterValue(long))
    }

    def toParameterValue: ParameterValue = {
      ParameterValue.toParameterValue(long)
    }
  }

  implicit class StringToDBString(val string: String) {
    def toDBParameter: DBParameter = {
      DBParameter(string, ParameterValue.toParameterValue(string))
    }

    def toParameterValue: ParameterValue = {
      ParameterValue.toParameterValue(string)
    }
  }

  implicit class SeqToDBSeq[T](val seq: Seq[T]) {
    def toDBParameter(implicit toStatement: ToStatement[T]): DBParameter = {
      DBParameter(seq, ParameterValue.toParameterValue(seq))
    }
  }
}

