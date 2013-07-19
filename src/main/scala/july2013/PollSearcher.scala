package july2013

case class PollSearcher(pollTree: Map[String, Map[String, Map[String, Seq[PollSample]]]]) {
  
  def tally(fieldValues: PollFieldValues): Int = {
    val fields = fieldValues.fieldValues
    require(fields.length == 3)  // TODO:  Would like to relax this constraint
    return pollTree.get(fields(0).toString) match {
      case None => 0
      case Some(levelOneResults) => {
        levelOneResults.get(fields(1).toString) match {
          case None => 0
          case Some(levelTwoResults) => {
            levelTwoResults.get(fields(2).toString) match {
              case None => 0
              case Some(levelThreeResults) => levelThreeResults.size
            }
          }
        }
      }
    }
  }
}

object PollSearcher {
	implicit def PollFieldValueListToPollFieldValues(fieldValues: List[PollFieldValue.Val]) = 
			PollFieldValues(fieldValues) 
}

object PollFieldValue extends Enumeration {
   case class Val(name: String) extends super.Val(nextId, name) {
     def / (nextField: Val) = List(this, nextField)
   }
   implicit def valueToPollFieldValue(x: Value) = x.asInstanceOf[Val] 
  
   val Democrat = Val("Democrat")
   val Republican = Val("Republican")
   val Male = Val("Male")
   val Female = Val("Female")
   val For = Val("For")
   val Against = Val("Against")
}

case class PollFieldValues(fieldValues: List[PollFieldValue.Val]) {
     def / (nextField: PollFieldValue.Val) = PollFieldValues((nextField :: fieldValues.reverse).reverse)  
}
