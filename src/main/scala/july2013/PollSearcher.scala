package july2013

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

case class PollTreeWrapper1(pollTree: Map[String, Map[String, Map[String, Seq[PollSample]]]]) {
  
  // TODO: Do I need this?
  def tally(fieldValue: PollFieldValue.Val) = PollTreeWrapper2(pollTree.get(fieldValue.toString))
  
  def newTally(fieldValues: PollFieldValues): Int = {
    val fields = fieldValues.fieldValues
    require(fields.length == 3)  // TODO:  Would like to relax this constraint
    val a = pollTree.get(fields(0).toString) match {
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
    
    0 // stub for now
  }
  
}

case class PollTreeWrapper2(pollTree: Option[Map[String, Map[String, Seq[PollSample]]]]) {
  def /(fieldValue: PollFieldValue.Val) = PollTreeWrapper3(pollTree.get(fieldValue.toString))
}

case class PollTreeWrapper3(pollTree: Map[String, Seq[PollSample]]) {
  def /(fieldValue: PollFieldValue.Val) = pollTree.get(fieldValue.toString) match {
    case Some(x) => x size
    case None => 0
  }
}