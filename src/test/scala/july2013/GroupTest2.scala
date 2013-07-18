package july2013

import org.scalatest._

case class PollSample(
  party: String,
  sex: String,
  position: String)

class GroupTest2 extends FunSpec with ShouldMatchers {

  val rawData = Seq(
    PollSample(party = "Democrat", sex = "Male", position = "Against"),
    PollSample(party = "Republican", sex = "Male", position = "Against"),
    PollSample(party = "Republican", sex = "Male", position = "Against"),
    PollSample(party = "Republican", sex = "Female", position = "Against"),
    PollSample(party = "Democrat", sex = "Female", position = "For"),
    PollSample(party = "Democrat", sex = "Male", position = "For"),
    PollSample(party = "Democrat", sex = "Female", position = "For"),
    PollSample(party = "Republican", sex = "Female", position = "For"))
/*
  class PollSeq extends Seq[ PollSample ]{

    def byProperty( s : Symbol ): Map[ String, PollSeq ] 
    	= s match {
      case 'party => groupBy(_.party)
      case 'sex => groupBy(_.sex)
      case 'position => groupBy(_.position)
    }
    
  }   
  */  
  describe("group tests") {

    it("should group an unordered list") {
      val byPartyAndPosition = rawData.groupBy(sample => sample.party + ' ' + sample.position)
      byPartyAndPosition.get("Democrat For").get.size should be(3)
      byPartyAndPosition.get("Republican For").get.size should be(1)
      byPartyAndPosition.get("Democrat Against").get.size should be(1)
      byPartyAndPosition.get("Republican Against").get.size should be(3)
    }

    def byProperty(pols:Seq[PollSample], s: Symbol): Map[String, Seq[PollSample]] = s match {
      case 'party => pols.groupBy(_.party)
      case 'sex => pols.groupBy(_.sex)
      case 'position => pols.groupBy(_.position)
    }

    describe("first level group tests") {

      val byParty: Map[String, Seq[PollSample]] = byProperty(rawData, 'party)
      val bySex: Map[String, Seq[PollSample]] = byProperty(rawData, 'sex)
      val byPosition: Map[String, Seq[PollSample]] = byProperty(rawData, 'position)

      it("has 4 dems") {
        byParty.get("Democrat").get.size should be(4)
      }

      it("has 4 repubs") {
        byParty.get("Republican").get.size should be(4)
      }

      it("has 4 men") {
        bySex.get("Male").get.size should be(4)
      }

      it("has 4 women") {
        bySex.get("Female").get.size should be(4)
      }

      it("has 4 For") {
        byPosition.get("For").get.size should be(4)
      }

      it("has 4 Against") {
        byPosition.get("Against").get.size should be(4)
      }

    }

    describe("two level test") {
      val positionSex = byProperty(rawData, 'position).mapValues {
        byProperty(_, 'sex)
      }
      val positionParty = byProperty(rawData, 'position).mapValues {
        byProperty(_, 'party)
      }
      
      it("3 women for") {
        val a = positionSex.get("For").get
        val b = a.get("Female")
        b.get.size should be(3)
      }
      it("1 man for") {
        val a = positionSex.get("For").get
        val b = a.get("Male")
        b.get.size should be(1)
      }
      it("1 repub for") {
        val a = positionParty.get("For").get
        val b = a.get("Republican")
        b.get.size should be(1)
      }
      it("3 dems for") {
        val a = positionParty.get("For").get
        val b = a.get("Democrat")
        b.get.size should be(3)
      }
      
    }

  }
}
  
//  it("can make a tree") {
//    val pollTree =
//      rawData.groupBy(_.party)
//        .mapValues(_.groupBy(_.sex)
//          .mapValues(_.groupBy(_.position)))
//    val democrats = pollTree.get("Democrat").get
//    val republicans = pollTree.get("Republican").get
//
//    describe("first level group") {
//      
//    it("has the right number of dems and repubs") {
//      democrats.size should be(4)
//      republicans.size should be(4)
//    }
//    }
//    
//    democrats.get("Male").get.get("For").get.size should be(1)
//    democrats.get("Male").get.get("Against").get.size should be(1)
//    democrats.get("Female").get.get("For").get.size should be(2)
//    democrats.get("Female").get.get("Against") should be(None)
//    republicans.get("Male").get.get("For") should be(None)
//    republicans.get("Male").get.get("Against").get.size should be(2)
//    republicans.get("Female").get.get("For").get.size should be(1)
//    republicans.get("Female").get.get("Against").get.size should be(1)
//  }

  
//}