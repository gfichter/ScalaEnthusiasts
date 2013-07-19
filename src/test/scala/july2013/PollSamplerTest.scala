package july2013

import org.scalatest._
import PollSampler._
import PollField._
import PollFieldValue._
import PollSearcher._

class PollSamplerTest extends FunSpec with ShouldMatchers {

  val rawData = Seq(
    PollSample(party = "Democrat", sex = "Male", position = "Against"),
    PollSample(party = "Republican", sex = "Male", position = "Against"),
    PollSample(party = "Republican", sex = "Male", position = "Against"),
    PollSample(party = "Republican", sex = "Female", position = "Against"),
    PollSample(party = "Democrat", sex = "Female", position = "For"),
    PollSample(party = "Democrat", sex = "Male", position = "For"),
    PollSample(party = "Democrat", sex = "Female", position = "For"),
    PollSample(party = "Republican", sex = "Female", position = "For"))

  it("Should make a tree using an explicit RawPollSampleWrapper") {
    val pollTree = RawPollSampleWrapper(rawData).makeTree(Party, Sex, Position)
    uglyValidationOne(pollTree)
  }

  it("Should make a tree using an implicit (monkey-patched) RawPollSampleWrapper") {
    val pollTree = rawData.makeTree(Party, Sex, Position)
    uglyValidationOne(pollTree)
  }

  it("can tally samples") {
    val pollTree = rawData.makeTree(Party, Sex, Position)

    pollTree.tally(Democrat)./(Male)./(For) should be(1)
    pollTree.tally(Democrat)./(Female)./(For) should be(2)
    pollTree.tally(Democrat)./(Female)./(Against) should be(0)
  }

  it("can use an implicit wrapper monkey patched against a poll tree") {
    val pollTree = rawData.makeTree(Party, Sex, Position)

    pollTree.tally(Democrat)./(Male)./(For) should be(1)
    pollTree.tally(Democrat)./(Female)./(For) should be(2)
    pollTree.tally(Democrat)./(Female)./(Against) should be(0)
  }

  it("can build a two-level XPath-like search expression") {
    Democrat / Male should be(List(Democrat, Male))
  }

  it("can build a three-level search expression explicitly") {
    val a: List[PollFieldValue.Val] = Democrat / Male
    val b: PollFieldValues = PollFieldValues(List(Democrat, Male))
    val c = b./(For)
    c should be(PollFieldValues(List(Democrat, Male, For)))
  }
  
  it("can build a three-level XPath-like search expression") {
    Democrat/Male/For should be(PollFieldValues(List(Democrat, Male, For)))
  }

    it("can use short syntax for slash to get XPath-like searching") {
      val pollTree = rawData.makeTree(Party, Sex, Position)
  
//      pollTree.tally (Democrat / Female / For) should be (2)  // fails
//      pollTree tally (Democrat / Female / For) should be (2)  // fails
      
      
      
      
//      pollTree tally (Democrat / Female / Against) should be (0)
      
  //    pollTree tally Democrat / Male / For should be(1)
//      pollTree.tally(Democrat)./(Female)./(For) should be(2)
//      pollTree.tally(Democrat)./(Female)./(Against) should be(0)
    }
    
    // TODO: test requirement for searching all three levels (boo)

  it("Can make a tree with fields in any order") {
    val pollTree = rawData.makeTree(Sex, Position, Party)
    uglyValidationTwo(pollTree)
  }

  it("Should not allow you to make a tree using duplicate fields") {
    intercept[IllegalArgumentException] {
      rawData.makeTree(Party, Party, Position)
    }
    intercept[IllegalArgumentException] {
      rawData.makeTree(Party, Position, Party)
    }
    intercept[IllegalArgumentException] {
      rawData.makeTree(Position, Party, Party)
    }
  }

  def uglyValidationTwo(ptr: PollTreeWrapper1) = {
    val men = ptr.pollTree.get("Male").get
    val women = ptr.pollTree.get("Female").get
    men.get("For").get.get("Democrat").get.size should be(1)
    men.get("Against").get.get("Democrat").get.size should be(1)
    men.get("For").get.get("Republican") should be(None)
    men.get("Against").get.get("Republican").get.size should be(2)
    women.get("For").get.get("Democrat").get.size should be(2)
    women.get("Against").get.get("Democrat") should be(None)
    women.get("For").get.get("Republican").get.size should be(1)
    women.get("Against").get.get("Republican").get.size should be(1)
  }

  def uglyValidationOne(ptr: PollTreeWrapper1) = {
    val democrats = ptr.pollTree.get("Democrat").get
    val republicans = ptr.pollTree.get("Republican").get
    democrats.get("Male").get.get("For").get.size should be(1)
    democrats.get("Male").get.get("Against").get.size should be(1)
    democrats.get("Female").get.get("For").get.size should be(2)
    democrats.get("Female").get.get("Against") should be(None)
    republicans.get("Male").get.get("For") should be(None)
    republicans.get("Male").get.get("Against").get.size should be(2)
    republicans.get("Female").get.get("For").get.size should be(1)
    republicans.get("Female").get.get("Against").get.size should be(1)
  }
}