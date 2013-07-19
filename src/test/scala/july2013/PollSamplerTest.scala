package july2013

import org.scalatest._
import PollSampler._
import PollField._

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

  it("Should make a tree using explicit RawPollSampleWrapper") {
    val pollTree = RawPollSampleWrapper(rawData).makeTree(Party, Sex, Position)
    uglyValidationOne(pollTree)
  }

  it("Should make a tree using an implicit (monkey-patched) RawPollSampleWrapper") {
    val pollTree = rawData.makeTree(Party, Sex, Position)
    uglyValidationOne(pollTree)
  }

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

  def uglyValidationTwo(pollTree: Map[String, Map[String, Map[String, Seq[PollSample]]]]) = {
    val men = pollTree.get("Male").get
    val women = pollTree.get("Female").get
    men.get("For").get.get("Democrat").get.size should be(1)
    men.get("Against").get.get("Democrat").get.size should be(1)
    men.get("For").get.get("Republican") should be(None)
    men.get("Against").get.get("Republican").get.size should be(2)
    women.get("For").get.get("Democrat").get.size should be(2)
    women.get("Against").get.get("Democrat") should be(None)
    women.get("For").get.get("Republican").get.size should be(1)
    women.get("Against").get.get("Republican").get.size should be(1)
  }

  def uglyValidationOne(pollTree: Map[String, Map[String, Map[String, Seq[PollSample]]]]) = {
    val democrats = pollTree.get("Democrat").get
    val republicans = pollTree.get("Republican").get
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