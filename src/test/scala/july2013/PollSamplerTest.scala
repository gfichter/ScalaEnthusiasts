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

  it("Can convert raw data to a tree and search using XPath-like syntax") {
    val pollTree = rawData.makeTree(Party, Sex, Position)

    pollTree tally Democrat/Male/For should be(1)
    pollTree tally Democrat/Male/Against should be(1)
    pollTree tally Democrat/Female/For should be(2)
    pollTree tally Democrat/Female/Against should be(0)
    pollTree tally Republican/Male/For should be(0)
    pollTree tally Republican/Male/Against should be(2)
    pollTree tally Republican/Female/For should be(1)
    pollTree tally Republican/Female/Against should be(1)
  }

  it("Can make a tree with fields in any order") {
    val pollTree = rawData.makeTree(Sex, Position, Party)

    pollTree tally Male/For/Democrat should be(1)
    pollTree tally Male/Against/Democrat should be(1)
    pollTree tally Male/For/Republican should be(0)
    pollTree tally Male/Against/Republican should be(2)
    pollTree tally Female/For/Democrat should be(2)
    pollTree tally Female/Against/Democrat should be(0)
    pollTree tally Female/For/Republican should be(1)
    pollTree tally Female/Against/Republican should be(1)
  }

  it("requires you search all three levels (boo)") {
    val pollTree = rawData.makeTree(Party, Sex, Position)

    // TODO:  should not throw
    intercept[IllegalArgumentException] {
      pollTree.tally(Democrat / Female)
    }

    // TODO:  should not be a compiler error
    // pollTree.tally(Democrat) <= type mismatch; found : july2013.PollFieldValue.Val required: july2013.PollFieldValues
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
}