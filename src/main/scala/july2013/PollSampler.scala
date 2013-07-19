package july2013

case class RawPollSampleWrapper(raw: Seq[PollSample]) {
  import PollField._
  def makeTree(field1: PollField.Value, field2: PollField.Value, field3: PollField.Value): PollSearcher = {
    require(field1 != field2)
    require(field1 != field3)
    require(field2 != field3)
    PollSearcher(
      raw.groupBy(fieldValForSample(field1, _))
        .mapValues(_.groupBy(fieldValForSample(field2, _))
          .mapValues(_.groupBy(fieldValForSample(field3, _)))))
  }
}

object PollSampler {
  implicit def RawPollDataToRawPollSampleWrapper(raw: Seq[PollSample]) = RawPollSampleWrapper(raw)
}
