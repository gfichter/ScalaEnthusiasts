package july2013

object PollField extends Enumeration {
  type PollField = Value
  val Party, Sex, Position = Value

  def fieldValForSample(field: Value, sample: PollSample): String = {
    field match {
      case Party => sample.party
      case Sex => sample.sex
      case Position => sample.position
    }
  }
}