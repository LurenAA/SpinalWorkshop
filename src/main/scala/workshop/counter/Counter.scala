package workshop.counter

import spinal.core._

case class Counter(width: Int) extends Component {
  val io = new Bundle {
    val clear    = in  Bool()
    val value    = out UInt(width bits)
    val full     = out Bool()
  }

  //TODO define the logic
  io.full := io.value === (1 << width) - 1
  val register = Reg(UInt(width bits)) init 0
  io.value := register
  register := Mux(io.clear, U(0), register + 1)
}
