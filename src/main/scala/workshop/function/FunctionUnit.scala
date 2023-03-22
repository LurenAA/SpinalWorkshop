package workshop.function

import spinal.core._
import spinal.lib._


case class FunctionUnit() extends Component{
  val io = new Bundle{
    val cmd    = slave Flow(Bits(8 bits))
    val valueA = out Bits(8 bits)
    val valueB = out Bits(32 bits)
    val valueC = out Bits(48 bits)
  }
  
  def patternDetector(str : String) = new Area{
    val hit = False
    // TODO
    val counter = Reg(UInt(log2Up(9) bits)) init(0)
    val passCouter = Reg(UInt(3 bits)) init(0)

    when(io.cmd.valid && passCouter === 0) {
      when(io.cmd.payload === B"8'x73") {
        counter := 1
      }.elsewhen(counter < 8) {
        counter := counter + 1
      }.otherwise{
        counter := 0
      }
    }.elsewhen(io.cmd.valid && passCouter > 0) {
      passCouter := passCouter - 1
      counter := 0
    }

    when(counter === 8 && str(8).toInt === io.cmd.payload) {
      hit := True
      if(str(8) == 'A') {
        passCouter := 1
      } else if(str(8) == 'B') {
        passCouter := 4
      } else if(str(8) == 'C') {
        passCouter := 6
      }
    }.otherwise{
      hit := False
    }
  }

  def valueLoader(start : Bool,that : Bits)= new Area{
    require(widthOf(that) % widthOf(io.cmd.payload) == 0) //You can make the assumption that the 'that' width is always an mulitple of 8
    // TODO
    val ncount = U(widthOf(that) / 8)
    val counter = Reg(UInt(ncount.getWidth bits)) init(0)
    val outthat = Reg(that) init(0)
    that := outthat
    when(start) {
      counter := ncount.resized
    }.elsewhen(counter > 0 && io.cmd.valid) {
      counter := counter - 1
      outthat((ncount - counter) * 8, 8 bits) := io.cmd.payload
    }

  }

  val setA    = patternDetector("setValueA")
  val loadA   = valueLoader(setA.hit,io.valueA)

  val setB    = patternDetector("setValueB")
  val loadB   = valueLoader(setB.hit,io.valueB)

  val setC    = patternDetector("setValueC")
  val loadC   = valueLoader(setC.hit,io.valueC)
}

