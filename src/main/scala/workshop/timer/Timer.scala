package workshop.timer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class Timer(width : Int) extends Component {
  val io = new Bundle {
    val tick = in Bool()
    val clear = in Bool()
    val limit = in UInt (width bits)

    val full  = out Bool()
    val value = out UInt (width bits)
  }

  //TODO phase 1
  val ck = Reg(UInt(width bits)) init(0)
  when(io.clear) {
    ck := 0
  }.elsewhen(io.tick && ck =/= io.limit) {
    ck := ck + 1
  }

  io.full := ck === io.limit
  io.value := ck

  def driveFrom(busCtrl : BusSlaveFactory,baseAddress : BigInt)(ticks : Seq[Bool],clears : Seq[Bool]) = new Area {
    //TODO phase 2

    val ticksEnable = busCtrl.createReadAndWrite(Bits(ticks.length bits), baseAddress, 0) init(0)
    // val tickIdx = OHToUInt(ticksEnable)
    io.tick := (ticksEnable & ticks.asBits).orR

    val clearsEnable = busCtrl.createReadAndWrite(Bits(clears.length bits),baseAddress, 16) init(0)
    // val clearIdx = OHToUInt(clearsEnable)

    when(busCtrl.isWriting(baseAddress + 8) || busCtrl.isWriting(baseAddress + 4)) {
      io.clear := True
    }.otherwise{
      // io.clear := clears(clearIdx) 
      io.clear := (clearsEnable & clears.asBits).orR
    }
    
    busCtrl.driveAndRead(io.limit, baseAddress + 4, 0)
    busCtrl.read(io.value, baseAddress + 8, 0)

  }
}
