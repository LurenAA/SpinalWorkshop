package workshop.stream

import spinal.core._
import spinal.lib._


case class MemoryWrite() extends Bundle{
  val address = UInt(8 bits)
  val data    = Bits(32 bits)
}

case class StreamUnit() extends Component{
  val io = new Bundle{
    val memWrite = slave  Flow(MemoryWrite())
    val cmdA     = slave  Stream(UInt(8 bits))
    val cmdB     = slave  Stream(Bits(32 bits))
    val rsp      = master Stream(Bits(32 bits))
  }
  
  val mem = Mem(Bits(32 bits),1 << 8)
  // TODO
  mem.write(io.memWrite.address, io.memWrite.data)
  val memread = mem.streamReadSync(io.cmdA)

  val joinedStreamWithoutPayload = StreamJoin.arg(io.cmdB,memread)
  io.rsp <> joinedStreamWithoutPayload.translateWith(io.cmdB.payload ^ memread.payload)
}
