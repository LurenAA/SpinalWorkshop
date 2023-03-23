package workshop.apb3decoder

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3Config, Apb3}


case class Mapping(base : BigInt, range : BigInt){
  // Return true when the given UInt is in the mapping range
  // Very simple and unoptimised implementation
  def hit(address : UInt) : Bool = address >= base && address < base + range
}

//Example of instantiation :
//new Apb3Decoder(
//  apbConfig = Apb3Config(
//    addressWidth = 16,
//    dataWidth = 32
//  ),
//  outputsMapping = List(
//    Mapping(base = 0x0000, range = 0x1000),
//    Mapping(base = 0x1000, range = 0x1000),
//    Mapping(base = 0x4000, range = 0x2000),
//    Mapping(base = 0x6000, range = 0x3000)
//  )
//)
case class Apb3Decoder(apbConfig : Apb3Config, outputsMapping : Seq[Mapping]) extends Component{
  require(apbConfig.selWidth == 1)

  val io = new Bundle{
    val input   = slave(Apb3(apbConfig))
    val outputs = Vec(master(Apb3(apbConfig)), outputsMapping.length)
  }
  // io.input.PREADY := True
  // io.input.PSLVERROR := False
  // io.input.PRDATA := 0
  // io.input.PRDATA := io.outputs(0).PRDATA
  // val hitn = UInt(outputsMapping.length bits)
  for(idx <- 0 until outputsMapping.length) {
    io.outputs(idx).PADDR := io.input.PADDR
    io.outputs(idx).PENABLE := io.input.PENABLE
    io.outputs(idx).PWDATA := io.input.PWDATA
    io.outputs(idx).PWRITE := io.input.PWRITE

    when(outputsMapping(idx).hit(io.input.PADDR)) {
      io.outputs(idx).PSEL := io.input.PSEL
      // hitn(idx) := True
    }.otherwise{
      io.outputs(idx).PSEL := 0
      // hitn(idx) := False
    }
  }
  
  when(outputsMapping(0).hit(io.input.PADDR)) {
      io.input.PRDATA := io.outputs(0).PRDATA
      io.input.PREADY := io.outputs(0).PREADY
      io.input.PSLVERROR := io.outputs(0).PSLVERROR
  }.elsewhen(outputsMapping(1).hit(io.input.PADDR)) {
      io.input.PRDATA := io.outputs(1).PRDATA
      io.input.PREADY := io.outputs(1).PREADY
      io.input.PSLVERROR := io.outputs(1).PSLVERROR
  }.elsewhen(outputsMapping(2).hit(io.input.PADDR)) {
      io.input.PRDATA := io.outputs(2).PRDATA
      io.input.PREADY := io.outputs(2).PREADY
      io.input.PSLVERROR := io.outputs(2).PSLVERROR
  }.elsewhen(outputsMapping(3).hit(io.input.PADDR)) {
      io.input.PRDATA := io.outputs(3).PRDATA
      io.input.PREADY := io.outputs(3).PREADY
      io.input.PSLVERROR := io.outputs(3).PSLVERROR
  }.otherwise{
    io.input.PRDATA := io.outputs(0).PRDATA
    io.input.PREADY := io.outputs(0).PREADY
    io.input.PSLVERROR := io.outputs(0).PSLVERROR
  }
  //TODO fully asynchronous apb3 decoder
}