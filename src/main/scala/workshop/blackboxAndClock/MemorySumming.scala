// package workshop.blackboxAndClock

// import spinal.core._
// import spinal.lib._


// // Define a Ram as a BlackBox
// case class Ram_1w_1r_2c(wordWidth: Int, addressWidth: Int,writeClock : ClockDomain,readClock : ClockDomain) extends BlackBox {
//   // TODO define Generics
//   addGeneric("addressWidth", addressWidth)
//   addGeneric("wordWidth", wordWidth)
//   // TODO define IO
//   val io = new Bundle {
//     val wr = new Bundle {
//       val en   = in Bool()
//       val addr = in UInt (addressWidth bits)
//       val data = in Bits (wordWidth bits)
//       val clk = in Bool()
//     }
//     val rd = new Bundle {
//       val en   = in Bool()
//       val addr = in UInt (addressWidth bits)
//       val data = out Bits (wordWidth bits)
//        val clk = in Bool()
//     }
//   }

//   // TODO define ClockDomains mappings
//   mapClockDomain(writeClock, io.wr.clk)
//   mapClockDomain(readClock, io.rd.clk)
// }

// // Create the top level and instanciate the Ram
// case class MemorySumming(writeClock : ClockDomain,sumClock : ClockDomain) extends Component {
//   val io = new Bundle {
//     val wr = new Bundle {
//       val en   = in Bool()
//       val addr = in UInt (8 bits)
//       val data = in Bits (16 bits)
//     }

//     val sum = new Bundle{
//       val start = in Bool()
//       val done  = out Bool()
//       val value = out UInt(16 bits)
//     }
//   }

//   // TODO define the ram
//   val ram = Ram_1w_1r_2c(16, 8, writeClock, sumClock)
//   // TODO connect the io.wr port to the ram
//   ram.io.wr.en := io.wr.en
//   ram.io.wr.addr := io.wr.addr
//   ram.io.wr.data := io.wr.data

//   val sumArea = new ClockingArea(sumClock){
//     // TODO define the memory read + summing logic
//     val sum = Reg(UInt(16 bits)) init(0)
//     val start = Reg(Bool) init(False)
//     val counter = Reg(UInt (8 bits)) init(0)
//     val done = Reg(Bool) init(False)
//     val ff = Reg(Bool) init(False)
//     io.sum.value := sum
//     io.sum.done := done
//     when(io.sum.start) {
//       start := True
      
//     }
    
//     ram.io.rd.addr := counter
//     ram.io.rd.en := start

//     when(start) {
//       when(counter =/= U"hFF") {
//         when(counter =/= 0) {
//           sum := sum + ram.io.rd.data.asUInt
//         }
//         counter := counter + 1
//       }.elsewhen(ff === False){
//         sum := sum + ram.io.rd.data.asUInt
//         // done := True
//         // start := False
//         ff := True
//       }.elsewhen(ff === True){
//         sum := sum + ram.io.rd.data.asUInt
//         start := False
//         ff := False
//         done := True
//       }
//     } .otherwise{
//       done := False
//       counter := 0
//       sum := 0
//     }
//   }
// }
package workshop.blackboxAndClock

import spinal.core._
import spinal.lib._


// Define a Ram as a BlackBox
case class Ram_1w_1r_2c(wordWidth: Int, addressWidth: Int,writeClock : ClockDomain,readClock : ClockDomain) extends BlackBox {
  addGeneric("addressWidth", addressWidth)
  addGeneric("wordWidth", wordWidth)

  val io = new Bundle {
    val wr = new Bundle {
      val clk  = in Bool()
      val en   = in Bool()
      val addr = in UInt(addressWidth bits)
      val data = in Bits(wordWidth bits)
    }
    val rd = new Bundle {
      val clk  = in Bool()
      val en   = in Bool()
      val addr = in UInt(addressWidth bits)
      val data = out Bits(wordWidth bits)
    }
  }

  mapClockDomain(clockDomain = writeClock, clock=io.wr.clk)
  mapClockDomain(clockDomain = readClock , clock=io.rd.clk)
}

// Create the top level and instanciate the Ram
case class MemorySumming(writeClock : ClockDomain, sumClock : ClockDomain) extends Component {
  val io = new Bundle {
    val wr = new Bundle {
      val en   = in Bool()
      val addr = in UInt (8 bits)
      val data = in Bits (16 bits)
    }

    val sum = new Bundle{
      val start = in Bool()
      val done  = out Bool()
      val value = out UInt(16 bits)
    }
  }

  val ram = new Ram_1w_1r_2c(
    wordWidth = 16,
    addressWidth = 8,
    writeClock = writeClock,
    readClock  = sumClock
  )

  io.wr.en   <> ram.io.wr.en
  io.wr.addr <> ram.io.wr.addr
  io.wr.data <> ram.io.wr.data

  val sumArea = new ClockingArea(sumClock){
    val counter = Reg(UInt(8 bits))
    val active  = RegInit(False)

    ram.io.rd.en := active
    ram.io.rd.addr := counter

    when(!active){
      active := io.sum.start
      counter := 0
    }otherwise{
      counter := counter + 1
      when(counter === counter.maxValue){
        active := False
      }
    }

    val readDataValid = RegNext(active) init(False)
    val sum = Reg(UInt(16 bits))
    when(readDataValid){
      sum := sum + ram.io.rd.data.asUInt
    } otherwise{
      sum := 0
    }

    io.sum.done := False
    when(readDataValid.fall(initAt = False)){
      io.sum.done := True
    }
  }

  io.sum.value := sumArea.sum
}
