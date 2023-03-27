package workshop.mandelbrot

import spinal.core._
import spinal.lib._


case class Dispatcher[T <: Data](dataType : T,outputsCount : Int) extends Component{
  val io = new Bundle {
    val input = slave Stream(dataType)
    val outputs = Vec(master Stream(dataType),outputsCount)
  }

  // TODO
  // val outputsPayloadReg = Reg(Vec(dataType, outputsCount))
  // val outputsValidReg = Reg(Vec(Bool, outputsCount))
  // for(idx <- 0 until outputsCount) {
  //   io.outputs(idx).valid := outputsValidReg(idx)
  //   // io.outputs(idx).payloadval counter = Reg(UInt(log2Up(outputsCount) bits)) init(0).assignFromBits(outputsPayloadReg(idx).asBits) 
  // }
  // // HIERARCHY VIOLATION : (toplevel/outputsPayloadReg_0_y :  SInt[28 bits]) is driven by (toplevel/dispa/??? :  SInt[28 bits]), but isn't accessible in the dispa component.
  // //   workshop.mandelbrot.Dispatcher$$anonfun$2.apply$mcV$sp(PixelSolverMultiCore.scala:31)
  // //   workshop.mandelbrot.Dispatcher.<init>(PixelSolverMultiCore.scala:24)
  // //   workshop.mandelbrot.PixelSolverMultiCore.<init>(PixelSolverMultiCore.scala:47)
  // //   workshop.mandelbrot.PixelSolverMultiCoreMain$$anonfun$main$1.apply(PixelSolverMultiCoreMain.scala:9)
  // //   workshop.mandelbrot.PixelSolverMultiCoreMain$$anonfun$main$1.apply(PixelSolverMultiCoreMain.scala:9)
  // //   spinal.sim.JvmThread.run(SimManager.scala:51)
  
  val counter = Reg(UInt(log2Up(outputsCount) bits)) init(0)
  io.input.ready := io.outputs(counter).ready

  for(idx <- 0 until outputsCount) {
    io.outputs(idx).payload := io.input.payload
    io.outputs(idx).valid := False

    when(counter === idx) {
      io.outputs(idx).valid := io.input.valid

      when(io.outputs(idx).fire) {
        when(counter === outputsCount - 1) {
          counter := 0
        }.otherwise{
          counter := counter + 1
        }
      }
    } 
  }
}

// TODO Define the Arbiter component (similar to the Dispatcher)

case class PixelSolverMultiCore(g : PixelSolverGenerics,coreCount : Int) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (PixelTask(g))
    val rsp = master Stream (PixelResult(g))
  }

  val counter = Reg(UInt(log2Up(coreCount) bits)) init(0)
  val dispa = Dispatcher(PixelTask(g), coreCount)
  dispa.io.input << io.cmd

  val pixelSolverArray = for (i <- 0 until coreCount) yield PixelSolver(g)
  io.rsp.payload := pixelSolverArray(0).io.rsp.payload
  io.rsp.valid := False

  for(idx <- 0 until coreCount) {
    pixelSolverArray(idx).io.cmd << dispa.io.outputs(idx)

    pixelSolverArray(idx).io.rsp.ready := False
    when(counter === idx) {
      pixelSolverArray(idx).io.rsp.ready := io.rsp.ready
      io.rsp.payload := pixelSolverArray(idx).io.rsp.payload
      io.rsp.valid := pixelSolverArray(idx).io.rsp.valid
      
      when(counter === coreCount - 1) {
        counter := 0
      }.otherwise{
        counter := counter + 1
      }
    }
 
  //   when(counter === idx) {
  //     when(pixelSolverArray(idx).io.rsp.valid && !io.rsp.valid) {
  //       payloadReg := pixelSolverArray(idx).io.rsp.payload
  //       validReg := True
  //     }.elsewhen(io.rsp.ready && io.rsp.valid) {
  //       validReg := False

  //       when(counter === coreCount - 1) {
  //         counter := 0
  //       }.otherwise{
  //         counter := counter + 1
  //       }
  //     }
  //   }

  }

  //TODO instantiate all components
  //TODO interconnect all that stuff
}

