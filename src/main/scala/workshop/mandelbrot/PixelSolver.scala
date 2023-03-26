package workshop.mandelbrot

import spinal.core._
import spinal.lib._

case class PixelSolverGenerics(fixAmplitude : Int,
                               fixResolution : Int,
                               iterationLimit : Int){
  val iterationWidth = log2Up(iterationLimit+1)
  def iterationType = UInt(iterationWidth bits)
  def fixType = SFix(
    peak = fixAmplitude exp,
    resolution = fixResolution exp
  )
}

case class PixelTask(g : PixelSolverGenerics) extends Bundle{
  val x,y = g.fixType
}

case class PixelResult(g : PixelSolverGenerics) extends Bundle{
  val iteration = g.iterationType
}

case class PixelSolver(g : PixelSolverGenerics) extends Component{
  val io = new Bundle{
    val cmd = slave  Stream(PixelTask(g))
    val rsp = master Stream(PixelResult(g))
  }

  val x = Reg(g.fixType) 
  val y = Reg(g.fixType)
  val x0 = Reg(g.fixType)
  val y0 = Reg(g.fixType)
  val iteration = Reg(g.iterationType) 
  val two = Reg(g.fixType).allowUnsetRegToAvoidLatch 
  two := 2

  val start = Reg(Bool) init(False)
  val validrsp = Reg(Bool) init(False)
  io.rsp.valid := validrsp
  
  when(start && x*x + y*y < 4 && iteration < U(255).resized) {
    x := (x*x - y*y + x0).truncated
    y := (x*y*two + y0).truncated
    iteration := iteration + 1
  }.elsewhen(start) {
    start := False
    validrsp := True
  }.otherwise{
    start := False
  }


  io.cmd.ready := (!io.rsp.valid | io.rsp.ready) && !start
  when(io.cmd.fire) {
    start := True
    x := 0.0
    y := 0.0
    iteration := 0
    x0 := io.cmd.payload.x
    y0 := io.cmd.payload.y
  }

  io.rsp.payload.iteration := iteration
  
  when(io.rsp.fire) {
    validrsp := False
  }
}
