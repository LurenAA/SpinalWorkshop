package workshop.pwm

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib._

//APB configuration class (generic/parameter)
case class ApbConfig(addressWidth : Int,
                     dataWidth    : Int,
                     selWidth     : Int)

//APB interface definition
case class Apb(config: ApbConfig) extends Bundle with IMasterSlave {
  //TODO define APB signals
  val PADDR = UInt(config.addressWidth bits)
  val PSEL = Bits(config.selWidth bits)
  val PENABLE = Bool()
  val PREADY = Bool()
  val PWRITE = Bool()
  val PWDATA = Bits(config.dataWidth bits)
  val PRDATA = Bits(config.dataWidth bits)

  override def asMaster(): Unit = {
    //TODO define direction of each signal in a master mode
    out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
    in(PREADY,PRDATA)
  }
}

case class ApbPwm(apbConfig: ApbConfig,timerWidth : Int) extends Component{
  require(apbConfig.dataWidth == 32)
  require(apbConfig.selWidth == 1)

  val io = new Bundle{
    val apb = slave(Apb(apbConfig)) //TODO
    val pwm = out(Bool) //TODO
  }

  io.apb.PREADY := True

  val logic = new Area {
    //TODO define the PWM logic
    val enableReg = Reg(Bool) init(False)
    val dutyCycleReg = Reg(UInt(timerWidth bits))
    val timerReg = Reg(UInt(timerWidth bits)) init(0)
    val outputReg = Reg(Bool) init(False)
    when(enableReg) {
      timerReg := timerReg + 1
    }

    when(timerReg < dutyCycleReg) {
      outputReg := True
    }.otherwise {
      outputReg := False
    }
  }
  io.pwm := logic.outputReg

  val control = new Area{
    //TODO define the APB slave logic that will make PWM's registers writable/readable
    io.apb.PRDATA := 0
    when(io.apb.PENABLE &&
      io.apb.PSEL(0) 
    ) {
      when(io.apb.PADDR === 0) {
        when( io.apb.PWRITE) {
          logic.enableReg := io.apb.PWDATA(0)
        }.elsewhen(~io.apb.PWRITE) {
          io.apb.PRDATA := logic.enableReg.asBits.resized
        }
      }.elsewhen(io.apb.PADDR === 4) {
        when(io.apb.PWRITE) {
          logic.dutyCycleReg := io.apb.PWDATA.asUInt.resized
        }.otherwise{
          io.apb.PRDATA := logic.dutyCycleReg.asBits.resized
        }
      }
      
    }


  }
}