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
    val pwm = out Bool() //TODO
  }
  var enableInput = Bool() 
  var enableEna = Bool() 
  val dutyCycleInput = Bits(timerWidth bits) 
  val dutyCycleEna = Bool() 
  val enable = Reg(Bool()) init(False)
  val dutyCycle = Reg(UInt(timerWidth bits)) init(0)
  val timer = Reg(UInt(timerWidth bits)) init(0)
  val outputPWM = Reg(Bool()) init(False)

  val logic = new Area {
    //TODO define the PWM logic
    
    when(enableEna) {
      enable := enableInput
    }

    when(dutyCycleEna) {
      dutyCycle := U(dutyCycleInput)
    }
    
    when(enable){
      timer := timer + 1
    }
    
    outputPWM := timer < dutyCycle
    io.pwm := outputPWM
  }
  
  val control = new Area{
    //TODO define the APB slave logic that will make PWM's registers writable/readable
    
    io.apb.PREADY :=  True
    when(io.apb.PSEL(0) && io.apb.PENABLE && io.apb.PWRITE) {
      enableEna := io.apb.PADDR === U(0)
    } otherwise  {
      enableEna := False;
    }
    enableInput :=  io.apb.PWDATA(0) && enableEna
    dutyCycleEna := io.apb.PADDR === U(4) && io.apb.PSEL(0) && io.apb.PENABLE && io.apb.PWRITE 
    dutyCycleInput := B(io.apb.PWDATA, timerWidth bits) & B(timerWidth bits, default -> dutyCycleEna)

    //io.apb.PRDATA := ~io.apb.PSEL(0) ? B(0) | (io.apb.PADDR === U(4) ? B(dutyCycle, apbConfig.dataWidth bits)| (io.apb.PADDR === U(0) ? B(enable, apbConfig.dataWidth bits) | B(0)));
    val valb = Bits(apbConfig.dataWidth bits)
    val vala = Bits(apbConfig.dataWidth bits)
    
    io.apb.PRDATA := ~io.apb.PSEL(0) ? B(0) | vala;
    valb := Mux(io.apb.PADDR === U(0), B(enable, apbConfig.dataWidth bits), B(0)) 
    vala := Mux(io.apb.PADDR === U(4), B(dutyCycle, apbConfig.dataWidth bits),valb)
  }
}