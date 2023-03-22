package workshop.uart

import spinal.core._
import spinal.lib._

case class UartRxGenerics( preSamplingSize: Int = 1,
                           samplingSize: Int = 5,
                           postSamplingSize: Int = 2){

  val rxdSamplePerBit = preSamplingSize + samplingSize + postSamplingSize
  require(isPow2(rxdSamplePerBit))

  if ((samplingSize % 2) == 0)
    SpinalWarning(s"It's not nice to have a even samplingSize value at ${ScalaLocated.short} (because of the majority vote)")
}

case class UartCtrlRx(generics : UartRxGenerics) extends Component{
  import generics._  //Allow to directly use generics attribute without generics. prefix
  val io = new Bundle{
    val rxd  = in Bool()
    val samplingTick = in Bool()
    val read = master Flow(Bits(8 bits))
  }

  // Implement the rxd sampling with a majority vote over samplingSize bits
  // Provide a new sampler.value each time sampler.tick is high
  val sampler = new Area {
    val samples = History(
      that  = io.rxd,
      range = 2 until 2+samplingSize,
      when  = io.samplingTick,
      init  = True
    )
    val value       = RegNext(MajorityVote(samples)) init(True)
    val tick        = RegNext(io.samplingTick) init(False)
  }

  // Provide a bitTimer.tick each rxSamplePerBit
  // reset() can be called to recenter the counter over a start bit.
  val bitTimer = new Area {
    val counter  = Reg(UInt(log2Up(rxdSamplePerBit) bit))
    val recenter = False
    val tick = False
    when(sampler.tick) {
      counter := counter - 1
      when(counter === 0) {
        tick := True
      }
    }
    when(recenter){
      counter := preSamplingSize + (samplingSize - 1) / 2 - 1
    }
  }

  // Provide bitCounter.value that count up each bitTimer.tick, Used by the state machine to count data bits and stop bits
  // reset() can be called to reset it to zero
  val bitCounter = new Area {
    val value = Reg(UInt(3 bits))
    val clear = False
    val storage = Reg(Bits(8 bit))

    when(bitTimer.tick) {
      value := value + 1
      storage(value) := io.rxd
    }
    when(clear){
      value := 0
    }
  }

  // Statemachine that use all precedent area
  val stateMachine = new Area {
    //TODO state machine
    object State extends SpinalEnum {
      val IDLE, START, DATA, STOP = newElement()
    }

    val stateNext = State()
    val state = Reg(State())

    switch(state) {
      is(State.IDLE) {
        stateNext := (sampler.tick && !sampler.value) ? State.START | State.IDLE
      }
      is(State.START) {
        stateNext := bitTimer.tick ? State.DATA | State.START
      }
      is(State.DATA) {
        stateNext := (bitTimer.tick && bitCounter.value === 7) ? State.STOP | State.DATA
      }
      is(State.STOP) {
        stateNext := bitTimer.tick ? State.IDLE | State.STOP
      }
    }

    state := stateNext
    when(state === State.IDLE && stateNext === State.START) {
      bitTimer.recenter :=  True
    }.otherwise {
      bitTimer.recenter :=  False
    }
    
    when(state === State.START && stateNext === State.DATA) {
      bitCounter.clear := True
    }.otherwise{
      bitCounter.clear := False
    }
    
    io.read.push(bitCounter.storage)
    when(bitTimer.tick && state === State.STOP) {
      io.read.valid := True
    }.otherwise{
      io.read.valid := False
    }
  }
}
