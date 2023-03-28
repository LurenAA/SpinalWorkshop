package workshop.waveplayer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

//Class used to parametrized the WavePlayer instanciation
case class WavePlayerGenerics(
                               sampleWidth : Int,
                               sampleCountLog2 : Int,
                               phaseWidth : Int,
                               filterCoefWidth : Int
                             ){
  def Sample = UInt(sampleWidth bits)
  def sampleCount = 1 << sampleCountLog2
}

//Area of logic which implement a phase counter, a rom sampler.
case class WavePlayer(generics : WavePlayerGenerics) extends Area{
  import generics._
  assert(phaseWidth >= sampleCountLog2)

  val phase = new Area{
    val run = Bool  //Driven later by a WavePlayerMapper (see WavePlayerMapper implementation)
    val rate = UInt(phaseWidth bits) //Driven later by a WavePlayerMapper
    
    //TODO phase
    val value = Reg(UInt(phaseWidth bits)) init(0)
    when(run) {
      value := value + rate
    }
  }

  val sampler = new Area{
    //TODO Rom definition with a sinus + it sampling
    val romSamples = for(sampleId <- 0 until sampleCount) yield{
      val sin = Math.sin(2 * Math.PI * sampleId / sampleCount) * ((1 << sampleWidth) - 1) * 0.5 + (1 << sampleWidth-1)
      BigInt(sin.toInt)
    }
    val rom = Mem(generics.Sample, sampleCount) initBigInt(romSamples)
    val sample = rom.readAsync(phase.value >> (phaseWidth - sampleCountLog2))
  }

  val filter = new Area{
    val bypass = Bool
    val coef = UInt(filterCoefWidth bits)
    val accumulator = Reg(UInt(sampleWidth + filterCoefWidth bits)) init(0)
    accumulator := accumulator - (accumulator*coef >> filterCoefWidth) + sampler.sample * coef
    val filtredSampler = accumulator >> filterCoefWidth
    val value = bypass ? sampler.sample | filtredSampler
  }
}

//Area capable to map a WavePlayer on a BusSlaveFactory
class WavePlayerMapper(bus : BusSlaveFactory, wavePlayer : WavePlayer) extends Area{
  bus.driveAndRead(wavePlayer.phase.run,  address = 0x00) init(False)
  //TODO phase.rate, phase.value, filter.bypass, filter.coef mapping
  bus.drive(wavePlayer.phase.rate, address = 0x04) 
  bus.read(wavePlayer.phase.value, address = 0x08)

  bus.driveAndRead(wavePlayer.filter.bypass,  address = 0x10) init(True)
  bus.drive(wavePlayer.filter.coef, address = 0x14) init(0)
}
