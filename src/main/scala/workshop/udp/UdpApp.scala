package workshop.udp
import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, StateParallelFsm, State, StateMachine}


case class UdpAppCmd() extends Bundle{
  val ip      = Bits(32 bits)
  val srcPort = Bits(16 bits)
  val dstPort = Bits(16 bits)
  val length  = UInt(16 bits)
}

case class UdpAppBus() extends Bundle with IMasterSlave{
  val cmd = Stream(UdpAppCmd())
  val data = Stream(Fragment(Bits(8 bits)))

  override def asMaster(): Unit = master(cmd,data)
}

object Hello{
  val discoveringCmd = 0x11
  val discoveringRsp = 0x22
}

case class UdpApp(helloMessage : String,helloPort : Int = 37984) extends Component{
  val io = new Bundle{
    val rx = slave(UdpAppBus())
    val tx = master(UdpAppBus())
  }

  // TODO give default value to rx/tx output pins
  io.rx.cmd.ready := False
  io.rx.data.ready := False
  io.tx.cmd.valid := False
  // io.tx.cmd.payload := UdpAppCmd()
  // io.tx.data.payload := Fragment(Bits(8 bits))
  io.tx.cmd.payload.dstPort := 0
  io.tx.cmd.payload.ip := 0
  io.tx.cmd.payload.length := 0
  io.tx.cmd.payload.srcPort := 0
  io.tx.data.valid := False
  io.tx.data.payload.fragment := 0
  io.tx.data.payload.last := False

  when(io.rx.cmd.valid) {
    io.tx.cmd.payload.dstPort := io.rx.cmd.payload.srcPort
    io.tx.cmd.payload.srcPort := io.rx.cmd.payload.dstPort
    io.tx.cmd.payload.ip := io.rx.cmd.payload.ip
    io.tx.cmd.payload.length := helloMessage.length() + 1
  }

  val ok = Reg(Bool) init(True)

  val fsm = new StateMachine{
    //Filter rx dst ports
    val idle : State = new State with EntryPoint{
      whenIsActive{
        // TODO Check io.rx.cmd dst port
        when(io.rx.cmd.dstPort === helloPort && ok && io.rx.cmd.valid) {
          ok := False
          goto(helloHeader)
        }.elsewhen(!ok && io.rx.cmd.valid) {
          io.rx.cmd.ready := io.rx.data.payload.last
          io.rx.data.ready := True
        }
        .elsewhen(io.rx.cmd.dstPort =/= helloPort && io.rx.cmd.valid ) {
          io.rx.data.ready := True
          when(io.rx.data.payload.last) {
            io.rx.cmd.ready := True
          }
        }
        .otherwise{
          ok := True
          
        }
        
      }
    }

    //Check the hello protocol Header
    val helloHeader = new State{
      whenIsActive {
        // TODO check that the first byte of the packet payload is equals to Hello.discoveringCmd
        when(io.rx.data.valid) {
          when(io.rx.data.payload === Hello.discoveringCmd) {
            goto(discoveringRspTx)
          }.otherwise{
            goto(idle)
          }
          
        }
      }
    }

    //Send an discoveringRsp packet
    val discoveringRspTx = new StateParallelFsm(
      discoveringRspTxCmdFsm,
      discoveringRspTxDataFsm
    ){
      whenCompleted{
        //TODO return to IDLE
        ok := True
        io.rx.cmd.ready := True
        goto(idle)
      }
    }
  }

  //Inner FSM of the discoveringRspTx state
  lazy val discoveringRspTxCmdFsm = new StateMachine{
    val sendCmd = new State with EntryPoint{
      whenIsActive{
        //TODO send one io.tx.cmd transaction
        io.tx.cmd.valid := True
        when(io.tx.cmd.ready) {
          exit()
        }
      }
    }
  }

  //Inner FSM of the discoveringRspTx state
  lazy val discoveringRspTxDataFsm = new StateMachine{
    val sendHeader = new State with EntryPoint{
      whenIsActive{
        //TODO send the io.tx.cmd header (Hello.discoveringRsp)
        io.tx.data.valid := True
        io.tx.data.payload.fragment := Hello.discoveringRsp
        when(io.tx.data.ready) {
          goto(sendMessage)
        }
      }
    }

    val sendMessage = new State{
      val counter = Reg(UInt(log2Up(helloMessage.length) bits))
      onEntry{
        counter := 0
      }
      whenIsActive{
        //TODO send the message on io.tx.cmd header
        io.tx.data.valid := True
        for(i <- 0 until helloMessage.length()) {
          when(counter === i) {
            io.tx.data.payload.fragment := helloMessage(i)
          }
        }
        when(counter === helloMessage.length - 1) {
          io.tx.data.payload.last := True
        }
        when(io.tx.data.ready) {
          counter := counter + 1
          when(counter === helloMessage.length - 1) {
            exit()
          }
        }
      }
    }
  }
}
