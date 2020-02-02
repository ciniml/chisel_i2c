// See README.md for license details.

package m5stack

import chisel3._
import chisel3.core.Analog
import chisel3.util._
import chisel3.util.Enum
import i2cslave._

class IOBUF extends BlackBox {
  val io = IO(new Bundle {
    val O = Output(Bool())
    val I = Input(Bool())
    val IO = Analog(1.W)
    val OEN = Input(Bool()) 
  })
}

class M5StackFPGARegisters(addressWidth: Int) extends Module {
  val io = IO(new Bundle {
    val led_out = Output(UInt(3.W))
    val reg_if = new I2CSlaveRegIO(addressWidth)
  })

  val response = RegInit(false.B)
  io.reg_if.response := response
  val read_data = RegInit(0.U(8.W))
  io.reg_if.read_data := read_data
  
  val led_out = RegInit(0.U(3.W))
  io.led_out := ~led_out

  response := false.B
  when(io.reg_if.request) {
    response := true.B
    when( io.reg_if.is_write ) {
      switch(io.reg_if.address) {
        is(1.U) {
          led_out := io.reg_if.write_data(2, 0)
          response := true.B
        }
      }
    } otherwise {
      switch(io.reg_if.address) {
        is(0.U) {
          read_data := 0xa5.U
          response := true.B
        }
      }
    }
  }

}

class M5StackFPGASystem(filterDepth: Int, i2cAddress: Int) extends Module {
  val io = IO(new Bundle {
    val i2c_sda = Analog(1.W)
    val i2c_scl = Analog(1.W)
    val led_out = Output(UInt(3.W))
  })
  val i2cSlave = Module(new I2CSlave(8, filterDepth, i2cAddress))
  val regs = Module(new M5StackFPGARegisters(8))
  val sda = Module(new IOBUF)
  val scl = Module(new IOBUF)
  
  sda.io.I := false.B
  i2cSlave.io.i2c.sda_i := sda.io.O
  sda.io.OEN := i2cSlave.io.i2c.sda_o
  io.i2c_sda <> sda.io.IO

  scl.io.I := false.B
  i2cSlave.io.i2c.scl_i := scl.io.O
  scl.io.OEN := i2cSlave.io.i2c.scl_o
  io.i2c_scl <> scl.io.IO

  
  io.led_out <> regs.io.led_out
  i2cSlave.io.reg_if <> regs.io.reg_if
}

object Elaborate extends App {
  Driver.execute(args, () => new M5StackFPGASystem(3, 0x48))
}
