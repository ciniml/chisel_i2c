// See README.md for license details.

package i2cslave

import java.io.File

import chisel3._
import chisel3.util._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class I2CSlaveTestRegister(addressWidth: Int) extends Module {
  val io = IO(new I2CSlaveRegIO(addressWidth))

  val mem = RegInit(VecInit(Range(0, 256).map(x => x.U(8.W))))
  val response = RegInit(false.B)
  io.response := response
  val read_data = RegInit(0.U(8.W))
  io.read_data := read_data
  
  response := false.B
  when(io.request) {
    response := true.B
    when( io.is_write ) {
      mem(io.address) := io.write_data
    } otherwise {
      read_data := mem(io.address)
    }
  }

}

class I2CSlaveTestSystem(addressWidth: Int, filterDepth: Int, i2cAddress: Int) extends Module {
  val dut = Module(new I2CSlave(addressWidth, filterDepth, i2cAddress))
  val testReg = Module(new I2CSlaveTestRegister(addressWidth))
  val io = IO(new I2CIO)
  io <> dut.io.i2c
  testReg.io <> dut.io.reg_if
}

class I2CSlaveUnitTester(system: I2CSlaveTestSystem, i2cClockBase: Int) extends PeekPokeTester(system) {
  def i2cStart = {
    poke(dut.io.scl_i, true)
    poke(dut.io.sda_i, true)
    step(i2cClockBase)
    poke(dut.io.sda_i, false)
    step(i2cClockBase)
  }
  def i2cStop = {
    poke(dut.io.scl_i, true)
    poke(dut.io.sda_i, false)
    step(i2cClockBase)
    poke(dut.io.sda_i, true)
    step(i2cClockBase)
  }
  def i2cRead(ack: Boolean): Int = {
    var data:Int = 0
    poke(dut.io.scl_i, true)
    for(i <- 0 until 8) {
      poke(dut.io.scl_i, false)
      step(i2cClockBase*2)
      poke(dut.io.scl_i, true)
      step(i2cClockBase)
      data = (data << 1) | peek(dut.io.sda_o).toInt
      step(i2cClockBase)
    }
    poke(dut.io.scl_i, false)
    step(i2cClockBase)
    poke(dut.io.sda_i, !ack)
    step(i2cClockBase)
    poke(dut.io.scl_i, true)
    step(i2cClockBase*2)
    poke(dut.io.scl_i, false)
    step(i2cClockBase*2)
    data
  }
  def i2cWrite(value: Int): Boolean = {
    var data: Int = value
    poke(dut.io.scl_i, true)
    for(i <- 0 until 8) {
      poke(dut.io.scl_i, false)
      poke(dut.io.sda_i, (data & 0x80) != 0)
      data = data << 1
      step(i2cClockBase*2)
      poke(dut.io.scl_i, true)
      step(i2cClockBase*2)
    }
    poke(dut.io.scl_i, false)
    poke(dut.io.sda_i, true)
    step(i2cClockBase*2)
    poke(dut.io.scl_i, true)
    step(i2cClockBase)
    val ack = peek(dut.io.sda_o) == 0
    step(i2cClockBase)
    poke(dut.io.scl_i, false)
    step(i2cClockBase*2)
    ack
  }

  def i2cRegisterRead(device: Int, address: Int): Option[Int]  = {
    var value: Option[Int] = None
    i2cStart
    if( i2cWrite((device << 1) | 0) ) {
      if( i2cWrite(address) ) {
        i2cStart
        if( i2cWrite((device << 1) | 1) ) {
          value = Some(i2cRead(false))
        }
      }
    }
    i2cStop
    value
  }

  def i2cRegisterWrite(device: Int, address: Int, value: Int): Boolean = {
    var success = false
    i2cStart
    if( i2cWrite((device << 1) | 0) ) {
      if( i2cWrite(address) ) {
        if( i2cWrite(value) ) {
          success = true
        }
      }
    }
    i2cStop
    success
  }
}

class I2CSlaveTester extends ChiselFlatSpec {
  val dutName = "I2CSlave"
  val i2cClockBase = 5
  val i2cDeviceAddress = 0x48
  behavior of dutName

  val args = Array(
    s"-tn=$dutName",
    s"-td=test_run_dir/$dutName",
    "-tgvo=on",
    "-tbn=verilator",
  )
  it should "Read must be successful" in {
    Driver.execute(args, () => new I2CSlaveTestSystem(8, 3, i2cDeviceAddress)){
      dut => new I2CSlaveUnitTester(dut, i2cClockBase) {
        for( i:Int <- 0 to 255) {
          val result = i2cRegisterRead(i2cDeviceAddress, i)
          expect(result.isDefined, msg = f"reg #$i: read failed")
          if( result.isDefined ) {
            expect(result.get == i, f"reg #$i: data mismatch, expected=$i, actual=${result.get}")
          }
        }
      }
    } should be (true)
  } 
  it should "Write must be successful" in {
    Driver.execute(args, () => new I2CSlaveTestSystem(8, 3, i2cDeviceAddress)){
      dut => new I2CSlaveUnitTester(dut, i2cClockBase) {
        for( i:Int <- 0 to 255) {
          var nv = 0
          var ii = i
          for( j <- 0 to 7 ) {
            nv = (nv << 1) | (ii & 1)
            ii = ii >> 1
          }
          val success = i2cRegisterWrite(i2cDeviceAddress, i, nv)
          expect(success, f"reg #$i: write failed")
          val result = i2cRegisterRead(i2cDeviceAddress, i)
          expect(result.isDefined, msg = f"reg #$i: read failed")
          if( result.isDefined ) {
            expect(result.get == nv, f"reg #$i: data mismatch, expected=$nv, actual=${result.get}")
          }
        }
      }
    } should be (true)
  } 
  it should "Recover from malformed transaction must success" in {
    Driver.execute(args, () => new I2CSlaveTestSystem(8, 3, i2cDeviceAddress)){
      dut => new I2CSlaveUnitTester(dut, i2cClockBase) {
        i2cStart
        i2cStop
        val result = i2cRegisterRead(i2cDeviceAddress, 0xde)
        expect(result.isDefined, msg = "read failed after start-stop malformed transaction")
      }
    } should be (true)
  } 
}
