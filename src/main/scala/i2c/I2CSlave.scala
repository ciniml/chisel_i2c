// See README.md for license details.

package i2cslave

import chisel3._
import chisel3.util._
import chisel3.util.Enum

class I2CSlaveRegIO(addressWidth: Int) extends Bundle {
  val address = Input(UInt(addressWidth.W))
  val is_write = Input(Bool())
  val write_data = Input(UInt(8.W))
  val read_data = Output(UInt(8.W))
  val request = Input(Bool())
  val response = Output(Bool())

  override def cloneType: this.type = new I2CSlaveRegIO(addressWidth).asInstanceOf[this.type]
}

class I2CIO extends Bundle {
  val sda_i = Input(Bool())
  val sda_o = Output(Bool())
  val scl_i = Input(Bool())
  val scl_o = Output(Bool())
}

class I2CSlave(addressWidth: Int, filterDepth: Int, i2cAddress: Int) extends Module {
  val io = IO(new Bundle {
    val i2c = new I2CIO
    val reg_if = Flipped(new I2CSlaveRegIO(addressWidth))
  })

  val scl_i = RegInit(Bool(), false.B)
  val sda_i = RegInit(Bool(), false.B)
  val scl_o = RegInit(Bool(), true.B)
  val sda_o = RegInit(Bool(), true.B)
  io.i2c.scl_o := scl_o
  io.i2c.sda_o := sda_o

  val scl_i_reg = RegInit(Bool(), false.B)
  val sda_i_reg = RegInit(Bool(), false.B)

  val scl_i_filter = RegInit(UInt(filterDepth.W), 0.U)
  val sda_i_filter = RegInit(UInt(filterDepth.W), 0.U)

  when (scl_i) {
    scl_i := scl_i_filter.orR()
  } .otherwise {
    scl_i := scl_i_filter.andR()
  }
  when (sda_i) {
    sda_i := sda_i_filter.orR()
  } .otherwise {
    sda_i := sda_i_filter.andR()
  }

  scl_i_reg := scl_i
  sda_i_reg := sda_i

  scl_i_filter := Cat(scl_i_filter(filterDepth-2, 0), io.i2c.scl_i.asUInt())
  sda_i_filter := Cat(sda_i_filter(filterDepth-2, 0), io.i2c.sda_i.asUInt())

  val start_condition = Wire(Bool())
  val stop_condition = Wire(Bool())
  start_condition := scl_i && sda_i_reg && !sda_i
  stop_condition := scl_i && !sda_i_reg && sda_i

  // Byte transmission
  val begin_byte = RegInit(false.B)
  val active = RegInit(false.B)
  val end_byte = RegInit(false.B)
  val end_ack = RegInit(false.B)
  val bit_counter = RegInit(0.U(4.W))
  val input_bits = RegInit(0.U(8.W))
  val output_bits = RegInit(0.U(8.W))
  val next_ack = RegInit(false.B)
  val master_read = RegInit(false.B)
  val master_acked = RegInit(false.B)

  val scl_rising = Wire(Bool())
  val scl_falling = Wire(Bool())
  scl_rising := !scl_i_reg && scl_i
  scl_falling := scl_i_reg && !scl_i

  when( start_condition || stop_condition ) {
    bit_counter := 0.U
    input_bits := 0.U
    master_acked := false.B
    active := false.B
    end_byte := false.B
    end_ack := false.B
    scl_o := true.B
    sda_o := true.B
  } .otherwise {
    when( begin_byte ) {
      active := true.B
    } .elsewhen( bit_counter >= 9.U ) {
      active := false.B
    } .otherwise {
      active := active
    }

    end_byte := active && scl_rising && bit_counter === 7.U
    end_ack := active && scl_rising && bit_counter >= 8.U

    when( !active && begin_byte ) {
      bit_counter := 0.U
    } .elsewhen ( active && scl_rising && bit_counter < 9.U ) {
      bit_counter := bit_counter + 1.U
    } .otherwise {
      bit_counter := bit_counter
    }

    when(active && bit_counter < 8.U && scl_rising ) {
      input_bits := Cat(input_bits(6, 0), sda_i)
    } .otherwise {
      input_bits := input_bits
    }

    when(active && scl_rising && bit_counter >= 8.U ) {
      master_acked := !sda_i
    } otherwise {
      master_acked := master_acked
    }

    when(active && !scl_i && master_read && bit_counter < 8.U ) {
      sda_o := output_bits(7.U - bit_counter)
    } .elsewhen( active && !scl_i && !master_read && bit_counter === 8.U ) {
      sda_o := !next_ack
    } .elsewhen( scl_i ) {
      sda_o := sda_o
    } .otherwise {
      sda_o := true.B
    }
  }

  val s_idle :: s_device_address :: s_reg_address :: s_read :: s_write :: Nil = Enum(5)
  val i2c_state = RegInit(s_idle)
  val reg_is_write = RegInit(false.B)
  val reg_request = RegInit(false.B)
  val reg_address = RegInit(0.U(addressWidth.W))
  val reg_read_data = RegInit(0.U(8.W))
  val reg_write_data = RegInit(0.U(8.W))

  // external register interface connection
  io.reg_if.request := reg_request
  io.reg_if.address := reg_address
  io.reg_if.is_write := reg_is_write
  io.reg_if.write_data := reg_write_data

  val is_address_matching = Wire(Bool())
  is_address_matching := input_bits(7, 1) === i2cAddress.U(7.W)

  begin_byte := false.B
  switch(i2c_state) {
    is(s_idle) {
      when( start_condition ) {
        next_ack := false.B
        master_read := false.B
        begin_byte := true.B
        i2c_state := s_device_address
      }
    }
    is( s_device_address ) {
      when( stop_condition ) {
        i2c_state := s_idle
      } .elsewhen( end_byte && is_address_matching) {
        next_ack := true.B
      } .elsewhen( end_ack && is_address_matching) {
        when( input_bits(0) ) {
          master_read := true.B
          begin_byte := true.B
          i2c_state := s_read
          reg_write_data := 0.U
          reg_is_write := false.B
          reg_request := true.B
        } .otherwise {
          master_read := false.B
          next_ack := true.B
          begin_byte := true.B
          i2c_state := s_reg_address
        }
      } .elsewhen( end_ack && !is_address_matching ) {
        i2c_state := s_idle
      }
    }
    is( s_reg_address) {
      when( stop_condition ) {
        i2c_state := s_idle
      } .elsewhen( end_byte ) {
        reg_address := input_bits
      } .elsewhen( end_ack ) {
        begin_byte := true.B
        master_read := false.B
        next_ack := false.B
        i2c_state := s_write
      }
    }
    is( s_read ) {
      when( reg_request ) {
        reg_request := false.B
      }
      when( io.reg_if.response ) {
        output_bits := io.reg_if.read_data
      } otherwise {
        output_bits := output_bits
      }

      when( stop_condition ) {
        i2c_state := s_idle
      } .elsewhen( start_condition ) {
        next_ack := true.B
        master_read := false.B
        begin_byte := true.B
        i2c_state := s_device_address
      } .elsewhen( end_ack && master_acked ) {
        begin_byte := true.B
        reg_address := reg_address + 1.U
        reg_request := true.B
      } .elsewhen( end_ack && !master_acked ) {
        i2c_state := s_idle
      }
    }
    is(s_write) {
      when( reg_request ) {
        reg_request := false.B
      }

      when( io.reg_if.response ) {
        next_ack := true.B
      } .elsewhen( reg_request ) {
        next_ack := io.reg_if.response
      } .otherwise {
        next_ack := next_ack
      }

      when( stop_condition ) {
        i2c_state := s_idle
      } .elsewhen( start_condition ) {
        next_ack := true.B
        master_read := false.B
        begin_byte := true.B
        i2c_state := s_device_address
      } .elsewhen( end_byte ) {
        reg_write_data := input_bits
        reg_is_write := true.B
        reg_request := true.B
      } .elsewhen( end_ack && next_ack ) {
        reg_address := reg_address + 1.U
        begin_byte := true.B
      } .elsewhen( end_ack && !next_ack ) {
        i2c_state := s_idle
      }
    }
  }

}

object ElaborateI2CSlave extends App {
  Driver.execute(args, () => new I2CSlave(8, 3, 0x48))
}
