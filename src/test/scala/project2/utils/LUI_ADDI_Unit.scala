package project2.utils

import chisel3._
import chisel3.util._
import RISCV.model._
import RISCV.interfaces.generic.AbstractExecutionUnit

class LUI_ADDI_Unit extends AbstractExecutionUnit {

  io.misa := "b01__0000__0_00000_00000_00000_00000_00000".U

  val instr_type = io.instr_type

  val valid_instr = VecInit(
    Seq(RISCV_TYPE.lui, RISCV_TYPE.addi).map(typ => typ.asUInt).toSeq
  )
  io.valid := valid_instr.contains(instr_type.asUInt)

  val opcode = io.instr(6, 0)
  val funct3 = io.instr(14, 12)
  val rs1 = io.instr(19, 15)
  val rd = io.instr(11, 7)
  val imm_lui = io.instr(31, 12)
  val imm_addi = io.instr(31, 20)

  io.stall := STALL_REASON.NO_STALL
  io_pc.pc_wdata := io_pc.pc + 4.U
  io_pc.pc_we := true.B

  io_reg.reg_rs1 := Mux(RISCV_OP(opcode) === RISCV_OP.LUI, 0.U, rs1)
  io_reg.reg_rs2 := 0.U
  io_reg.reg_rd := rd
  io_reg.reg_write_en := true.B

  io_reg.reg_write_data := 0.U
  switch(RISCV_OP(opcode)) {
    is(RISCV_OP.LUI) {
      io_reg.reg_write_data := (imm_lui ## 0.U(12.W))
    }
    is(RISCV_OP.OP_IMM) {
      io_reg.reg_write_data := (io_reg.reg_read_data1.asSInt + imm_addi.asSInt).asUInt
    }
  }

  io_data.data_req := false.B
  io_data.data_addr := 0.U
  io_data.data_be := 0.U
  io_data.data_we := false.B
  io_data.data_wdata := 0.U

  // Assign the trap interface
  io_trap.trap_valid := false.B
  io_trap.trap_reason := TRAP_REASON.NONE
}
