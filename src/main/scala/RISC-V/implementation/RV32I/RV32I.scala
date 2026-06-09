package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.interfaces.RV32I._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class RV32I(
    genCU: => AbstractControlUnit,
    genDecoder: => AbstractDecoder,
    genBU: => AbstractBranchUnit,
    genALU: => AbstractALU
) extends AbstractExecutionUnit {

  val instr_type = io.instr_type

  val valid_instr = VecInit(InstructionSets.RV32I.map(typ => typ.asUInt).toSeq)
  io.valid := valid_instr.contains(instr_type.asUInt)

  io.misa := "b01__0000__0_00000_00000_00000_01000_00000".U

  val control_unit = Module(genCU)
  control_unit.io_reset <> io_reset

  val decoder = Module(genDecoder)
  decoder.io_reset <> io_reset

  val branch_unit = Module(genBU)
  branch_unit.io_reset <> io_reset

  val alu = Module(genALU)
  alu.io_reset <> io_reset

  decoder.io_decoder.instr := io.instr
  io.stall := control_unit.io_ctrl.stall

  // PC switch
  io_pc.pc_wdata := io_reset.boot_addr
  switch(control_unit.io_ctrl.next_pc_select) {
    is(NEXT_PC_SELECT.PC_PLUS_4) {
      io_pc.pc_wdata := io_pc.pc + 4.U
    }
    is(NEXT_PC_SELECT.BRANCH) {
      io_pc.pc_wdata := Mux(
        branch_unit.io_branch.branch_taken,
        io_pc.pc + decoder.io_decoder.imm,
        io_pc.pc + 4.U
      )
    }
    is(NEXT_PC_SELECT.IMM) {
      io_pc.pc_wdata := io_pc.pc + decoder.io_decoder.imm
    }
    is(NEXT_PC_SELECT.ALU_OUT_ALIGNED) {
      io_pc.pc_wdata := alu.io_alu.result & "hFFFFFFFE".U
    }
  }
  io_pc.pc_we := control_unit.io_ctrl.stall === STALL_REASON.NO_STALL

  // Register file
  io_reg.reg_rs1 := decoder.io_decoder.rs1
  io_reg.reg_rs2 := decoder.io_decoder.rs2
  io_reg.reg_rd := decoder.io_decoder.rd
  io_reg.reg_write_en := control_unit.io_ctrl.reg_we
  io_reg.reg_write_data := 0.U
  switch(control_unit.io_ctrl.reg_write_sel) {
    is(REG_WRITE_SEL.ALU_OUT) {
      io_reg.reg_write_data := alu.io_alu.result
    }
    is(REG_WRITE_SEL.IMM) {
      io_reg.reg_write_data := decoder.io_decoder.imm
    }
    is(REG_WRITE_SEL.PC_PLUS_4) {
      io_reg.reg_write_data := io_pc.pc + 4.U
    }
  }

  // Control unit inputs
  control_unit.io_ctrl.instr_type := instr_type
  control_unit.io_ctrl.data_gnt := io_data.data_gnt

  // ALU inputs
  alu.io_alu.op1 := 0.U
  switch(control_unit.io_ctrl.alu_op_1_sel) {
    is(ALU_OP_1_SEL.RS1) {
      alu.io_alu.op1 := io_reg.reg_read_data1
    }
    is(ALU_OP_1_SEL.PC) {
      alu.io_alu.op1 := io_pc.pc
    }
  }
  alu.io_alu.op2 := 0.U
  switch(control_unit.io_ctrl.alu_op_2_sel) {
    is(ALU_OP_2_SEL.RS2) {
      alu.io_alu.op2 := io_reg.reg_read_data2
    }
    is(ALU_OP_2_SEL.IMM) {
      alu.io_alu.op2 := decoder.io_decoder.imm
    }
  }
  alu.io_alu.alu_op := control_unit.io_ctrl.alu_control

  branch_unit.io_branch.instr_type := instr_type
  branch_unit.io_branch.alu_out := alu.io_alu.result

  // Data interface
  io_data.data_req := control_unit.io_ctrl.data_req
  io_data.data_addr := alu.io_alu.result
  io_data.data_be := control_unit.io_ctrl.data_be
  io_data.data_we := control_unit.io_ctrl.data_we
  io_data.data_wdata := io_reg.reg_read_data2

  // Trap switch
  io_trap.trap_valid := false.B
  io_trap.trap_reason := TRAP_REASON.NONE
  switch(control_unit.io_ctrl.next_pc_select) {
    is(NEXT_PC_SELECT.BRANCH) {
      io_trap.trap_valid := branch_unit.io_branch.branch_taken &&
        ((io_pc.pc + decoder.io_decoder.imm) & "h00000003".U) =/= 0.U
      io_trap.trap_reason := TRAP_REASON.INSTRUCTION_ADDRESS_MISALIGNED
    }
    is(NEXT_PC_SELECT.IMM) {
      io_trap.trap_valid := ((io_pc.pc + decoder.io_decoder.imm) & "h00000003".U) =/= 0.U
      io_trap.trap_reason := TRAP_REASON.INSTRUCTION_ADDRESS_MISALIGNED
    }
    is(NEXT_PC_SELECT.ALU_OUT_ALIGNED) {
      io_trap.trap_valid := (alu.io_alu.result & "h00000003".U) =/= 0.U
      io_trap.trap_reason := TRAP_REASON.INSTRUCTION_ADDRESS_MISALIGNED
    }
  }
}
