package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class SimplePackedUnit extends AbstractExecutionUnit {

  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  val valid_instr = VecInit(InstructionSets.BasicPacked.map(_.asUInt).toSeq)
  io.valid := valid_instr.contains(io.instr_type.asUInt)

  io.stall := STALL_REASON.NO_STALL
  io_pc.pc_we := true.B
  io_pc.pc_wdata := io_pc.pc + 4.U
  io_trap.trap_valid := false.B
  io_trap.trap_reason := TRAP_REASON.NONE
  io_data.data_req := false.B
  io_data.data_addr := 0.U
  io_data.data_be := 0.U
  io_data.data_we := false.B
  io_data.data_wdata := 0.U

  // rs1/rs2 are 0 for immediate instructions (PLI.B/H, PLUI.H)
  val isImm = (io.instr_type.asUInt === RISCV_TYPE.pli_b.asUInt) ||
    (io.instr_type.asUInt === RISCV_TYPE.pli_h.asUInt) ||
    (io.instr_type.asUInt === RISCV_TYPE.plui_h.asUInt)
  io_reg.reg_rs1 := Mux(isImm, 0.U, io.instr(19, 15))
  io_reg.reg_rs2 := Mux(isImm, 0.U, io.instr(24, 20))
  io_reg.reg_rd := io.instr(11, 7)
  io_reg.reg_write_en := io.valid

  val src1 = io_reg.reg_read_data1
  val src2 = io_reg.reg_read_data2

  // saturating signed add
  def satAdd(a: SInt, b: SInt, n: Int): SInt = {
    val res = a +& b
    val maxVal = ((1 << (n - 1)) - 1).S
    val minVal = (-(1 << (n - 1))).S
    Mux(res > maxVal, maxVal, Mux(res < minVal, minVal, res(n - 1, 0).asSInt))
  }

  // averaging signed add
  def avgAdd(a: SInt, b: SInt): SInt = {
    ((a +& b) >> 1).asSInt
  }

  // PLI.B — replicate 8-bit imm into all 4 byte lanes
  // Assembler encodes the 8-bit imm at bits [23:16]
  val pli_b_imm = io.instr(23, 16)
  val pli_b_result = Cat(pli_b_imm, pli_b_imm, pli_b_imm, pli_b_imm)

  // PLI.H — replicate 10-bit sign-extended imm into both halfword lanes
  // Assembler encodes imm[8:0] at bits [24:16] and imm[9] (sign) at bit [15]
  val pli_h_imm = Cat(Fill(6, io.instr(15)), io.instr(15), io.instr(24, 16))
  val pli_h_result = Cat(pli_h_imm(15, 0), pli_h_imm(15, 0))

  // PLUI.H — load 10-bit imm into upper bits [15:6] of both halfwords
  // Assembler encodes imm[9:1] at bits [23:15] and imm[0] at bit [24]
  val plui_h_imm = Cat(io.instr(23, 15), io.instr(24), 0.U(6.W))
  val plui_h_result = Cat(plui_h_imm(15, 0), plui_h_imm(15, 0))

  // PADD.B — 4x8-bit wrapping add
  val padd_b_result = Cat(
    (src1(31, 24) + src2(31, 24))(7, 0),
    (src1(23, 16) + src2(23, 16))(7, 0),
    (src1(15, 8) + src2(15, 8))(7, 0),
    (src1(7, 0) + src2(7, 0))(7, 0)
  )

  // PADD.H — 2x16-bit wrapping add
  val padd_h_result = Cat(
    (src1(31, 16) + src2(31, 16))(15, 0),
    (src1(15, 0) + src2(15, 0))(15, 0)
  )

  // PSUB.B — 4x8-bit wrapping subtract
  val psub_b_result = Cat(
    (src1(31, 24) - src2(31, 24))(7, 0),
    (src1(23, 16) - src2(23, 16))(7, 0),
    (src1(15, 8) - src2(15, 8))(7, 0),
    (src1(7, 0) - src2(7, 0))(7, 0)
  )

  // PSUB.H — 2x16-bit wrapping subtract
  val psub_h_result = Cat(
    (src1(31, 16) - src2(31, 16))(15, 0),
    (src1(15, 0) - src2(15, 0))(15, 0)
  )

  // PSADD.B — 4x8-bit saturating signed add
  val psadd_b_result = Cat(
    satAdd(src1(31, 24).asSInt, src2(31, 24).asSInt, 8).asUInt(7, 0),
    satAdd(src1(23, 16).asSInt, src2(23, 16).asSInt, 8).asUInt(7, 0),
    satAdd(src1(15, 8).asSInt, src2(15, 8).asSInt, 8).asUInt(7, 0),
    satAdd(src1(7, 0).asSInt, src2(7, 0).asSInt, 8).asUInt(7, 0)
  )

  // PSADD.H — 2x16-bit saturating signed add
  val psadd_h_result = Cat(
    satAdd(src1(31, 16).asSInt, src2(31, 16).asSInt, 16).asUInt(15, 0),
    satAdd(src1(15, 0).asSInt, src2(15, 0).asSInt, 16).asUInt(15, 0)
  )

  // PAADD.B — 4x8-bit averaging signed add
  val paadd_b_result = Cat(
    avgAdd(src1(31, 24).asSInt, src2(31, 24).asSInt).asUInt(7, 0),
    avgAdd(src1(23, 16).asSInt, src2(23, 16).asSInt).asUInt(7, 0),
    avgAdd(src1(15, 8).asSInt, src2(15, 8).asSInt).asUInt(7, 0),
    avgAdd(src1(7, 0).asSInt, src2(7, 0).asSInt).asUInt(7, 0)
  )

  // PAADD.H — 2x16-bit averaging signed add
  val paadd_h_result = Cat(
    avgAdd(src1(31, 16).asSInt, src2(31, 16).asSInt).asUInt(15, 0),
    avgAdd(src1(15, 0).asSInt, src2(15, 0).asSInt).asUInt(15, 0)
  )

  // PADD.BS — broadcast byte0 of src2 to all 4 byte lanes, add to src1, pack as 2x16
  val bs_scalar = src2(7, 0)
  val padd_bs_hi = Cat(0.U(8.W), src1(31, 24)) + Cat(0.U(8.W), bs_scalar)
  val padd_bs_lo = Cat(0.U(8.W), src1(23, 16)) + Cat(0.U(8.W), bs_scalar)
  val padd_bs_result = Cat(
    Cat(padd_bs_hi(7, 0), padd_bs_lo(7, 0)),
    Cat(
      (Cat(0.U(8.W), src1(15, 8)) + Cat(0.U(8.W), bs_scalar))(7, 0),
      (Cat(0.U(8.W), src1(7, 0)) + Cat(0.U(8.W), bs_scalar))(7, 0)
    )
  )

  // PADD.HS — broadcast halfword0 of src2 to both 16-bit lanes, add to src1
  val hs_scalar = src2(15, 0)
  val padd_hs_result = Cat(
    (src1(31, 16) + hs_scalar)(15, 0),
    (src1(15, 0) + hs_scalar)(15, 0)
  )

  io_reg.reg_write_data := 0.U
  switch(io.instr_type) {
    is(RISCV_TYPE.pli_b) { io_reg.reg_write_data := pli_b_result }
    is(RISCV_TYPE.pli_h) { io_reg.reg_write_data := pli_h_result }
    is(RISCV_TYPE.plui_h) { io_reg.reg_write_data := plui_h_result }
    is(RISCV_TYPE.padd_b) { io_reg.reg_write_data := padd_b_result }
    is(RISCV_TYPE.padd_h) { io_reg.reg_write_data := padd_h_result }
    is(RISCV_TYPE.padd_bs) { io_reg.reg_write_data := padd_bs_result }
    is(RISCV_TYPE.padd_hs) { io_reg.reg_write_data := padd_hs_result }
    is(RISCV_TYPE.psub_b) { io_reg.reg_write_data := psub_b_result }
    is(RISCV_TYPE.psub_h) { io_reg.reg_write_data := psub_h_result }
    is(RISCV_TYPE.psadd_b) { io_reg.reg_write_data := psadd_b_result }
    is(RISCV_TYPE.psadd_h) { io_reg.reg_write_data := psadd_h_result }
    is(RISCV_TYPE.paadd_b) { io_reg.reg_write_data := paadd_b_result }
    is(RISCV_TYPE.paadd_h) { io_reg.reg_write_data := paadd_h_result }
  }
}
