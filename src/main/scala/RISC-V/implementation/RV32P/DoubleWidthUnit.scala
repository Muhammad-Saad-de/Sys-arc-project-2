package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model.InstructionSets
import RISCV.model.STALL_REASON
import RISCV.model.TRAP_REASON
import RISCV.model.RISCV_TYPE
import RISCV.model.RISCV_FORMAT

class DoubleWidthUnit extends AbstractExecutionUnit {

  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  val valid_instr = VecInit(
    InstructionSets.DoubleWidthPacked.map(_.asUInt).toSeq
  )
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

  // State: 0=first(lo) cycle, 1=second(hi) cycle, 2=third cycle (pwadda.b only)
  val state = RegInit(0.U(2.W))
  val saved_hi = RegInit(0.U(32.W))
  val saved_pwadd_lo = RegInit(0.U(32.W))

  // Instruction field decoding.
  // Double-dest/source registers have their LSB forced in encoding, so the base
  // register is Cat(instr[N:N-3], 0).
  val rd_base = Cat(io.instr(11, 8), 0.U(1.W))
  val rs1_base = Cat(io.instr(19, 16), 0.U(1.W))
  val rs2_base = Cat(io.instr(24, 21), 0.U(1.W))
  val rs2_scalar = io.instr(24, 20)
  val rs1_single = io.instr(19, 15)

  // PLI.DB: same bit layout as PLI.B — 8-bit imm at [23:16]
  val pli_db_imm = io.instr(23, 16)
  val pli_db_result = Cat(pli_db_imm, pli_db_imm, pli_db_imm, pli_db_imm)

  val src1 = io_reg.reg_read_data1
  val src2 = io_reg.reg_read_data2

  // 4x8-bit wrapping add
  def paddB(s1: UInt, s2: UInt): UInt = Cat(
    (s1(31, 24) + s2(31, 24))(7, 0),
    (s1(23, 16) + s2(23, 16))(7, 0),
    (s1(15, 8) + s2(15, 8))(7, 0),
    (s1(7, 0) + s2(7, 0))(7, 0)
  )

  // 4x8-bit averaging signed add
  def paaddB(s1: UInt, s2: UInt): UInt = Cat(
    ((s1(31, 24).asSInt +& s2(31, 24).asSInt) >> 1).asUInt(7, 0),
    ((s1(23, 16).asSInt +& s2(23, 16).asSInt) >> 1).asUInt(7, 0),
    ((s1(15, 8).asSInt +& s2(15, 8).asSInt) >> 1).asUInt(7, 0),
    ((s1(7, 0).asSInt +& s2(7, 0).asSInt) >> 1).asUInt(7, 0)
  )

  // Broadcast byte0 of scalar to all 4 lanes, wrapping add to s1
  def paddBS(s1: UInt, scalar_reg: UInt): UInt = {
    val sc = scalar_reg(7, 0)
    Cat(
      (Cat(0.U(8.W), s1(31, 24)) + Cat(0.U(8.W), sc))(7, 0),
      (Cat(0.U(8.W), s1(23, 16)) + Cat(0.U(8.W), sc))(7, 0),
      (Cat(0.U(8.W), s1(15, 8)) + Cat(0.U(8.W), sc))(7, 0),
      (Cat(0.U(8.W), s1(7, 0)) + Cat(0.U(8.W), sc))(7, 0)
    )
  }

  // 32-bit saturating signed add (detects overflow via 33-bit carry)
  def satAdd32(s1: UInt, s2: UInt): UInt = {
    val res = s1.asSInt +& s2.asSInt
    val overflow = !res(32) && res(31) // positive overflow
    val underflow = res(32) && !res(31) // negative underflow
    Mux(
      overflow,
      "h7fffffff".U,
      Mux(underflow, "h80000000".U, res(31, 0).asUInt)
    )
  }

  // Widening byte add: byte1+byte1 → [31:16], byte0+byte0 → [15:0]
  def pwaddBLo(s1: UInt, s2: UInt): UInt = Cat(
    (Cat(0.U(8.W), s1(15, 8)) + Cat(0.U(8.W), s2(15, 8)))(15, 0),
    (Cat(0.U(8.W), s1(7, 0)) + Cat(0.U(8.W), s2(7, 0)))(15, 0)
  )

  // Widening byte add: byte3+byte3 → [31:16], byte2+byte2 → [15:0]
  def pwaddBHi(s1: UInt, s2: UInt): UInt = Cat(
    (Cat(0.U(8.W), s1(31, 24)) + Cat(0.U(8.W), s2(31, 24)))(15, 0),
    (Cat(0.U(8.W), s1(23, 16)) + Cat(0.U(8.W), s2(23, 16)))(15, 0)
  )

  // Register interface defaults (overridden in the state machine below)
  io_reg.reg_rs1 := 0.U
  io_reg.reg_rs2 := 0.U
  io_reg.reg_rd := rd_base
  io_reg.reg_write_en := false.B
  io_reg.reg_write_data := 0.U

  when(io.valid) {
    when(state === 0.U) {
      // First cycle: compute lo result, write rd_base, then stall for cycle 2
      io.stall := STALL_REASON.EXECUTION_UNIT
      io_pc.pc_we := false.B
      state := 1.U // default transition; overridden below if needed

      switch(io.instr_type) {
        is(RISCV_TYPE.pli_db) {
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := pli_db_result
        }
        is(RISCV_TYPE.padd_db) {
          io_reg.reg_rs1 := rs1_base
          io_reg.reg_rs2 := rs2_base
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := paddB(src1, src2)
        }
        is(RISCV_TYPE.padd_dbs) {
          io_reg.reg_rs1 := rs1_base
          io_reg.reg_rs2 := rs2_scalar
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := paddBS(src1, src2)
        }
        is(RISCV_TYPE.psadd_dw) {
          io_reg.reg_rs1 := rs1_base
          io_reg.reg_rs2 := rs2_base
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := satAdd32(src1, src2)
        }
        is(RISCV_TYPE.paadd_db) {
          io_reg.reg_rs1 := rs1_base
          io_reg.reg_rs2 := rs2_base
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := paaddB(src1, src2)
        }
        is(RISCV_TYPE.pwadd_b) {
          io_reg.reg_rs1 := rs1_single
          io_reg.reg_rs2 := rs2_scalar
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := pwaddBLo(src1, src2)
          saved_hi := pwaddBHi(src1, src2)
        }
        is(RISCV_TYPE.pwadda_b) {
          // Must read src registers first; no write yet (accumulate unknown)
          io_reg.reg_rs1 := rs1_single
          io_reg.reg_rs2 := rs2_scalar
          io_reg.reg_write_en := false.B
          saved_pwadd_lo := pwaddBLo(src1, src2)
          saved_hi := pwaddBHi(src1, src2)
        }
      }
    }.elsewhen(state === 1.U) {
      // Second cycle: write rd_base+1 and retire (default), except pwadda.b
      state := 0.U

      switch(io.instr_type) {
        is(RISCV_TYPE.pli_db) {
          io_reg.reg_rd := rd_base | 1.U
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := pli_db_result
        }
        is(RISCV_TYPE.padd_db) {
          io_reg.reg_rs1 := rs1_base | 1.U
          io_reg.reg_rs2 := rs2_base | 1.U
          io_reg.reg_rd := rd_base | 1.U
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := paddB(src1, src2)
        }
        is(RISCV_TYPE.padd_dbs) {
          io_reg.reg_rs1 := rs1_base | 1.U
          io_reg.reg_rs2 := rs2_scalar
          io_reg.reg_rd := rd_base | 1.U
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := paddBS(src1, src2)
        }
        is(RISCV_TYPE.psadd_dw) {
          io_reg.reg_rs1 := rs1_base | 1.U
          io_reg.reg_rs2 := rs2_base | 1.U
          io_reg.reg_rd := rd_base | 1.U
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := satAdd32(src1, src2)
        }
        is(RISCV_TYPE.paadd_db) {
          io_reg.reg_rs1 := rs1_base | 1.U
          io_reg.reg_rs2 := rs2_base | 1.U
          io_reg.reg_rd := rd_base | 1.U
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := paaddB(src1, src2)
        }
        is(RISCV_TYPE.pwadd_b) {
          // Reads x0/x0 (absorbed by RVFI dummy pair); write hi from saved reg
          io_reg.reg_rd := rd_base | 1.U
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := saved_hi
        }
        is(RISCV_TYPE.pwadda_b) {
          // Read accumulate pair (rd_base, rd_base+1); still need one more cycle
          io.stall := STALL_REASON.EXECUTION_UNIT
          io_pc.pc_we := false.B
          state := 2.U
          io_reg.reg_rs1 := rd_base
          io_reg.reg_rs2 := rd_base | 1.U
          io_reg.reg_rd := rd_base
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := src1 + saved_pwadd_lo
          saved_hi := src2 + saved_hi // src2=x17; saved_hi=pwadd_hi
        }
      }
    }.elsewhen(state === 2.U) {
      // Third cycle: only used by pwadda.b — write rd_base+1, retire
      io_reg.reg_rd := rd_base | 1.U
      io_reg.reg_write_en := true.B
      io_reg.reg_write_data := saved_hi
      state := 0.U
    }
  }
}
