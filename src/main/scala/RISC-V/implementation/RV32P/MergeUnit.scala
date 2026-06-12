package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model.InstructionSets
import RISCV.model.STALL_REASON
import RISCV.model.TRAP_REASON
import RISCV.model.RISCV_TYPE

class MergeUnit extends AbstractExecutionUnit {

  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  val var_opcode = io.instr(6, 0)
  val var_funct3 = io.instr(14, 12)
  val var_funct7 = io.instr(31, 25)
  val var_rs1Addr = io.instr(19, 15)
  val var_rs2Addr = io.instr(24, 20)
  val var_rdAddr = io.instr(11, 7)

  val var_OP32 = "b0111011".U(7.W)
  val var_OP = "b0110011".U(7.W)

  val var_isMVM =
    (var_opcode === var_OP32) && (var_funct3 === "b001".U) && (var_funct7 === "b1010100".U)
  val var_isMVMN =
    (var_opcode === var_OP32) && (var_funct3 === "b001".U) && (var_funct7 === "b1010101".U)
  val var_isMERGE =
    (var_opcode === var_OP32) && (var_funct3 === "b001".U) && (var_funct7 === "b1010110".U)
  val var_isPACK =
    (var_opcode === var_OP) && (var_funct3 === "b100".U) && (var_funct7 === "b0000100".U)

  val var_isKnown = var_isMVM || var_isMVMN || var_isMERGE || var_isPACK
  val var_needs3Regs = var_isMVM || var_isMVMN || var_isMERGE

  // mvm mvmn merge need an extra cycle to read prior rd
  val sIdle :: sReadRD :: Nil = Enum(2)
  val var_state = RegInit(sIdle)

  val var_savedRS1 = Reg(UInt(32.W))
  val var_savedRS2 = Reg(UInt(32.W))
  val var_savedRDAddr = Reg(UInt(5.W))
  val var_savedPC = Reg(UInt(32.W))
  val var_savedOp = Reg(UInt(2.W))

  val var_OP_MVM = 0.U(2.W)
  val var_OP_MVMN = 1.U(2.W)
  val var_OP_MERGE = 2.U(2.W)

  io.valid := false.B
  io.stall := STALL_REASON.NO_STALL

  io_reg.reg_rs1 := var_rs1Addr
  io_reg.reg_rs2 := var_rs2Addr
  io_reg.reg_rd := var_rdAddr
  io_reg.reg_write_en := false.B
  io_reg.reg_write_data := 0.U

  io_pc.pc_we := false.B
  io_pc.pc_wdata := io_pc.pc + 4.U

  io_data.data_req := false.B
  io_data.data_addr := 0.U
  io_data.data_be := 0.U
  io_data.data_we := false.B
  io_data.data_wdata := 0.U

  io_trap.trap_valid := false.B
  io_trap.trap_reason := TRAP_REASON.NONE

  switch(var_state) {

    is(sIdle) {
      when(var_isKnown) {
        io.valid := true.B

        when(var_isPACK) {
          // pack lower 16 bits of rs1 and rs2, retires immediately
          val var_result =
            Cat(io_reg.reg_read_data2(15, 0), io_reg.reg_read_data1(15, 0))

          io_reg.reg_rd := var_rdAddr
          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := var_result

          io_pc.pc_we := true.B
          io_pc.pc_wdata := io_pc.pc + 4.U

          io.stall := STALL_REASON.NO_STALL

        }.elsewhen(var_needs3Regs) {
          // latch rs1 and rs2, stall, do not retire yet
          var_savedRS1 := io_reg.reg_read_data1
          var_savedRS2 := io_reg.reg_read_data2
          var_savedRDAddr := var_rdAddr
          var_savedPC := io_pc.pc
          var_savedOp := MuxCase(
            var_OP_MVM,
            Seq(
              var_isMVMN -> var_OP_MVMN,
              var_isMERGE -> var_OP_MERGE
            )
          )

          io_pc.pc_we := false.B
          io.stall := STALL_REASON.EXECUTION_UNIT
          var_state := sReadRD
        }
      }
    }

    is(sReadRD) {
      io.valid := true.B

      // redirect port 1 to rd to read its prior value
      io_reg.reg_rs1 := var_savedRDAddr
      io_reg.reg_rs2 := 0.U

      val var_rdPrior = io_reg.reg_read_data1
      val var_rs1 = var_savedRS1
      val var_rs2 = var_savedRS2

      // mvm mvmn merge bit select logic
      val var_result = MuxCase(
        0.U(32.W),
        Seq(
          (var_savedOp === var_OP_MVM) -> ((~var_rs2 & var_rdPrior) | (var_rs2 & var_rs1)),
          (var_savedOp === var_OP_MVMN) -> ((~var_rs2 & var_rs1) | (var_rs2 & var_rdPrior)),
          (var_savedOp === var_OP_MERGE) -> ((~var_rdPrior & var_rs1) | (var_rdPrior & var_rs2))
        )
      )

      io_reg.reg_rd := var_savedRDAddr
      io_reg.reg_write_en := true.B
      io_reg.reg_write_data := var_result

      io_pc.pc_we := true.B
      io_pc.pc_wdata := var_savedPC + 4.U

      io.stall := STALL_REASON.NO_STALL
      var_state := sIdle
    }
  }
}
