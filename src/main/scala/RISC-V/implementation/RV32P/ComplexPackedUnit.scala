package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model.InstructionSets
import RISCV.model.STALL_REASON
import RISCV.model.TRAP_REASON
import RISCV.model.RISCV_TYPE
import RISCV.model.RISCV_FORMAT

class ComplexPackedUnit extends AbstractExecutionUnit {

  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  val opcode = io.instr(6, 0)
  val funct3 = io.instr(14, 12)
  val funct7 = io.instr(31, 25)
  val rs1Addr = io.instr(19, 15)
  val rs2Addr = io.instr(24, 20)
  val rdAddr = io.instr(11, 7)

  val OP_IMM_32 = "b0011011".U(7.W)
  val OP_32 = "b0111011".U(7.W)

  val isShift =
    (opcode === OP_IMM_32 || opcode === OP_32) && (funct3 === "b010".U)
  val isPSSHA_HS = isShift && (funct7(1, 0) === "b00".U)
  val isPSSHAR_HS = isShift && (funct7(1, 0) === "b01".U)
  val isPSSHL_HS = isShift && (funct7(1, 0) === "b10".U)
  val isPSSHLR_HS = isShift && (funct7(1, 0) === "b11".U)

  val isClipFunct3 = (funct3 === "b001".U) || (funct3 === "b100".U)
  val isPNCLIPI_B =
    (opcode === OP_IMM_32 || opcode === OP_32) && isClipFunct3 && (funct7 === "b0110000".U)
  val isPNCLIPI_H =
    (opcode === OP_IMM_32 || opcode === OP_32) && isClipFunct3 && (funct7 === "b0110001".U)
  val isPNCLIPRI_B =
    (opcode === OP_IMM_32 || opcode === OP_32) && isClipFunct3 && (funct7 === "b0111000".U)
  val isPNCLIPRI_H =
    (opcode === OP_IMM_32 || opcode === OP_32) && isClipFunct3 && (funct7 === "b0111001".U)

  val isPM2ADD_H =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1000000".U)
  val isPM2ADDU_H =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1010000".U)
  val isPM2ADDSU_H =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1110000".U)
  val isPM2ADDA_H =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1000100".U)
  val isPM2ADDAU_H =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1010100".U)
  val isPM2ADDASU_H =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1110100".U)

  val isPM2ADD_HX =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1001000".U)
  val isPM2ADDA_HX =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1001100".U)
  val isPM2SADD_H =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1100010".U)
  val isPM2SADD_HX =
    (opcode === OP_32) && (funct3 === "b101".U) && (funct7 === "b1101010".U)

  val isSingleCycle = isShift ||
    isPNCLIPI_B || isPNCLIPI_H || isPNCLIPRI_B || isPNCLIPRI_H ||
    isPM2ADD_H || isPM2ADDSU_H || isPM2ADDU_H || isPM2ADD_HX ||
    isPM2SADD_H || isPM2SADD_HX

  val isMultiCycle =
    isPM2ADDA_H || isPM2ADDASU_H || isPM2ADDAU_H || isPM2ADDA_HX
  val isKnown = isSingleCycle || isMultiCycle

  val isClip = isPNCLIPI_B || isPNCLIPI_H || isPNCLIPRI_B || isPNCLIPRI_H

  io.valid := false.B
  io.stall := STALL_REASON.NO_STALL

  io_reg.reg_rs1 := Mux(isClip, rs1Addr & 30.U, rs1Addr)
  io_reg.reg_rs2 := Mux(isClip, rs1Addr | 1.U, rs2Addr)
  io_reg.reg_rd := rdAddr
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

  val sIdle :: sReadRD :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val savedRS1 = Reg(UInt(32.W))
  val savedRS2 = Reg(UInt(32.W))
  val savedRDAddr = Reg(UInt(5.W))
  val savedPC = Reg(UInt(32.W))
  val savedOp = Reg(UInt(4.W))

  val OP_ADDA_H = 0.U(4.W)
  val OP_ADDASU_H = 1.U(4.W)
  val OP_ADDAU_H = 2.U(4.W)
  val OP_ADDA_HX = 3.U(4.W)

  val rs1Data = io_reg.reg_read_data1
  val rs2Data = io_reg.reg_read_data2

  val rs1_l = rs1Data(15, 0)
  val rs1_h = rs1Data(31, 16)
  val rs2_l = rs2Data(15, 0)
  val rs2_h = rs2Data(31, 16)

  switch(state) {
    is(sIdle) {
      when(isKnown) {
        io.valid := true.B

        when(isSingleCycle) {

          val p1_s = rs1_l.asSInt * rs2_l.asSInt
          val p2_s = rs1_h.asSInt * rs2_h.asSInt
          val p1_u = rs1_l * rs2_l
          val p2_u = rs1_h * rs2_h
          val p1_su = rs1_l.asSInt * Cat(0.U(1.W), rs2_l).asSInt
          val p2_su = rs1_h.asSInt * Cat(0.U(1.W), rs2_h).asSInt
          val p1_x = rs1_l.asSInt * rs2_h.asSInt
          val p2_x = rs1_h.asSInt * rs2_l.asSInt

          val p1_s_ext = Wire(SInt(34.W)); p1_s_ext := p1_s
          val p2_s_ext = Wire(SInt(34.W)); p2_s_ext := p2_s
          val p1_x_ext = Wire(SInt(34.W)); p1_x_ext := p1_x
          val p2_x_ext = Wire(SInt(34.W)); p2_x_ext := p2_x

          val sum_s = p1_s_ext + p2_s_ext
          val sum_u = p1_u + p2_u
          val sum_su = p1_su + p2_su
          val sum_x = p1_x_ext + p2_x_ext

          val max32 = 2147483647.S(34.W)
          val min32 = -2147483648.S(34.W)
          val sat_sum_s =
            Mux(sum_s > max32, max32, Mux(sum_s < min32, min32, sum_s))
          val sat_sum_x =
            Mux(sum_x > max32, max32, Mux(sum_x < min32, min32, sum_x))

          val isArith = isPSSHA_HS || isPSSHAR_HS
          val isRnd = isPSSHAR_HS || isPSSHLR_HS

          def doShift(value: UInt, shamtRaw: UInt): UInt = {
            // spec says shift amount is rs2[7:0] interpreted as signed
            // negative = right shift, positive = left shift
            val shamt8 = shamtRaw(7, 0)
            val isRight = shamt8(7) // sign bit: 1 = negative = right shift
            val shAmtMag = Mux(isRight, (~shamt8 + 1.U)(4, 0), shamt8(4, 0))

            val sValue = value.asSInt
            val leftShiftedSInt = (sValue << shAmtMag)
            val logicLeft = (value << shAmtMag)(15, 0)

            val rightShiftedArith = (sValue >> shAmtMag).asUInt(15, 0)
            val rightShiftedLogic = (value >> shAmtMag)(15, 0)
            val rightShifted =
              Mux(isArith, rightShiftedArith, rightShiftedLogic)

            val valExt = Cat(0.U(1.W), value)
            val discardedBit =
              Mux(shAmtMag === 0.U, 0.U, (valExt >> (shAmtMag - 1.U))(0))
            val roundedRight =
              (rightShifted + Mux(isRnd && isRight, discardedBit, 0.U))(15, 0)

            val max16 = 32767.S(32.W)
            val min16 = -32768.S(32.W)
            val satLeft = Mux(
              leftShiftedSInt > max16,
              max16.asUInt,
              Mux(leftShiftedSInt < min16, min16.asUInt, leftShiftedSInt.asUInt)
            )(15, 0)

            val leftResult = Mux(isArith, satLeft, logicLeft)
            Mux(isRight, roundedRight, leftResult)
          }

          val shift_h1 = doShift(rs1_h, rs2_l)
          val shift_h0 = doShift(rs1_l, rs2_l)

          val isClipB = isPNCLIPI_B || isPNCLIPRI_B
          val isClipH = isPNCLIPI_H || isPNCLIPRI_H
          val isClipRndB = isPNCLIPRI_B
          val isClipRndH = isPNCLIPRI_H
          val clip_shamt = Mux(isClipB, io.instr(23, 20), io.instr(24, 20))

          def clipShiftB(x: SInt): UInt = {
            val shifted = x >> clip_shamt
            val roundBit = Mux(
              clip_shamt === 0.U,
              0.S,
              Mux((x >> (clip_shamt - 1.U))(0), 1.S, 0.S)
            )
            val rounded = shifted + Mux(isClipRndB, roundBit, 0.S)
            val maxB = 127.S
            val minB = -128.S
            Mux(
              rounded > maxB,
              maxB.asUInt(7, 0),
              Mux(rounded < minB, minB.asUInt(7, 0), rounded.asUInt(7, 0))
            )
          }

          def clipShiftH(x: SInt): UInt = {
            val shifted = x >> clip_shamt
            val roundBit = Mux(
              clip_shamt === 0.U,
              0.S,
              Mux((x >> (clip_shamt - 1.U))(0), 1.S, 0.S)
            )
            val rounded = shifted + Mux(isClipRndH, roundBit, 0.S)
            val maxH = 32767.S
            val minH = -32768.S
            Mux(
              rounded > maxH,
              maxH.asUInt(15, 0),
              Mux(rounded < minH, minH.asUInt(15, 0), rounded.asUInt(15, 0))
            )
          }

          val clipB_h3 = clipShiftB(rs2Data(31, 16).asSInt)
          val clipB_h2 = clipShiftB(rs2Data(15, 0).asSInt)
          val clipB_h1 = clipShiftB(rs1Data(31, 16).asSInt)
          val clipB_h0 = clipShiftB(rs1Data(15, 0).asSInt)

          val clipH_w1 = clipShiftH(rs2Data.asSInt)
          val clipH_w0 = clipShiftH(rs1Data.asSInt)

          val singleResult = MuxCase(
            0.U(32.W),
            Seq(
              isPM2ADD_H -> sum_s(31, 0),
              isPM2ADDU_H -> sum_u(31, 0),
              isPM2ADDSU_H -> sum_su(31, 0),
              isPM2ADD_HX -> sum_x(31, 0),
              isPM2SADD_H -> sat_sum_s(31, 0),
              isPM2SADD_HX -> sat_sum_x(31, 0),
              isShift -> Cat(shift_h1, shift_h0),
              (isPNCLIPI_B || isPNCLIPRI_B) -> Cat(
                clipB_h3,
                clipB_h2,
                clipB_h1,
                clipB_h0
              ),
              (isPNCLIPI_H || isPNCLIPRI_H) -> Cat(clipH_w1, clipH_w0)
            )
          )

          io_reg.reg_write_en := true.B
          io_reg.reg_write_data := singleResult
          io_pc.pc_we := true.B

        }.elsewhen(isMultiCycle) {
          savedRS1 := rs1Data
          savedRS2 := rs2Data
          savedRDAddr := rdAddr
          savedPC := io_pc.pc
          savedOp := MuxCase(
            OP_ADDA_H,
            Seq(
              isPM2ADDAU_H -> OP_ADDAU_H,
              isPM2ADDASU_H -> OP_ADDASU_H,
              isPM2ADDA_HX -> OP_ADDA_HX
            )
          )

          io_pc.pc_we := false.B
          io.stall := STALL_REASON.EXECUTION_UNIT
          state := sReadRD
        }
      }
    }

    is(sReadRD) {
      io.valid := true.B

      io_reg.reg_rs1 := savedRDAddr
      io_reg.reg_rs2 := 0.U

      val rdPrior = io_reg.reg_read_data1
      val s_rs1_l = savedRS1(15, 0)
      val s_rs1_h = savedRS1(31, 16)
      val s_rs2_l = savedRS2(15, 0)
      val s_rs2_h = savedRS2(31, 16)

      val s_p1_s = s_rs1_l.asSInt * s_rs2_l.asSInt
      val s_p2_s = s_rs1_h.asSInt * s_rs2_h.asSInt
      val s_p1_u = s_rs1_l * s_rs2_l
      val s_p2_u = s_rs1_h * s_rs2_h
      val s_p1_su = s_rs1_l.asSInt * Cat(0.U(1.W), s_rs2_l).asSInt
      val s_p2_su = s_rs1_h.asSInt * Cat(0.U(1.W), s_rs2_h).asSInt
      val s_p1_x = s_rs1_l.asSInt * s_rs2_h.asSInt
      val s_p2_x = s_rs1_h.asSInt * s_rs2_l.asSInt

      val accumulateResult = MuxCase(
        0.U(32.W),
        Seq(
          (savedOp === OP_ADDA_H) -> (rdPrior.asSInt + s_p1_s + s_p2_s).asUInt,
          (savedOp === OP_ADDAU_H) -> (rdPrior + s_p1_u + s_p2_u).asUInt,
          (savedOp === OP_ADDASU_H) -> (rdPrior.asSInt + s_p1_su + s_p2_su).asUInt,
          (savedOp === OP_ADDA_HX) -> (rdPrior.asSInt + s_p1_x + s_p2_x).asUInt
        )
      )

      io_reg.reg_rd := savedRDAddr
      io_reg.reg_write_en := true.B
      io_reg.reg_write_data := accumulateResult

      io_pc.pc_we := true.B
      io_pc.pc_wdata := savedPC + 4.U

      io.stall := STALL_REASON.NO_STALL
      state := sIdle
    }
  }
}
