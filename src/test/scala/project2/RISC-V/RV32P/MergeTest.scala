package project2.RISCV.RV32P

import chisel3._
import chisel3.util._

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RISCV.utils.assembler.RISCVAssembler
import RISCV.implementation.Core
import RISCV.implementation.RV32I._
import RISCV.implementation.RV32P._
import project2.utils.ProcessorTestUtils._
import project2.utils.RISCVInstruction._
import project2.utils.TraceSim
import project2.utils.models.RVFI

class MergeTest extends AnyFlatSpec with ChiselSim with TraceSim with Matchers {

  private def mask32(value: Long): Long = value & 0xffffffffL

  private def packedRegisters(overrides: (Int, Long)*): Map[Int, Long] = {
    generateRandomMap(0xffffffffL) ++ overrides.toMap
  }

  private def runMergeTest(
      instructionText: String,
      registers: Map[Int, Long],
      // optional explicit lists for registers/read destinations
      rsSeq: Seq[(Int, Long)] = Seq.empty,
      rdSeq: Seq[(Int, Long)] = Seq.empty,
      waitForRetireFlag: Boolean = false
  ): Unit = {
    simulate(
      new Core(
        Seq(
          () =>
            new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU),
          () => new MergeUnit
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString(instructionText).replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      if (waitForRetireFlag) {
        waitForRetire(dut)
      }

      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U(64.W)
        insn = instr
        // vectorized rs fields
        rs_seq = rsSeq.map { case (addr, data) =>
          (addr.U(5.W), signedToUnsigned(data, 32).U(32.W))
        }
        rs_rcount = rsSeq.length.U

        rd_seq = rdSeq.map { case (addr, data) =>
          (addr.U(5.W), signedToUnsigned(data, 32).U(32.W))
        }
        rd_wcount = rdSeq.length.U
        pc_rdata = state.pc.U(32.W)
        pc_wdata = (state.pc + 4).U(32.W)

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "mvm" in {
    val rdVal = 0xf0f0f0f0L
    val rs1Val = 0xaaaaaaaaL
    val rs2Val = 0x0f0f0f0fL
    val expected = mask32((~rs2Val & rdVal) | (rs2Val & rs1Val))
    runMergeTest(
      "mvm x1, x2, x3",
      packedRegisters(1 -> rdVal, 2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((1, rdVal), (2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = true
    )
  }

  it should "mvmn" in {
    val rdVal = 0xf0f0f0f0L
    val rs1Val = 0xaaaaaaaaL
    val rs2Val = 0x0f0f0f0fL
    val expected = mask32((~rs2Val & rs1Val) | (rs2Val & rdVal))
    runMergeTest(
      "mvmn x1, x2, x3",
      packedRegisters(1 -> rdVal, 2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((1, rdVal), (2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = true
    )
  }

  it should "merge" in {
    val rdVal = 0x0f0f0f0fL
    val rs1Val = 0xaaaaaaaaL
    val rs2Val = 0x55555555L
    val expected = mask32((~rdVal & rs1Val) | (rdVal & rs2Val))
    runMergeTest(
      "merge x1, x2, x3",
      packedRegisters(1 -> rdVal, 2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((1, rdVal), (2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = true
    )
  }

  it should "pack" in {
    val rs1Val = 0x12345678L
    val rs2Val = 0xabcdef01L
    val expected = mask32(((rs2Val & 0xffffL) << 16) | (rs1Val & 0xffffL))
    runMergeTest(
      "pack x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = false
    )
  }
}
