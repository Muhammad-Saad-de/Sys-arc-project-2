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

class SimplePackedTest
    extends AnyFlatSpec
    with ChiselSim
    with TraceSim
    with Matchers {

  private def packedRegisters(overrides: (Int, Long)*): Map[Int, Long] = {
    generateRandomMap(0xffffffffL) ++ overrides.toMap
  }

  private def runPackedTest(
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
          () => new SimplePackedUnit
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

  it should "pli.b" in {
    runPackedTest(
      "pli.b x1, 0x12",
      packedRegisters(),
      rsSeq = Seq(),
      rdSeq = Seq((1, 0x12121212L)),
      waitForRetireFlag = false
    )
  }

  it should "pli.h" in {
    runPackedTest(
      "pli.h x1, 0x134",
      packedRegisters(),
      rsSeq = Seq(),
      rdSeq = Seq((1, 0x01340134L)),
      waitForRetireFlag = false
    )
  }

  it should "plui.h" in {
    runPackedTest(
      "plui.h x1, -1",
      packedRegisters(),
      rsSeq = Seq(),
      rdSeq = Seq((1, 0xffc0ffc0L)),
      waitForRetireFlag = false
    )
  }

  it should "padd.b" in {
    runPackedTest(
      "padd.b x1, x2, x3",
      packedRegisters(2 -> 0x10203040L, 3 -> 0x01020304L),
      rsSeq = Seq((2, 0x10203040L), (3, 0x01020304L)),
      rdSeq = Seq((1, 0x11223344L)),
      waitForRetireFlag = false
    )
  }

  it should "padd.h" in {
    runPackedTest(
      "padd.h x1, x2, x3",
      packedRegisters(2 -> 0x10002000L, 3 -> 0x00010002L),
      rsSeq = Seq((2, 0x10002000L), (3, 0x00010002L)),
      rdSeq = Seq((1, 0x10012002L)),
      waitForRetireFlag = false
    )
  }

  it should "padd.bs" in {
    runPackedTest(
      "padd.bs x1, x2, x3",
      packedRegisters(2 -> 0x11223344L, 3 -> 0x00000010L),
      rsSeq = Seq((2, 0x11223344L), (3, 0x00000010L)),
      rdSeq = Seq((1, 0x21324354L)),
      waitForRetireFlag = false
    )
  }

  it should "padd.hs" in {
    runPackedTest(
      "padd.hs x1, x2, x3",
      packedRegisters(2 -> 0x11223344L, 3 -> 0x00000100L),
      rsSeq = Seq((2, 0x11223344L), (3, 0x00000100L)),
      rdSeq = Seq((1, 0x12223444L)),
      waitForRetireFlag = false
    )
  }

  it should "psub.b" in {
    runPackedTest(
      "psub.b x1, x2, x3",
      packedRegisters(2 -> 0x10203040L, 3 -> 0x01020304L),
      rsSeq = Seq((2, 0x10203040L), (3, 0x01020304L)),
      rdSeq = Seq((1, 0x0f1e2d3cL)),
      waitForRetireFlag = false
    )
  }

  it should "psub.h" in {
    runPackedTest(
      "psub.h x1, x2, x3",
      packedRegisters(2 -> 0x10002000L, 3 -> 0x00010002L),
      rsSeq = Seq((2, 0x10002000L), (3, 0x00010002L)),
      rdSeq = Seq((1, 0x0fff1ffeL)),
      waitForRetireFlag = false
    )
  }

  it should "psadd.b" in {
    runPackedTest(
      "psadd.b x1, x2, x3",
      packedRegisters(2 -> 0xfe01807fL, 3 -> 0xfeffff01L),
      rsSeq = Seq((2, 0xfe01807fL), (3, 0xfeffff01L)),
      rdSeq = Seq((1, 0xfc00807fL)),
      waitForRetireFlag = false
    )
  }

  it should "psadd.h" in {
    runPackedTest(
      "psadd.h x1, x2, x3",
      packedRegisters(2 -> 0x80007fffL, 3 -> 0xffff0001L),
      rsSeq = Seq((2, 0x80007fffL), (3, 0xffff0001L)),
      rdSeq = Seq((1, 0x80007fffL)),
      waitForRetireFlag = false
    )
  }

  it should "paadd.b" in {
    runPackedTest(
      "paadd.b x1, x2, x3",
      packedRegisters(2 -> 0xff01807fL, 3 -> 0x00020001L),
      rsSeq = Seq((2, 0xff01807fL), (3, 0x00020001L)),
      rdSeq = Seq((1, 0xff01c040L)),
      waitForRetireFlag = false
    )
  }

  it should "paadd.h" in {
    runPackedTest(
      "paadd.h x1, x2, x3",
      packedRegisters(2 -> 0x80007fffL, 3 -> 0x00000001L),
      rsSeq = Seq((2, 0x80007fffL), (3, 0x00000001L)),
      rdSeq = Seq((1, 0xc0004000L)),
      waitForRetireFlag = false
    )
  }
}
