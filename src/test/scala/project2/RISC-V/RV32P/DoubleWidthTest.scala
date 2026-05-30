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

class DoubleWidthTest
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
          () => new DoubleWidthUnit
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

  it should "pli.db" in {
    runPackedTest(
      "pli.db x2, 0x12",
      packedRegisters(),
      rsSeq = Seq(),
      rdSeq = Seq((2, 0x12121212L), (3, 0x12121212L)),
      waitForRetireFlag = true
    ) // also writes register 3
  }

  it should "padd.db" in {
    runPackedTest(
      "padd.db x6, x2, x4",
      packedRegisters(
        2 -> 0x01020304L,
        3 -> 0x01020304L,
        4 -> 0x01020304L,
        5 -> 0x01020304L
      ),
      rsSeq = Seq(
        (2, 0x01020304L),
        (3, 0x01020304L),
        (4, 0x01020304L),
        (5, 0x01020304L)
      ),
      rdSeq = Seq((6, 0x02040608L), (7, 0x02040608L)),
      waitForRetireFlag = true
    ) // also writes register
  }

  it should "padd.dbs" in {
    runPackedTest(
      "padd.dbs x8, x4, x6",
      packedRegisters(
        4 -> 0x00112233L,
        5 -> 0x00112233L,
        6 -> 0x00000001L
      ),
      rsSeq = Seq((4, 0x00112233L), (5, 0x00112233L), (6, 0x00000001L)),
      rdSeq = Seq((8, 0x01122334L), (9, 0x01122334L)),
      waitForRetireFlag = true
    ) // also writes register 9
  }

  it should "psadd.dw" in {
    runPackedTest(
      "psadd.dw x10, x2, x4",
      packedRegisters(
        2 -> 0x00000010L,
        3 -> 0x00000010L,
        4 -> 0x00000020L,
        5 -> 0x00000020L
      ),
      rsSeq = Seq(
        (2, 0x00000010L),
        (3, 0x00000010L),
        (4, 0x00000020L),
        (5, 0x00000020L)
      ),
      rdSeq = Seq((10, 0x00000030L), (11, 0x00000030L)),
      waitForRetireFlag = true
    ) // also writes register 11
  }

  it should "paadd.db" in {
    runPackedTest(
      "paadd.db x12, x2, x4",
      packedRegisters(
        2 -> 0x00020004L,
        3 -> 0x00020004L,
        4 -> 0x00020104L,
        5 -> 0x00020104L
      ),
      rsSeq = Seq(
        (2, 0x00020004L),
        (3, 0x00020004L),
        (4, 0x00020104L),
        (5, 0x00020104L)
      ),
      rdSeq = Seq((12, 0x00020004L), (13, 0x00020004L)),
      waitForRetireFlag = true
    ) // also writes register 13
  }

  it should "pwadd.b" in {
    runPackedTest(
      "pwadd.b x14, x2, x3",
      packedRegisters(
        2 -> 0x01020304L,
        3 -> 0x05060708L
      ),
      rsSeq = Seq((2, 0x01020304L), (3, 0x05060708L)),
      rdSeq = Seq((14, 0x000a000cL), (15, 0x00060008L)),
      waitForRetireFlag = true
    )
  }

  it should "pwadda.b" in {
    runPackedTest(
      "pwadda.b x16, x2, x3",
      packedRegisters(
        2 -> 0x01020304L,
        3 -> 0x05060708L,
        16 -> 0x00010002L,
        17 -> 0x00030004L
      ),
      rsSeq = Seq(
        (2, 0x01020304L),
        (3, 0x05060708L),
        (16, 0x00010002L),
        (17, 0x00030004L)
      ),
      rdSeq = Seq((16, 0x000b000eL), (17, 0x0009000cL)),
      waitForRetireFlag = true
    )
  }

}
