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

class ComplexPackedTest
    extends AnyFlatSpec
    with ChiselSim
    with TraceSim
    with Matchers {

  private def packedRegisters(overrides: (Int, Long)*): Map[Int, Long] = {
    generateRandomMap(0xffffffffL) ++ overrides.toMap
  }

  private def mask32(value: Long): Long = value & 0xffffffffL

  private def s16(value: Int): Int = value.toShort.toInt

  private def clipSigned8(value: Int): Int = {
    if (value < -128) 0x80
    else if (value > 127) 0x7f
    else value & 0xff
  }

  private def clipSigned16(value: Long): Int = {
    if (value < -32768) 0x8000
    else if (value > 32767) 0x7fff
    else (value.toInt & 0xffff)
  }

  private def clipUnsigned16(value: Long): Int = {
    if (value > 0xffffL) 0xffff
    else value.toInt & 0xffff
  }

  private def roundRightSigned16(value: Int, shamt: Int): Int = {
    val x = value << 1
    val shifted = x >> shamt
    (shifted + 1) >> 1
  }

  private def roundRightUnsigned16(value: Int, shamt: Int): Int = {
    val x = (value & 0xffff) << 1
    val shifted = x >>> shamt
    (shifted + 1) >>> 1
  }

  private def psshaHs(rs1: Int, shamt: Int): Long = {
    val sshamt = shamt.toByte.toInt
    val lanes = (0 until 2).map { i =>
      val h = s16((rs1 >> (16 * i)) & 0xffff)
      if (sshamt < 0) {
        val neg = -sshamt
        if (neg >= 16) {
          if (h < 0) 0xffff else 0x0000
        } else {
          (h >> neg) & 0xffff
        }
      } else {
        val left = h.toLong << sshamt
        clipSigned16(left)
      }
    }
    ((lanes(1) << 16) | lanes(0)).toLong & 0xffffffffL
  }

  private def pssharHs(rs1: Int, shamt: Int): Long = {
    val sshamt = shamt.toByte.toInt
    val lanes = (0 until 2).map { i =>
      val h = s16((rs1 >> (16 * i)) & 0xffff)
      if (sshamt < 0) {
        val neg = math.min(16, -sshamt)
        roundRightSigned16(h, neg) & 0xffff
      } else {
        val left = h.toLong << sshamt
        clipSigned16(left)
      }
    }
    ((lanes(1) << 16) | lanes(0)).toLong & 0xffffffffL
  }

  private def psshlHs(rs1: Int, shamt: Int): Long = {
    val sshamt = shamt.toByte.toInt
    val lanes = (0 until 2).map { i =>
      val h = (rs1 >> (16 * i)) & 0xffff
      if (sshamt < 0) {
        val neg = -sshamt
        if (neg >= 16) 0 else (h >>> neg) & 0xffff
      } else {
        clipUnsigned16((h.toLong << sshamt) & 0xffffffffL)
      }
    }
    ((lanes(1) << 16) | lanes(0)).toLong & 0xffffffffL
  }

  private def psshlrHs(rs1: Int, shamt: Int): Long = {
    val sshamt = shamt.toByte.toInt
    val lanes = (0 until 2).map { i =>
      val h = (rs1 >> (16 * i)) & 0xffff
      if (sshamt < 0) {
        val neg = math.min(16, -sshamt)
        roundRightUnsigned16(h, neg) & 0xffff
      } else {
        clipUnsigned16((h.toLong << sshamt) & 0xffffffffL)
      }
    }
    ((lanes(1) << 16) | lanes(0)).toLong & 0xffffffffL
  }

  private def pnclipiB(s1: Long, shamt: Int, round: Boolean): Long = {
    val lanes = (0 until 4).map { i =>
      val h = s16(((s1 >> (16 * i)) & 0xffffL).toInt)
      val r = if (round) {
        roundRightSigned16(h, shamt)
      } else {
        h >> shamt
      }
      clipSigned8(r)
    }
    lanes.reverse.foldLeft(0L) { (acc, b) => (acc << 8) | (b & 0xff) }
  }

  private def pnclipiH(s1: Long, shamt: Int, round: Boolean): Long = {
    val lanes = (0 until 2).map { i =>
      val w = (s1 >> (32 * i)).toInt
      val r = if (round) {
        val x = (w.toLong << 1) >> shamt
        ((x + 1) >> 1).toInt
      } else {
        w >> shamt
      }
      clipSigned16(r.toLong)
    }
    ((lanes(1) << 16) | lanes(0)).toLong & 0xffffffffL
  }

  private def pm2add(
      rs1: Int,
      rs2: Int,
      signedA: Boolean,
      signedB: Boolean,
      cross: Boolean
  ): Long = {
    val a0 = if (signedA) s16(rs1 & 0xffff).toLong else (rs1 & 0xffff).toLong
    val a1 =
      if (signedA) s16((rs1 >> 16) & 0xffff).toLong
      else ((rs1 >> 16) & 0xffff).toLong
    val b0 = if (signedB) s16(rs2 & 0xffff).toLong else (rs2 & 0xffff).toLong
    val b1 =
      if (signedB) s16((rs2 >> 16) & 0xffff).toLong
      else ((rs2 >> 16) & 0xffff).toLong
    val p0 = if (cross) a0 * b1 else a0 * b0
    val p1 = if (cross) a1 * b0 else a1 * b1
    mask32(p0 + p1)
  }

  private def pm2sadd(rs1: Int, rs2: Int, cross: Boolean): Long = {
    val a0 = s16(rs1 & 0xffff)
    val a1 = s16((rs1 >> 16) & 0xffff)
    val b0 = s16(rs2 & 0xffff)
    val b1 = s16((rs2 >> 16) & 0xffff)
    val p0 = if (cross) a0.toLong * b1.toLong else a0.toLong * b0.toLong
    val p1 = if (cross) a1.toLong * b0.toLong else a1.toLong * b1.toLong
    val sum = p0.toLong + p1.toLong
    if (sum > Int.MaxValue.toLong) 0x7fffffffL
    else if (sum < Int.MinValue.toLong) 0x80000000L
    else mask32(sum)
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
          () => new ComplexPackedUnit
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

  it should "pssha.hs" in {
    val rs1Val = 0x20004000
    val rs2Val = 0x00000002
    val expected = psshaHs(rs1Val, rs2Val)
    runPackedTest(
      "pssha.hs x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "psshar.hs" in {
    val rs1Val = 0x00030001
    val rs2Val = 0x000000ff
    val expected = pssharHs(rs1Val, rs2Val)
    runPackedTest(
      "psshar.hs x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "psshl.hs" in {
    val rs1Val = 0x80000001
    val rs2Val = 0x00000001
    val expected = psshlHs(rs1Val, rs2Val)
    runPackedTest(
      "psshl.hs x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "psshlr.hs" in {
    val rs1Val = 0x00030002
    val rs2Val = 0x000000ff
    val expected = psshlrHs(rs1Val, rs2Val)
    runPackedTest(
      "psshlr.hs x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pnclipi.b" in {
    val low = 0x007f0080L
    val high = 0xff80007fL
    val s1 = (high << 32) | (low & 0xffffffffL)
    val expected = pnclipiB(s1, 1, round = false)
    runPackedTest(
      "pnclipi.b x1, x2, 1",
      packedRegisters(2 -> low, 3 -> high),
      rsSeq = Seq((2, low), (3, high)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pnclipi.h" in {
    val low = 0x00008000L
    val high = 0x80000000L
    val s1 = (high << 32) | (low & 0xffffffffL)
    val expected = pnclipiH(s1, 2, round = false)
    runPackedTest(
      "pnclipi.h x1, x2, 2",
      packedRegisters(2 -> low, 3 -> high),
      rsSeq = Seq((2, low), (3, high)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pnclipri.b" in {
    val low = 0x007f0080L
    val high = 0xff80007fL
    val s1 = (high << 32) | (low & 0xffffffffL)
    val expected = pnclipiB(s1, 2, round = true)
    runPackedTest(
      "pnclipri.b x1, x2, 2",
      packedRegisters(2 -> low, 3 -> high),
      rsSeq = Seq((2, low), (3, high)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pnclipri.h" in {
    val low = 0x00008000L
    val high = 0x80000000L
    val s1 = (high << 32) | (low & 0xffffffffL)
    val expected = pnclipiH(s1, 2, round = true)
    runPackedTest(
      "pnclipri.h x1, x2, 2",
      packedRegisters(2 -> low, 3 -> high),
      rsSeq = Seq((2, low), (3, high)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pm2add.h" in {
    val rs1Val = 0x00020003
    val rs2Val = 0x00040005
    val expected =
      pm2add(rs1Val, rs2Val, signedA = true, signedB = true, cross = false)
    runPackedTest(
      "pm2add.h x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pm2addsu.h" in {
    val rs1Val = 0xfffe0002
    val rs2Val = 0x00030004
    val expected =
      pm2add(rs1Val, rs2Val, signedA = true, signedB = false, cross = false)
    runPackedTest(
      "pm2addsu.h x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pm2addu.h" in {
    val rs1Val = 0xfffe0002
    val rs2Val = 0x00030004
    val expected =
      pm2add(rs1Val, rs2Val, signedA = false, signedB = false, cross = false)
    runPackedTest(
      "pm2addu.h x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pm2add.hx" in {
    val rs1Val = 0x00020003
    val rs2Val = 0x00040005
    val expected =
      pm2add(rs1Val, rs2Val, signedA = true, signedB = true, cross = true)
    runPackedTest(
      "pm2add.hx x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pm2sadd.h" in {
    val rs1Val = 0x80008000
    val rs2Val = 0x80008000
    val expected = pm2sadd(rs1Val, rs2Val, cross = false)
    runPackedTest(
      "pm2sadd.h x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pm2sadd.hx" in {
    val rs1Val = 0x80008000
    val rs2Val = 0x80008000
    val expected = pm2sadd(rs1Val, rs2Val, cross = true)
    runPackedTest(
      "pm2sadd.hx x1, x2, x3",
      packedRegisters(2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected))
    )
  }

  it should "pm2adda.h" in {
    val rs1Val = 0x00020003
    val rs2Val = 0x00040005
    val rdVal = 0x00000010L
    val expected = mask32(
      rdVal + pm2add(
        rs1Val,
        rs2Val,
        signedA = true,
        signedB = true,
        cross = false
      )
    )
    runPackedTest(
      "pm2adda.h x1, x2, x3",
      packedRegisters(1 -> rdVal, 2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((1, rdVal), (2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = true
    )
  }

  it should "pm2addasu.h" in {
    val rs1Val = 0xfffe0002
    val rs2Val = 0x00030004
    val rdVal = 0x00000010L
    val expected = mask32(
      rdVal + pm2add(
        rs1Val,
        rs2Val,
        signedA = true,
        signedB = false,
        cross = false
      )
    )
    runPackedTest(
      "pm2addasu.h x1, x2, x3",
      packedRegisters(1 -> rdVal, 2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((1, rdVal), (2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = true
    )
  }

  it should "pm2addau.h" in {
    val rs1Val = 0xfffe0002
    val rs2Val = 0x00030004
    val rdVal = 0x00000010L
    val expected = mask32(
      rdVal + pm2add(
        rs1Val,
        rs2Val,
        signedA = false,
        signedB = false,
        cross = false
      )
    )
    runPackedTest(
      "pm2addau.h x1, x2, x3",
      packedRegisters(1 -> rdVal, 2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((1, rdVal), (2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = true
    )
  }

  it should "pm2adda.hx" in {
    val rs1Val = 0x00020003
    val rs2Val = 0x00040005
    val rdVal = 0x00000010L
    val expected = mask32(
      rdVal + pm2add(
        rs1Val,
        rs2Val,
        signedA = true,
        signedB = true,
        cross = true
      )
    )
    runPackedTest(
      "pm2adda.hx x1, x2, x3",
      packedRegisters(1 -> rdVal, 2 -> rs1Val, 3 -> rs2Val),
      rsSeq = Seq((1, rdVal), (2, rs1Val), (3, rs2Val)),
      rdSeq = Seq((1, expected)),
      waitForRetireFlag = true
    )
  }
}
