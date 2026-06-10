package project2.utils

import scala.util.Random

import chisel3._
import chisel3.util._
import chisel3.simulator.ChiselSim

import scala.collection.mutable

import RISCV.utils.assembler.RISCVAssembler
import RISCV.implementation.generic.GenericCore
import project2.utils.models._

object ProcessorTestUtils extends ChiselSim {
  private def expectSignal(
      signal: Data,
      expected: Data,
      fieldName: String
  ): Unit = {
    signal.expect(
      expected,
      "Expected " + fieldName + " to be " + expected.litValue.toString(
        16
      ) + " but got " + signal.peek().litValue.toString(16)
    )
  }

  def resetCore(core: GenericCore): Unit = {
    core.io_reset.boot_addr.poke(0.U)
    core.io_reset.rst_n.poke(false.B)
    core.io_instr.instr_gnt.poke(false.B)
    core.io_data.data_gnt.poke(false.B)
    core.clock.step()
    core.io_reset.rst_n.poke(true.B)
    core.clock.step()
  }

  def executeInstruction(core: GenericCore, instruction: UInt): Unit = {
    core.io_instr.instr_rdata.poke(instruction)
    core.io_instr.instr_gnt.poke(true.B)
    core.clock.step()
  }

  def waitForRetire(core: GenericCore): Unit = {
    while (!core.io_rvfi.rvfi_valid.peek().litToBoolean) {
      core.clock.step()
    }
  }

  def evaluateRVFI(core: GenericCore, expected_state: RVFI): Unit = {
    expectSignal(core.io_rvfi.rvfi_valid, expected_state.valid, "rvfi_valid")
    expectSignal(core.io_rvfi.rvfi_order, expected_state.order, "rvfi_order")
    expectSignal(core.io_rvfi.rvfi_insn, expected_state.insn, "rvfi_insn")
    expectSignal(core.io_rvfi.rvfi_trap, expected_state.trap, "rvfi_trap")
    expectSignal(core.io_rvfi.rvfi_halt, expected_state.halt, "rvfi_halt")
    expectSignal(core.io_rvfi.rvfi_intr, expected_state.intr, "rvfi_intr")
    expectSignal(core.io_rvfi.rvfi_mode, expected_state.mode, "rvfi_mode")
    expectSignal(core.io_rvfi.rvfi_ixl, expected_state.ixl, "rvfi_ixl")

    if (expected_state.rs_seq != Seq()) {
      // check that every expected read has a matching actual read
      val expectedPairs = expected_state.rs_seq.map { case (addr, data) =>
        (addr.litValue, data.litValue)
      }.toSet ++ Set(
        (BigInt(0L), BigInt(0L))
      ) // add a dummy pair to allow for the case where no rs reads are expected
      val actualPairs =
        (0 until core.io_rvfi.rvfi_rs_rcount.peek().litValue.toInt).map { i =>
          val addr = core.io_rvfi.rvfi_rs_addr(i).peek().litValue
          val data = core.io_rvfi.rvfi_rs_rdata(i).peek().litValue
          (addr, data)
        }.toSet ++ Set(
          (BigInt(0L), BigInt(0L))
        ) // add a dummy pair to allow for the case where no rs reads are expected
      require(
        expectedPairs == actualPairs,
        "Expected rvfi rs reads " + expectedPairs
          .map { case (addr, data) => s"(${addr}, 0x${data.toString(16)})" }
          .mkString(", ") + " but got " + actualPairs
          .map { case (addr, data) => s"($addr, 0x${data.toString(16)})" }
          .mkString(", ")
      )
    } else {
      // Normal case: check rs1 and rs2
      expectSignal(
        core.io_rvfi.rvfi_rs1_addr,
        expected_state.rs1_addr,
        "rvfi_rs1_addr"
      )
      expectSignal(
        core.io_rvfi.rvfi_rs1_rdata,
        expected_state.rs1_rdata,
        "rvfi_rs1_rdata"
      )
      expectSignal(
        core.io_rvfi.rvfi_rs2_addr,
        expected_state.rs2_addr,
        "rvfi_rs2_addr"
      )
      expectSignal(
        core.io_rvfi.rvfi_rs2_rdata,
        expected_state.rs2_rdata,
        "rvfi_rs2_rdata"
      )
    }

    if (expected_state.rd_seq != Seq()) {
      // check that every expected write has a matching actual write
      val expectedPairs = expected_state.rd_seq.map { case (addr, data) =>
        (addr.litValue, data.litValue)
      }.toSet ++ Set(
        (BigInt(0L), BigInt(0L))
      ) // add a dummy pair to allow for cycles with no writes
      val actualPairs =
        (0 until core.io_rvfi.rvfi_rd_wcount.peek().litValue.toInt).map { i =>
          val addr = core.io_rvfi.rvfi_rd_addr(i).peek().litValue
          val data = core.io_rvfi.rvfi_rd_wdata(i).peek().litValue
          (addr, data)
        }.toSet ++ Set(
          (BigInt(0L), BigInt(0L))
        ) // add a dummy pair to allow for cycles with no writes
      require(
        expectedPairs == actualPairs,
        "Expected rvfi rd writes " + expectedPairs
          .map { case (addr, data) => s"(${addr}, 0x${data.toString(16)})" }
          .mkString(", ") + " but got " + actualPairs
          .map { case (addr, data) => s"($addr, 0x${data.toString(16)})" }
          .mkString(", ")
      )
    } else {
      // Normal case: check rd
      expectSignal(
        core.io_rvfi.rvfi_rd_addr(0),
        expected_state.rd_addr,
        "rvfi_rd_addr"
      )
      expectSignal(
        core.io_rvfi.rvfi_rd_wdata(0),
        expected_state.rd_wdata,
        "rvfi_rd_wdata"
      )
    }

    expectSignal(
      core.io_rvfi.rvfi_pc_rdata,
      expected_state.pc_rdata,
      "rvfi_pc_rdata"
    )
    expectSignal(
      core.io_rvfi.rvfi_pc_wdata,
      expected_state.pc_wdata,
      "rvfi_pc_wdata"
    )

    expectSignal(
      core.io_rvfi.rvfi_mem_addr,
      expected_state.mem_addr,
      "rvfi_mem_addr"
    )
    expectSignal(
      core.io_rvfi.rvfi_mem_rmask,
      expected_state.mem_rmask,
      "rvfi_mem_rmask"
    )
    expectSignal(
      core.io_rvfi.rvfi_mem_wmask,
      expected_state.mem_wmask,
      "rvfi_mem_wmask"
    )
    expectSignal(
      core.io_rvfi.rvfi_mem_rdata,
      expected_state.mem_rdata,
      "rvfi_mem_rdata"
    )
    expectSignal(
      core.io_rvfi.rvfi_mem_wdata,
      expected_state.mem_wdata,
      "rvfi_mem_wdata"
    )
  }

  def expectMemoryRequest(core: GenericCore, expected_state: RVFI): Unit = {
    expectSignal(core.io_rvfi.rvfi_valid, false.B, "rvfi_valid")
    expectSignal(core.io_data.data_req, true.B, "data_req")
    expectSignal(
      core.io_data.data_we,
      if (expected_state.mem_wmask.litValue != 0) true.B else false.B,
      "data_we"
    )
    expectSignal(
      core.io_data.data_be,
      (expected_state.mem_rmask.litValue | expected_state.mem_wmask.litValue)
        .U(4.W),
      "data_be"
    )
    expectSignal(core.io_data.data_addr, expected_state.mem_addr, "data_addr")
    expectSignal(
      core.io_data.data_wdata,
      expected_state.mem_wdata,
      "data_wdata"
    )
    core.io_data.data_rdata.poke(expected_state.mem_rdata)
    core.io_data.data_gnt.poke(expected_state.mem_rmask.litValue != 0)
    core.clock.step()
  }

  def executeAndEvaluate(
      core: GenericCore,
      instruction: UInt,
      expected_state: RVFI
  ): Unit = {
    executeInstruction(core, instruction)
    evaluateRVFI(core, expected_state)
  }

  def executeAndEvaluateMemory(
      core: GenericCore,
      instruction: UInt,
      expected_state: RVFI
  ): Unit = {
    executeInstruction(core, instruction)
    expectMemoryRequest(core, expected_state)
    while (!core.io_rvfi.rvfi_valid.peek().litToBoolean) {
      core.clock.step()
    }
    evaluateRVFI(core, expected_state)
  }

  def generateRandomMap(limit: Long): Map[Int, Long] =
    Random.shuffle((1 to 31).toList).map(i => i -> Random.nextLong(limit)).toMap

  def prepareState(
      core: GenericCore,
      registers: Map[Int, Long]
  ): ProcessorState = {
    var state = new ProcessorState()
    var lui = registers.map {
      case (k, v) => {
        var upper_val = (v >>> 12) & 0xfffff
        val has_bit_12_set = (v & 0x800) != 0
        upper_val = if (has_bit_12_set) upper_val + 1 else upper_val
        BigInt(
          RISCVAssembler
            .fromString("lui x" + k + " " + upper_val)
            .split("\n")(0),
          16
        ).U
      }
    }
    lui.foreach(instr => state = executeInstruction(core, instr, state, 0))
    var addi = registers.map {
      case (k, v) => {
        val lower_val = v & 0xfff
        BigInt(
          RISCVAssembler
            .fromString("addi x" + k + " x" + k + " " + lower_val)
            .split("\n")(0),
          16
        ).U
      }
    }
    addi.foreach(instr => state = executeInstruction(core, instr, state, 0))
    return state
  }

  def repeatBits(input: Int): Int =
    (0 until 4).map(i => ((input >> i) & 1) * 0xff << (i * 8)).reduce(_ | _)

  def load(state: ProcessorState, address: BigInt, mask: BigInt): BigInt = {
    // Initialize a result accumulator
    var result: BigInt = 0
    var shiftAmount = 0

    for (i <- 0 until 4) {
      // Check if the current byte needs to be read based on the mask
      if ((mask & (0xff << (i * 8))) != 0) {
        // Calculate the byte address (address + i) and read the byte
        val byteAddress = address + i
        val offset = (byteAddress % 4).toInt
        val aligned_address = byteAddress - offset
        val word = state.data_mem.getOrElse(aligned_address, BigInt(0))
        val byte = (word >> (offset * 8)) & 0xff
        result |= (byte & 0xff) << shiftAmount
      }
      shiftAmount += 8
    }
    return result
  }

  def store(
      state: ProcessorState,
      address: BigInt,
      mask: BigInt,
      data: BigInt
  ): mutable.Map[BigInt, BigInt] = {
    for (i <- 0 until 4) {
      // Check if the current byte needs to be written based on the mask
      if ((mask & (0xff << (i * 8))) != 0) {
        // Calculate the byte address (address + i) and read the byte
        val byteAddress = address + i
        val offset = (byteAddress % 4).toInt
        val aligned_address = byteAddress - offset
        val word = state.data_mem.get(aligned_address).getOrElse(BigInt(0))
        val byte = (data >> (i * 8)) & 0xff
        val new_word = (word & ~(0xff << (offset * 8))) | (byte << (offset * 8))
        state.data_mem += (aligned_address -> new_word)
      }
    }
    return state.data_mem
  }

  def decodeMemoryAccess(
      instruction: BigInt,
      rs1_val: BigInt,
      rs2_val: BigInt,
      previous_state: ProcessorState
  ): (BigInt, BigInt, BigInt, BigInt, BigInt) = {
    val opcode = instruction & 0x7f
    val funct3 = (instruction >> 12) & 0x7

    def signedImm(value: BigInt, width: Int): BigInt = {
      val signBit = BigInt(1) << (width - 1)
      val mask = (BigInt(1) << width) - 1
      val trimmed = value & mask
      if ((trimmed & signBit) != 0) trimmed - (BigInt(1) << width) else trimmed
    }

    def unsignedMask(width: Int): BigInt = (BigInt(1) << width) - 1

    def loadValue(address: BigInt, byteMask: BigInt): BigInt = {
      load(previous_state, address, repeatBits(byteMask.toInt))
    }

    opcode match {
      case opcodeValue if opcodeValue == BigInt(0x03) =>
        val imm = signedImm(instruction >> 20, 12)
        val address = rs1_val + imm
        funct3 match {
          case funct3Value if funct3Value == BigInt(0x0) =>
            (
              address,
              BigInt(0x1),
              BigInt(0),
              loadValue(address, BigInt(0x1)),
              BigInt(0)
            )
          case funct3Value if funct3Value == BigInt(0x1) =>
            (
              address,
              BigInt(0x3),
              BigInt(0),
              loadValue(address, BigInt(0x3)),
              BigInt(0)
            )
          case funct3Value if funct3Value == BigInt(0x2) =>
            (
              address,
              BigInt(0xf),
              BigInt(0),
              loadValue(address, BigInt(0xf)),
              BigInt(0)
            )
          case funct3Value if funct3Value == BigInt(0x4) =>
            (
              address,
              BigInt(0x1),
              BigInt(0),
              loadValue(address, BigInt(0x1)),
              BigInt(0)
            )
          case funct3Value if funct3Value == BigInt(0x5) =>
            (
              address,
              BigInt(0x3),
              BigInt(0),
              loadValue(address, BigInt(0x3)),
              BigInt(0)
            )
          case _ => (BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0))
        }
      case opcodeValue if opcodeValue == BigInt(0x23) =>
        val immLow = (instruction >> 7) & 0x1f
        val immHigh = (instruction >> 25) & 0x7f
        val imm = signedImm((immHigh << 5) | immLow, 12)
        val address = rs1_val + imm
        val writeData = rs2_val & unsignedMask(32)
        funct3 match {
          case funct3Value if funct3Value == BigInt(0x0) =>
            (
              address,
              BigInt(0),
              BigInt(0x1),
              BigInt(0),
              writeData & unsignedMask(8)
            )
          case funct3Value if funct3Value == BigInt(0x1) =>
            (
              address,
              BigInt(0),
              BigInt(0x3),
              BigInt(0),
              writeData & unsignedMask(16)
            )
          case funct3Value if funct3Value == BigInt(0x2) =>
            (address, BigInt(0), BigInt(0xf), BigInt(0), writeData)
          case _ => (BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0))
        }
      case _ => (BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0))
    }
  }

  def executeInstruction(
      core: GenericCore,
      instruction: UInt,
      previous_state: ProcessorState,
      mem_delay: Int
  ): ProcessorState = {
    core.io_instr.instr_rdata.poke(instruction)
    core.io_instr.instr_gnt.poke(true.B)
    core.clock.step()
    var delay = mem_delay
    while (!core.io_rvfi.rvfi_valid.peek().litToBoolean) {
      if (core.io_instr.instr_req.peek().litToBoolean) {
        core.io_instr.instr_gnt.poke(true.B)
        core.io_instr.instr_rdata.poke(instruction)
      } else {
        core.io_instr.instr_gnt.poke(false.B)
        core.io_instr.instr_rdata.poke(instruction)
      }
      if (core.io_data.data_req.peek().litToBoolean) {
        if (delay > 0) {
          core.io_data.data_gnt.poke(false.B)
          delay -= 1
        } else {
          if (!core.io_data.data_we.peek().litToBoolean) {
            val mask = repeatBits(core.io_data.data_be.peek().litValue.toInt)
            val read_address = core.io_data.data_addr.peek().litValue
            val read_data = load(previous_state, read_address, mask)
            core.io_data.data_rdata.poke(read_data.U)
          }
          core.io_data.data_gnt.poke(true.B)
          delay = mem_delay
        }
      } else {
        core.io_data.data_gnt.poke(false.B)
      }
      core.clock.step()
    }
    core.io_instr.instr_gnt.poke(false.B)
    core.io_data.data_gnt.poke(false.B)
    expectSignal(core.io_rvfi.rvfi_valid, true.B, "rvfi_valid")
    expectSignal(
      core.io_rvfi.rvfi_order,
      previous_state.retire_count.U,
      "rvfi_order"
    )
    previous_state.retire_count += 1
    val retired_insn = core.io_rvfi.rvfi_insn.peek().litValue
    expectSignal(core.io_rvfi.rvfi_insn, retired_insn.U, "rvfi_insn")
    expectSignal(core.io_rvfi.rvfi_halt, false.B, "rvfi_halt")
    expectSignal(core.io_rvfi.rvfi_trap, false.B, "rvfi_trap")
    expectSignal(core.io_rvfi.rvfi_intr, false.B, "rvfi_intr")
    expectSignal(core.io_rvfi.rvfi_mode, 0.U, "rvfi_mode")
    expectSignal(core.io_rvfi.rvfi_ixl, 0.U, "rvfi_ixl")

    expectSignal(
      core.io_rvfi.rvfi_pc_rdata,
      previous_state.pc.U,
      "rvfi_pc_rdata"
    )
    previous_state.pc = core.io_rvfi.rvfi_pc_wdata.peek().litValue.toInt

    // Read operand values from RVFI before updating state
    val rsReadPairs =
      (0 until core.io_rvfi.rvfi_rs_rcount.peek().litValue.toInt)
        .map(i =>
          core.io_rvfi.rvfi_rs_addr(i).peek().litValue -> core.io_rvfi
            .rvfi_rs_rdata(i)
            .peek()
            .litValue
        )
        .toMap
    def rsValueFor(addr: BigInt): BigInt = rsReadPairs.getOrElse(addr, 0)
    val rs1_addr = (retired_insn >> 15) & 0x1f
    val rs2_addr = (retired_insn >> 20) & 0x1f
    val rs1_value = rsValueFor(rs1_addr)
    val rs2_value = rsValueFor(rs2_addr)

    // Decode memory access using operand values from RVFI
    val (memAddr, memRmask, memWmask, memRdata, memWdata) =
      decodeMemoryAccess(retired_insn, rs1_value, rs2_value, previous_state)

    // Now update state with the instruction's results
    val rdWriteCount = core.io_rvfi.rvfi_rd_wcount.peek().litValue.toInt
    for (i <- 0 until rdWriteCount) {
      val rdAddr = core.io_rvfi.rvfi_rd_addr(i).peek().litValue.toInt
      val rdData = core.io_rvfi.rvfi_rd_wdata(i).peek().litValue
      if (rdAddr != 0) {
        previous_state.registers += (BigInt(rdAddr) -> rdData)
      }
    }
    if (memRmask != 0) {
      expectSignal(core.io_rvfi.rvfi_mem_addr, memAddr.U(32.W), "rvfi_mem_addr")
      expectSignal(
        core.io_rvfi.rvfi_mem_rmask,
        memRmask.U(4.W),
        "rvfi_mem_rmask"
      )
      expectSignal(
        core.io_rvfi.rvfi_mem_rdata,
        memRdata.U(32.W),
        "rvfi_mem_rdata"
      )
      expectSignal(core.io_rvfi.rvfi_mem_wmask, 0.U(4.W), "rvfi_mem_wmask")
      expectSignal(core.io_rvfi.rvfi_mem_wdata, 0.U(32.W), "rvfi_mem_wdata")
    } else if (memWmask != 0) {
      expectSignal(core.io_rvfi.rvfi_mem_addr, memAddr.U(32.W), "rvfi_mem_addr")
      expectSignal(core.io_rvfi.rvfi_mem_rmask, 0.U(4.W), "rvfi_mem_rmask")
      expectSignal(core.io_rvfi.rvfi_mem_rdata, 0.U(32.W), "rvfi_mem_rdata")
      expectSignal(
        core.io_rvfi.rvfi_mem_wmask,
        memWmask.U(4.W),
        "rvfi_mem_wmask"
      )
      expectSignal(
        core.io_rvfi.rvfi_mem_wdata,
        memWdata.U(32.W),
        "rvfi_mem_wdata"
      )
      previous_state.data_mem =
        store(previous_state, memAddr, repeatBits(memWmask.toInt), memWdata)
    } else {
      expectSignal(core.io_rvfi.rvfi_mem_addr, 0.U, "rvfi_mem_addr")
      expectSignal(core.io_rvfi.rvfi_mem_rmask, 0.U, "rvfi_mem_rmask")
      expectSignal(core.io_rvfi.rvfi_mem_rdata, 0.U, "rvfi_mem_rdata")
      expectSignal(core.io_rvfi.rvfi_mem_wmask, 0.U, "rvfi_mem_wmask")
      expectSignal(core.io_rvfi.rvfi_mem_wdata, 0.U, "rvfi_mem_wdata")
    }
    return previous_state
  }
}
