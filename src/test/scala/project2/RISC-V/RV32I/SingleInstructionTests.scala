package project2.RISCV.RV32I

import chisel3._
import chisel3.util._

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

import RISCV.implementation.Core
import RISCV.implementation.RV32I._
import RISCV.utils.assembler.RISCVAssembler

import project2.utils.RISCVInstruction.getSignedValue
import project2.utils.RISCVInstruction.signedToUnsigned
import project2.utils.RISCVInstruction
import project2.utils.models._
import project2.utils.ProcessorTestUtils._
import project2.utils.TraceSim

class SingleInstructionTests
    extends AnyFlatSpec
    with ChiselSim
    with TraceSim
    with Matchers {

  def instructions: Seq[() => ProcessorState => RISCVInstruction] = Seq(
    (() => RISCVInstruction.getADDI(_)),
    (() => RISCVInstruction.getSLTI(_)),
    (() => RISCVInstruction.getSLTIU(_)),
    (() => RISCVInstruction.getANDI(_)),
    (() => RISCVInstruction.getORI(_)),
    (() => RISCVInstruction.getXORI(_)),
    (() => RISCVInstruction.getSLLI(_)),
    (() => RISCVInstruction.getSRLI(_)),
    (() => RISCVInstruction.getSRAI(_)),
    (() => RISCVInstruction.getLUI(_)),
    (() => RISCVInstruction.getAUIPC(_)),
    (() => RISCVInstruction.getADD(_)),
    (() => RISCVInstruction.getSUB(_)),
    (() => RISCVInstruction.getSLL(_)),
    (() => RISCVInstruction.getSLT(_)),
    (() => RISCVInstruction.getSLTU(_)),
    (() => RISCVInstruction.getXOR(_)),
    (() => RISCVInstruction.getSRL(_)),
    (() => RISCVInstruction.getSRA(_)),
    (() => RISCVInstruction.getOR(_)),
    (() => RISCVInstruction.getAND(_)),
    (() => RISCVInstruction.getJAL(_)),
    (() => RISCVInstruction.getJALR(_)),
    (() => RISCVInstruction.getBEQ(_)),
    (() => RISCVInstruction.getBNE(_)),
    (() => RISCVInstruction.getBLT(_)),
    (() => RISCVInstruction.getBGE(_)),
    (() => RISCVInstruction.getBLTU(_)),
    (() => RISCVInstruction.getBGEU(_)),
    (() => RISCVInstruction.getLB(_)),
    (() => RISCVInstruction.getLH(_)),
    (() => RISCVInstruction.getLW(_)),
    (() => RISCVInstruction.getLBU(_)),
    (() => RISCVInstruction.getLHU(_)),
    (() => RISCVInstruction.getSB(_)),
    (() => RISCVInstruction.getSH(_)),
    (() => RISCVInstruction.getSW(_))
  )

  for (instr <- instructions) {
    val name = instr()(new ProcessorState).instruction.split(" ")(0)
    it should "do a random " + name in {
      simulate(
        new Core(
          Seq(() =>
            new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
          )
        )
      ) { dut =>
        enableWaves()
        val registers = generateRandomMap(
          signedToUnsigned(getSignedValue(0xffffffff, 32), 32)
        )
        resetCore(dut)
        val state = prepareState(dut, registers)
        val instruction = instr()(state)
        executeInstruction(
          dut,
          instruction.assembly,
          if (instruction.state != null) instruction.state else state,
          0
        )
        evaluateRVFI(dut, instruction.effect)
      }
    }
  }

  it should "lui" in {
    val imm = Random.nextInt(0x1000)
    val register = Random.nextInt(31) + 1
    val instr = BigInt(
      RISCVAssembler
        .fromString("lui x" + register + " 0x" + imm.toHexString)
        .split("\n")(0),
      16
    ).U
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val rvfi = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 0.U
        rs2_addr = 0.U
        rs1_rdata = 0.U
        rs2_rdata = 0.U
        rd_addr = register.U
        rd_wdata = (imm << 12).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U
      }
      executeAndEvaluate(dut, instr, rvfi)
    }
  }

  it should "addi" in {
    val instr =
      BigInt(RISCVAssembler.fromString("addi x1 x0 0x0ab").split("\n")(0), 16).U
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      val registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      resetCore(dut)
      val state = prepareState(dut, registers)

      val rvfi = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        trap = false.B
        halt = false.B
        intr = false.B
        mode = 0.U
        ixl = 0.U
        rs1_addr = 0.U
        rs2_addr = 0.U
        rs1_rdata = 0.U
        rs2_rdata = 0.U
        rd_addr = 1.U
        rd_wdata = 0x0ab.U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U
        mem_addr = 0.U
        mem_rmask = 0.U
        mem_wmask = 0.U
        mem_rdata = 0.U
        mem_wdata = 0.U
      }
      executeAndEvaluate(dut, instr, rvfi)
    }
  }

  it should "add" in {
    val instr =
      BigInt(RISCVAssembler.fromString("add x1 x0 x0").split("\n")(0), 16).U
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      val rvfi = new RVFI {
        valid = true.B
        order = 0.U
        insn = instr
        trap = false.B
        halt = false.B
        intr = false.B
        mode = 0.U
        ixl = 0.U
        rs1_addr = 0.U
        rs2_addr = 0.U
        rs1_rdata = 0.U
        rs2_rdata = 0.U
        rd_addr = 1.U
        rd_wdata = 0.U
        pc_rdata = 0.U
        pc_wdata = 4.U
        mem_addr = 0.U
        mem_rmask = 0.U
        mem_wmask = 0.U
        mem_rdata = 0.U
        mem_wdata = 0.U
      }
      resetCore(dut)
      executeAndEvaluate(dut, instr, rvfi)
    }
  }

  it should "lw" in {
    val instr =
      BigInt(RISCVAssembler.fromString("lw x1 0x100(x0)").split("\n")(0), 16).U
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      val rvfi = new RVFI {
        valid = true.B
        order = 0.U
        insn = instr
        trap = false.B
        halt = false.B
        intr = false.B
        mode = 0.U
        ixl = 0.U
        rs1_addr = 0.U
        rs2_addr = 0.U
        rs1_rdata = 0.U
        rs2_rdata = 0.U
        rd_addr = 1.U
        rd_wdata = 0xabcd.U
        pc_rdata = 0.U
        pc_wdata = 4.U
        mem_addr = 0x100.U
        mem_rmask = 0xf.U
        mem_wmask = 0.U
        mem_rdata = 0xabcd.U
        mem_wdata = 0.U
      }
      resetCore(dut)
      executeAndEvaluateMemory(dut, instr, rvfi)
    }
  }

  it should "still addi" in {
    val instr =
      BigInt(RISCVAssembler.fromString("addi x1 x0 0x0ab").split("\n")(0), 16).U
    var state = new ProcessorState();
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      state = executeInstruction(dut, instr, state, 0)
    }
  }

  it should "accumulate instructions" in {
    val instructions = """
            |addi x1, x0, 0x0ab
            |add x2, x1, x1
            |beq x1, x2, 0x08
            """.stripMargin
    val assembly = RISCVAssembler.fromString(instructions).split("\n")
    val instrs = assembly.map(x => BigInt(x, 16).U)
    var state = new ProcessorState();
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      for (instr <- instrs) {
        state = executeInstruction(dut, instr, state, 0)
      }
    }
  }

  it should "prepare the state" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      for ((k, v) <- registers) {
        state.registers(k) should be(v)
      }
    }
  }

  it should "prepare the state high" in {
    var registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    registers += (3 -> signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      for ((k, v) <- registers) {
        state.registers(k) should be(v)
      }
    }
  }

  it should "addi automatically" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getADDI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "slti" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getSLTI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "sltiu" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getSLTIU(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "andi" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getANDI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "ori" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getORI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "xori" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getXORI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "slli" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getSLLI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "srli" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getSRLI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

  it should "srai" in {
    val registers =
      generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = RISCVInstruction.getSRAI(state)
      executeAndEvaluate(dut, instr.assembly, instr.effect)
    }
  }

}
