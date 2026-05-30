package project2.RISCV.RV32I

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RISCV.model.ALU_CONTROL
import RISCV.implementation.RV32I.ALU
import project2.utils.TraceSim

class ALUTest extends AnyFlatSpec with ChiselSim with TraceSim with Matchers {
  behavior of "ALU"

  it should "perform addition correctly" in {
    simulate(new ALU) { dut =>
      enableWaves()
      dut.io_alu.alu_op.poke(ALU_CONTROL.ADD)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(8.U)
    }
  }

  it should "perform subtraction correctly" in {
    simulate(new ALU) { dut =>
      enableWaves()
      dut.io_alu.alu_op.poke(ALU_CONTROL.SUB)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(2.U)
    }
  }

  it should "perform bitwise AND correctly" in {
    simulate(new ALU) { dut =>
      enableWaves()
      dut.io_alu.alu_op.poke(ALU_CONTROL.AND)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(1.U)
    }
  }

  it should "perform bitwise OR correctly" in {
    simulate(new ALU) { dut =>
      enableWaves()
      dut.io_alu.alu_op.poke(ALU_CONTROL.OR)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(7.U)
    }
  }

  it should "perform unsigned less than correctly" in {
    simulate(new ALU) { dut =>
      enableWaves()
      dut.io_alu.alu_op.poke(ALU_CONTROL.SLTU)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(0.U)
    }
  }

  it should "return zero for unknown ALU control code" in {
    simulate(new ALU) { dut =>
      enableWaves()
      dut.io_alu.alu_op.poke(ALU_CONTROL.UNKNOWN)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(0.U)
    }
  }
}
