package project2.RISCV.RV32I

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RISCV.utils.assembler.RISCVAssembler
import RISCV.model._
import RISCV.implementation.RV32I._
import RISCV.implementation.Core
import project2.utils.models._
import project2.utils.TraceSim

class InitialTest
    extends AnyFlatSpec
    with ChiselSim
    with TraceSim
    with Matchers {

  def testInstructions(instructions: String, expected_state: Seq[RVFI]) = {
    val assembly = RISCVAssembler.fromString(instructions).split("\n")
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)
      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)

      assembly
        .zip(expected_state)
        .foreach(
          { case (instr, state) =>
            dut.io_instr.instr_rdata.poke(BigInt(instr, 16).U)
            dut.io_instr.instr_gnt.poke(true.B)

            dut.clock.step()

            dut.io_rvfi.rvfi_valid.expect(true.B)
            dut.io_rvfi.rvfi_order.expect(state.order)
            dut.io_rvfi.rvfi_insn.expect(state.insn)
            dut.io_rvfi.rvfi_trap.expect(state.trap)
            dut.io_rvfi.rvfi_halt.expect(state.halt)
            dut.io_rvfi.rvfi_intr.expect(state.intr)
            dut.io_rvfi.rvfi_mode.expect(state.mode)
            dut.io_rvfi.rvfi_ixl.expect(state.ixl)

            dut.io_rvfi.rvfi_rs1_addr.expect(state.rs1_addr)
            dut.io_rvfi.rvfi_rs2_addr.expect(state.rs2_addr)
            dut.io_rvfi.rvfi_rs1_rdata.expect(state.rs1_rdata)
            dut.io_rvfi.rvfi_rs2_rdata.expect(state.rs2_rdata)

            dut.io_rvfi.rvfi_rd_addr(0).expect(state.rd_addr)
            dut.io_rvfi.rvfi_rd_wdata(0).expect(state.rd_wdata)

            dut.io_rvfi.rvfi_pc_rdata.expect(state.pc_rdata)
            dut.io_rvfi.rvfi_pc_wdata.expect(state.pc_wdata)
          }
        )
    }
  }

  it should "be a ALU" in {
    simulate(new ALU) { dut =>
      enableWaves()
      dut.io_alu.alu_op.poke(ALU_CONTROL.ADD)
      dut.io_alu.op2.poke(3.U)
      dut.io_alu.op1.poke(5.U)
      dut.clock.step()
      dut.io_alu.result.expect(8.U)
    }
  }

  it should "be a Decoder" in {
    simulate(new Decoder) { dut =>
      enableWaves()
      dut.io_decoder.instr.poke(0x00000013.U)
      dut.clock.step()
      dut.io_decoder.rs1.expect(0.U)
      dut.io_decoder.rs2.expect(0.U)
      dut.io_decoder.rd.expect(0.U)
      dut.io_decoder.imm.expect(0.U)
    }
  }

  it should "decode a negative value" in {
    simulate(new Decoder) { dut =>
      enableWaves()
      val instr = BigInt(
        RISCVAssembler.fromString("bne x17 x4 0x-4").replace("\n", ""),
        16
      ).U
      dut.io_decoder.instr.poke(instr)
      dut.clock.step()
      dut.io_decoder.rs1.expect(17.U)
      dut.io_decoder.rs2.expect(4.U)
      dut.io_decoder.rd.expect(0.U)
      dut.io_decoder.imm.expect(BigInt("FFFFFFFC", 16).U)
    }
  }

  it should "detect a branch" in {
    simulate(new ControlUnit) { dut =>
      enableWaves()
      dut.io_ctrl.instr_type.poke(RISCV_TYPE.beq)
      dut.clock.step()
      dut.io_ctrl.next_pc_select.expect(NEXT_PC_SELECT.BRANCH)
    }
  }

  it should "use x0 as rs1 for jumps" in {
    simulate(new Decoder) { dut =>
      enableWaves()
      dut.io_decoder.instr.poke(0xabba00efL.U)
      dut.clock.step()
      dut.io_decoder.rs1.expect(0.U)
    }

  }

  it should "assemble RISC-V" in {
    val hex = RISCVAssembler.fromString("addi x0, x0, 0")
    hex should be("00000013\n")
  }

  it should "execute a nop" in {
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(nop, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

    }
  }

  it should "execute a few instructions" in {
    val addi = RISCVAssembler.fromString("addi x1, x0, 0x0ab").split("\n")(0)
    val add = RISCVAssembler.fromString("add x2, x1, x1").split("\n")(0)
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(BigInt(addi, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(BigInt(add, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(BigInt(addi, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(1.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0x0ab.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(8.U)
      dut.io_instr.instr_rdata.poke(0x00000013.U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(1.U)
      dut.io_rvfi.rvfi_insn.expect(BigInt(add, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(1.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(1.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0x0ab.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0x0ab.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(2.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0x00000156.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(4.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(8.U)
    }
  }

  it should "branch" in {
    val beq = RISCVAssembler.fromString("beq x1, x2, 0x08").split("\n")(0)
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(BigInt(beq, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0x08.U)
      dut.io_instr.instr_rdata.poke(0x00000013.U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(BigInt(beq, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(1.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(2.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(0x08.U)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0x0c.U)
      dut.io_instr.instr_rdata.poke(0x00000013.U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(1.U)
      dut.io_rvfi.rvfi_insn.expect(0x00000013.U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0x08.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(0x0c.U)
    }
  }

  it should "execute instructions" in {
    val instructions = """
        |addi x1, x0, 0x0ab
        |add x2, x1, x1
        |beq x1, x2, 0x08
        """.stripMargin
    val assembly = RISCVAssembler.fromString(instructions).split("\n")
    val expected_state = Seq(
      new RVFI {
        valid = true.B
        order = 0.U
        insn = BigInt(assembly(0), 16).U
        rs1_addr = 0.U
        rs2_addr = 0.U
        rs1_rdata = 0.U
        rs2_rdata = 0.U
        rd_addr = 1.U
        rd_wdata = 0x0ab.U
        pc_wdata = 0x04.U
      },
      new RVFI {
        valid = true.B
        order = 1.U
        insn = BigInt(assembly(1), 16).U
        rs1_addr = 1.U
        rs2_addr = 1.U
        rs1_rdata = 0x0ab.U
        rs2_rdata = 0x0ab.U
        rd_addr = 2.U
        rd_wdata = (0x0ab + 0x0ab).U
        pc_rdata = 0x04.U
        pc_wdata = 0x08.U
      },
      new RVFI {
        valid = true.B
        order = 2.U
        insn = BigInt(assembly(2), 16).U
        rs1_addr = 1.U
        rs2_addr = 2.U
        rs1_rdata = 0x0ab.U
        rs2_rdata = (0x0ab + 0x0ab).U
        rd_addr = 0.U
        rd_wdata = 0.U
        pc_rdata = 0x08.U
        pc_wdata = 0x0c.U
      }
    )
    testInstructions(instructions, expected_state)
  }

  it should "stall if no instruction is available" in {
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(0.U)
      dut.io_instr.instr_gnt.poke(false.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(nop, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

    }
  }

  it should "trap on a misaligned jal" in {
    val jal = RISCVAssembler.fromString("jal x1, 6").split("\n")(0)
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(0.U)
      dut.io_instr.instr_gnt.poke(false.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(jal, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect("h00400000".U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(jal, 16).U)
      dut.io_rvfi.rvfi_trap.expect(true.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect("h00400000".U)

    }
  }

  it should "trap on a misaligned beq" in {
    val beq = RISCVAssembler.fromString("beq zero, zero, 6").split("\n")(0)
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(0.U)
      dut.io_instr.instr_gnt.poke(false.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(beq, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect("h00400000".U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(beq, 16).U)
      dut.io_rvfi.rvfi_trap.expect(true.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect("h00400000".U)

    }
  }

  it should "not trap on a non-taken misaligned bne" in {
    val bne = RISCVAssembler.fromString("bne zero, zero, 6").split("\n")(0)
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    simulate(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ) { dut =>
      enableWaves()
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(0.U)
      dut.io_instr.instr_gnt.poke(false.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(bne, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(bne, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr(0).expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata(0).expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

    }
  }
}
