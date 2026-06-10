package RISCV.implementation.generic

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic._
import RISCV.model._

/** <p><b>GenericCore</b> is an abstract class representing a Core module.</p>
  * <p>It provides the basic structure and interface for implementing a
  * processor core.</p>
  *
  * <h4>IO</h4> <ul> <li>[[interfaces.core_interface.GeneralInterface]]:
  * Interface providing general signals.</li>
  * <li>[[interfaces.core_interface.InstructionInterface]]: Interface providing
  * signals related to instruction handling.</li>
  * <li>[[interfaces.core_interface.DataInterface]]: Interface providing signals
  * related to data access.</li>
  * <li>[[interfaces.core_interface.RVFIInterface]]: Interface providing signals
  * for the RISC-V formal interface (RVFI).</li> </ul>
  *
  * @param genExecutionUnits
  *   A sequence of generators for execution units.
  * @param genProgramCounter
  *   A function that generates an instance of the ProgramCounter module.
  * @param genRegisterFile
  *   A function that generates an instance of the RegisterFile module.
  */
abstract class GenericCore(
    genExecutionUnits: Seq[() => AbstractExecutionUnit],
    genProgramCounter: => AbstractProgramCounter,
    genRegisterFile: => AbstractRegisterFile,
    genCaches: Seq[() => AbstractCache],
    rdPorts: Int = 2,
    rsPorts: Int = 4
) extends Module {

  val io_reset = IO(new ResetInterface)
  val io_instr = IO(new InstructionInterface)
  val io_rvfi = IO(new RVFIInterface(rdPorts, rsPorts))
  val io_data = IO(new DataInterface)

  val core_data = Wire(new DataInterface)

  val executionUnits = genExecutionUnits.map(gen => Module(gen()))

  val caches = genCaches.map(gen => Module(gen()))
  var current_cache_io = core_data
  caches.foreach(cache => {
    cache.core_io <> current_cache_io
    current_cache_io = Wire(new DataInterface)
    cache.mem_io <> current_cache_io
    cache.io_reset <> io_reset
  })
  current_cache_io <> io_data

  val pc = Module(genProgramCounter)
  pc.io_reset.rst_n := io_reset.rst_n
  pc.io_reset.boot_addr := io_reset.boot_addr

  pc.io.pc_we := false.B
  pc.io.pc_wdata := io_reset.boot_addr

  val register_file = Module(genRegisterFile)
  register_file.io_reset.rst_n := io_reset.rst_n
  register_file.io_reset.boot_addr := io_reset.boot_addr

  register_file.io.reg_rs1 := 0.U
  register_file.io.reg_rs2 := 0.U
  register_file.io.reg_rd := 0.U
  register_file.io.reg_write_en := false.B
  register_file.io.reg_write_data := 0.U

  core_data.data_addr := 0.U
  core_data.data_req := false.B
  core_data.data_we := false.B
  core_data.data_be := 0.U
  core_data.data_wdata := 0.U

  val misa = Wire(UInt(32.W))
  misa := executionUnits.map(_.io.misa).reduce(_ | _)

  val trap_valid = Wire(Bool())
  trap_valid := false.B
  val trap_reason = Wire(TRAP_REASON())
  trap_reason := TRAP_REASON.NONE

  val stall = Wire(STALL_REASON())
  when(io_instr.instr_gnt) {
    stall := STALL_REASON.NO_STALL
  }.otherwise {
    stall := STALL_REASON.INSTR_REQ
  }

  val next_instruction = Wire(UInt(32.W))
  val instruction = RegNext(next_instruction)
  next_instruction := io_instr.instr_rdata
  when(~io_reset.rst_n) {
    instruction := Fill(32, 1.U)
  }.elsewhen(
    stall === STALL_REASON.NO_STALL || stall === STALL_REASON.INSTR_REQ
  ) {
    when(io_instr.instr_gnt) {
      next_instruction := io_instr.instr_rdata
    }.otherwise {
      next_instruction := Fill(32, 1.U)
    }
  }.elsewhen(stall === STALL_REASON.EXECUTION_UNIT) {
    next_instruction := instruction
  }

  val data_gnt = RegNext(core_data.data_gnt)
  val data_rdata = RegNext(core_data.data_rdata)

  io_instr.instr_req := pc.io.pc_we || stall === STALL_REASON.INSTR_REQ
  io_instr.instr_addr := pc.io.pc_wdata

  val instr_type = Wire(RISCV_TYPE())
  instr_type := RISCV_TYPE.instr_to_riscvtype(instruction)

  val instruction_valid = Wire(Bool())
  instruction_valid := false.B

  executionUnits.foreach(unit => {
    unit.io.instr := instruction
    unit.io.instr_type := instr_type

    unit.io_pc.pc := pc.io.pc

    unit.io_reg.reg_read_data1 := register_file.io.reg_read_data1
    unit.io_reg.reg_read_data2 := register_file.io.reg_read_data2

    unit.io_data.data_gnt := data_gnt
    unit.io_data.data_rdata := data_rdata

    unit.io_reset.rst_n := io_reset.rst_n
    unit.io_reset.boot_addr := io_reset.boot_addr

    when(unit.io.valid) {
      instruction_valid := true.B
      stall := unit.io.stall

      pc.io.pc_we := unit.io_pc.pc_we
      pc.io.pc_wdata := unit.io_pc.pc_wdata

      register_file.io.reg_rs1 := unit.io_reg.reg_rs1
      register_file.io.reg_rs2 := unit.io_reg.reg_rs2
      register_file.io.reg_rd := unit.io_reg.reg_rd
      register_file.io.reg_write_en := unit.io_reg.reg_write_en
      register_file.io.reg_write_data := unit.io_reg.reg_write_data

      core_data.data_req := unit.io_data.data_req
      core_data.data_addr := unit.io_data.data_addr
      core_data.data_be := unit.io_data.data_be
      core_data.data_we := unit.io_data.data_we
      core_data.data_wdata := unit.io_data.data_wdata

      trap_valid := unit.io_trap.trap_valid
      trap_reason := unit.io_trap.trap_reason
    }
  })

  when(~io_reset.rst_n && ~instruction_valid) {
    trap_valid := true.B
    trap_reason := TRAP_REASON.ILLEGAL_INSTRUCTION
  }

  when(trap_valid) {
    pc.io.pc_we := true.B
    pc.io.pc_wdata := "h00400000".U // Set PC to 0x4000000 on trap
    register_file.io.reg_write_en := false.B
    register_file.io.reg_write_data := 0.U
    core_data.data_req := false.B
    core_data.data_addr := 0.U
    core_data.data_we := false.B
    core_data.data_be := 0.U
    core_data.data_wdata := 0.U
  }

  io_rvfi.rvfi_valid := stall === STALL_REASON.NO_STALL

  val order = RegInit(0.U(64.W))
  when(~io_reset.rst_n) {
    order := 0.U
  }.elsewhen(stall === STALL_REASON.NO_STALL) {
    order := order + 1.U
  }
  io_rvfi.rvfi_order := order - 1.U
  io_rvfi.rvfi_insn := instruction
  io_rvfi.rvfi_trap := trap_valid
  io_rvfi.rvfi_halt := false.B
  io_rvfi.rvfi_intr := false.B
  io_rvfi.rvfi_mode := 0.U
  io_rvfi.rvfi_ixl := 0.U

  io_rvfi.rvfi_rs1_addr := register_file.io.reg_rs1
  io_rvfi.rvfi_rs2_addr := register_file.io.reg_rs2
  io_rvfi.rvfi_rs1_rdata := register_file.io.reg_read_data1
  io_rvfi.rvfi_rs2_rdata := register_file.io.reg_read_data2

  val rs_addr_reg = RegInit(VecInit(Seq.fill(rsPorts)(0.U(5.W))))
  val rs_rdata_reg = RegInit(VecInit(Seq.fill(rsPorts)(0.U(32.W))))
  val rs_rcount_reg = RegInit(0.U(log2Ceil(rsPorts + 1).W))

  val rs_addr_next = Wire(Vec(rsPorts, UInt(5.W)))
  val rs_rdata_next = Wire(Vec(rsPorts, UInt(32.W)))
  val rs_rcount_next = Wire(UInt(log2Ceil(rsPorts + 1).W))

  rs_addr_next := rs_addr_reg
  rs_rdata_next := rs_rdata_reg
  rs_rcount_next := rs_rcount_reg

  val rs_capture = instruction_valid
  if (rsPorts == 1) {
    when(rs_capture && (rs_rcount_reg < 1.U)) {
      rs_addr_next(0) := register_file.io.reg_rs1
      rs_rdata_next(0) := register_file.io.reg_read_data1
      rs_rcount_next := 1.U
    }
  } else {
    val rs_count_after_rs1 = Wire(UInt(log2Ceil(rsPorts + 1).W))
    val rs_count_after_rs2 = Wire(UInt(log2Ceil(rsPorts + 1).W))
    rs_count_after_rs1 := rs_rcount_reg
    rs_count_after_rs2 := rs_count_after_rs1

    when(rs_capture && (rs_rcount_reg < rsPorts.U)) {
      val rs_index = rs_rcount_reg(log2Ceil(rsPorts) - 1, 0)
      rs_addr_next(rs_index) := register_file.io.reg_rs1
      rs_rdata_next(rs_index) := register_file.io.reg_read_data1
      rs_count_after_rs1 := rs_rcount_reg + 1.U
    }

    when(rs_capture && (rs_count_after_rs1 < rsPorts.U)) {
      val rs_index = rs_count_after_rs1(log2Ceil(rsPorts) - 1, 0)
      rs_addr_next(rs_index) := register_file.io.reg_rs2
      rs_rdata_next(rs_index) := register_file.io.reg_read_data2
      rs_count_after_rs2 := rs_count_after_rs1 + 1.U
    }

    rs_rcount_next := rs_count_after_rs2
  }

  when(~io_reset.rst_n) {
    rs_addr_reg := VecInit(Seq.fill(rsPorts)(0.U(5.W)))
    rs_rdata_reg := VecInit(Seq.fill(rsPorts)(0.U(32.W)))
    rs_rcount_reg := 0.U
  }.elsewhen(io_rvfi.rvfi_valid) {
    rs_addr_reg := VecInit(Seq.fill(rsPorts)(0.U(5.W)))
    rs_rdata_reg := VecInit(Seq.fill(rsPorts)(0.U(32.W)))
    rs_rcount_reg := 0.U
  }.otherwise {
    rs_addr_reg := rs_addr_next
    rs_rdata_reg := rs_rdata_next
    rs_rcount_reg := rs_rcount_next
  }

  io_rvfi.rvfi_rs_addr := rs_addr_next
  io_rvfi.rvfi_rs_rdata := rs_rdata_next
  io_rvfi.rvfi_rs_rcount := Mux(io_rvfi.rvfi_valid, rs_rcount_next, 0.U)

  val rd_addr_reg = RegInit(VecInit(Seq.fill(rdPorts)(0.U(5.W))))
  val rd_wdata_reg = RegInit(VecInit(Seq.fill(rdPorts)(0.U(32.W))))
  val rd_wcount_reg = RegInit(0.U(log2Ceil(rdPorts + 1).W))

  val rd_addr_next = Wire(Vec(rdPorts, UInt(5.W)))
  val rd_wdata_next = Wire(Vec(rdPorts, UInt(32.W)))
  val rd_wcount_next = Wire(UInt(log2Ceil(rdPorts + 1).W))

  rd_addr_next := rd_addr_reg
  rd_wdata_next := rd_wdata_reg
  rd_wcount_next := rd_wcount_reg

  val reg_write_valid =
    register_file.io.reg_write_en & (register_file.io.reg_rd =/= 0.U)
  if (rdPorts == 1) {
    when(reg_write_valid && (rd_wcount_reg < 1.U)) {
      rd_addr_next(0) := register_file.io.reg_rd
      rd_wdata_next(0) := register_file.io.reg_write_data
      rd_wcount_next := 1.U
    }
  } else {
    val rd_index = rd_wcount_reg(log2Ceil(rdPorts) - 1, 0)
    when(reg_write_valid && (rd_wcount_reg < rdPorts.U)) {
      rd_addr_next(rd_index) := register_file.io.reg_rd
      rd_wdata_next(rd_index) := register_file.io.reg_write_data
      rd_wcount_next := rd_wcount_reg + 1.U
    }
  }

  when(~io_reset.rst_n) {
    rd_addr_reg := VecInit(Seq.fill(rdPorts)(0.U(5.W)))
    rd_wdata_reg := VecInit(Seq.fill(rdPorts)(0.U(32.W)))
    rd_wcount_reg := 0.U
  }.elsewhen(io_rvfi.rvfi_valid) {
    rd_addr_reg := VecInit(Seq.fill(rdPorts)(0.U(5.W)))
    rd_wdata_reg := VecInit(Seq.fill(rdPorts)(0.U(32.W)))
    rd_wcount_reg := 0.U
  }.otherwise {
    rd_addr_reg := rd_addr_next
    rd_wdata_reg := rd_wdata_next
    rd_wcount_reg := rd_wcount_next
  }

  io_rvfi.rvfi_rd_addr := rd_addr_next
  io_rvfi.rvfi_rd_wdata := rd_wdata_next
  io_rvfi.rvfi_rd_wcount := Mux(io_rvfi.rvfi_valid, rd_wcount_next, 0.U)

  io_rvfi.rvfi_pc_rdata := pc.io.pc
  io_rvfi.rvfi_pc_wdata := pc.io.pc_wdata

  val had_mem_req = RegInit(false.B)
  when(~io_reset.rst_n) {
    had_mem_req := false.B
  }.elsewhen(core_data.data_req) {
    had_mem_req := true.B
  }.elsewhen(io_rvfi.rvfi_valid) {
    had_mem_req := false.B
  }

  val mem_addr = RegInit(0.U(32.W))
  when(~io_reset.rst_n) {
    mem_addr := 0.U
  }.elsewhen(core_data.data_req) {
    mem_addr := core_data.data_addr
  }

  val mem_rmask = RegInit(0.U(4.W))
  val mem_rdata = RegInit(0.U(32.W))
  val mem_wmask = RegInit(0.U(4.W))
  val mem_wdata = RegInit(0.U(32.W))
  val mem_active_mask = Mux(core_data.data_req, core_data.data_be, mem_rmask)
  when(~io_reset.rst_n) {
    mem_rmask := 0.U
    mem_rdata := 0.U
    mem_wmask := 0.U
    mem_wdata := 0.U
  }.elsewhen(core_data.data_req & core_data.data_we) {
    mem_rmask := 0.U
    mem_wmask := core_data.data_be
    mem_wdata := core_data.data_wdata & (Fill(8, core_data.data_be(3)) ## Fill(
      8,
      core_data.data_be(2)
    ) ## Fill(8, core_data.data_be(1)) ## Fill(8, core_data.data_be(0)))
  }.elsewhen(core_data.data_req) {
    mem_rmask := core_data.data_be
    mem_wmask := 0.U
    mem_wdata := 0.U
  }

  when(core_data.data_gnt & ~core_data.data_we) {
    mem_rdata := core_data.data_rdata & (Fill(8, mem_active_mask(3)) ## Fill(
      8,
      mem_active_mask(2)
    ) ## Fill(8, mem_active_mask(1)) ## Fill(8, mem_active_mask(0)))
  }.elsewhen(io_rvfi.rvfi_valid) {
    mem_rdata := 0.U
  }

  when(had_mem_req) {
    io_rvfi.rvfi_mem_addr := mem_addr
    io_rvfi.rvfi_mem_rmask := mem_rmask
    io_rvfi.rvfi_mem_wmask := mem_wmask
    io_rvfi.rvfi_mem_rdata := mem_rdata
    io_rvfi.rvfi_mem_wdata := mem_wdata
  }.otherwise {
    io_rvfi.rvfi_mem_addr := 0.U
    io_rvfi.rvfi_mem_rmask := 0.U
    io_rvfi.rvfi_mem_wmask := 0.U
    io_rvfi.rvfi_mem_rdata := 0.U
    io_rvfi.rvfi_mem_wdata := 0.U
  }
}
