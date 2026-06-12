package RISCV.implementation.generic.caches

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractCache

class Prefetcher extends AbstractCache {

  val addr0 = RegInit(0.U(32.W))
  val addr1 = RegInit(0.U(32.W))
  val addr2 = RegInit(0.U(32.W))
  val valid_count = RegInit(0.U(2.W))

  val prefetching = RegInit(false.B)
  val stored_prefetch_addr = RegInit(0.U(32.W))

  val pending_req = RegInit(false.B)
  val pending_addr = RegInit(0.U(32.W))
  val pending_we = RegInit(false.B)
  val pending_be = RegInit(0.U(4.W))
  val pending_wdata = RegInit(0.U(32.W))

  // tracks an in-flight core read so history commits even after data_req drops
  val read_active = RegInit(false.B)
  val read_addr = RegInit(0.U(32.W))

  val cur_stride = addr2 - addr1

  core_io.data_gnt := false.B
  core_io.data_rdata := 0.U
  mem_io.data_req := false.B
  mem_io.data_addr := 0.U
  mem_io.data_be := 0.U
  mem_io.data_we := false.B
  mem_io.data_wdata := 0.U

  when(prefetching) {
    mem_io.data_req := true.B
    mem_io.data_addr := stored_prefetch_addr
    mem_io.data_be := "hF".U
    mem_io.data_we := false.B

    when(mem_io.data_gnt) {
      prefetching := false.B
    }

    when(core_io.data_req && !pending_req) {
      pending_req := true.B
      pending_addr := core_io.data_addr
      pending_we := core_io.data_we
      pending_be := core_io.data_be
      pending_wdata := core_io.data_wdata
    }
  }.elsewhen(pending_req) {
    mem_io.data_req := true.B
    mem_io.data_addr := pending_addr
    mem_io.data_be := pending_be
    mem_io.data_we := pending_we
    mem_io.data_wdata := pending_wdata

    when(mem_io.data_gnt) {
      core_io.data_gnt := true.B
      core_io.data_rdata := mem_io.data_rdata
      pending_req := false.B
    }
  }.elsewhen(core_io.data_req) {
    mem_io.data_req := true.B
    mem_io.data_addr := core_io.data_addr
    mem_io.data_be := core_io.data_be
    mem_io.data_we := core_io.data_we
    mem_io.data_wdata := core_io.data_wdata

    when(mem_io.data_gnt) {
      core_io.data_gnt := true.B
      core_io.data_rdata := mem_io.data_rdata
    }

    when(!mem_io.data_gnt && !core_io.data_we) {
      read_active := true.B
      read_addr := core_io.data_addr
    }
  }

  // commit a completed read into the address history, independent of data_req
  when(read_active && mem_io.data_gnt) {
    read_active := false.B

    val cnt = valid_count
    val do_pf = cnt >= 2.U &&
      (read_addr - addr2 === cur_stride) &&
      cur_stride =/= 0.U

    addr0 := addr1
    addr1 := addr2
    addr2 := read_addr
    valid_count := Mux(cnt < 3.U, cnt + 1.U, 3.U)

    when(do_pf) {
      prefetching := true.B
      stored_prefetch_addr := read_addr + cur_stride
    }
  }

  when(~io_reset.rst_n) {
    addr0 := 0.U
    addr1 := 0.U
    addr2 := 0.U
    valid_count := 0.U
    prefetching := false.B
    stored_prefetch_addr := 0.U
    pending_req := false.B
    pending_addr := 0.U
    pending_we := false.B
    pending_be := 0.U
    pending_wdata := 0.U
    read_active := false.B
    read_addr := 0.U
  }
}
