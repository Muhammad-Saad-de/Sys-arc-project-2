package RISCV.implementation.generic.caches

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractCache
import RISCV.model.CACHE_STATE

class FIFOCache(capacity: Int) extends AbstractCache {

  val cache_addr = RegInit(VecInit(Seq.fill(capacity)(0.U(32.W))))
  val cache_data = RegInit(VecInit(Seq.fill(capacity)(0.U(32.W))))
  val cache_valid = RegInit(VecInit(Seq.fill(capacity)(false.B)))

  val head = RegInit(0.U(log2Ceil(capacity).W))
  val tail = RegInit(0.U(log2Ceil(capacity).W))
  val count = RegInit(0.U((log2Ceil(capacity) + 1).W))

  // state reg to track if we're waiting on memory
  val waiting = RegInit(false.B)
  val pending_we = RegInit(false.B)

  core_io.data_gnt := false.B
  core_io.data_rdata := 0.U

  mem_io.data_req := false.B
  mem_io.data_addr := 0.U
  mem_io.data_be := 0.U
  mem_io.data_we := false.B
  mem_io.data_wdata := 0.U

  val hit_vec = VecInit(cache_valid.zip(cache_addr).map { case (v, a) =>
    v && (a === core_io.data_addr)
  })
  val hit = hit_vec.asUInt.orR
  val hit_index = PriorityEncoder(hit_vec)

  def nextPtr(p: UInt): UInt = Mux(p === (capacity - 1).U, 0.U, p + 1.U)

  // helper to insert into cache, evicts oldest if full
  def insertIntoCache(addr: UInt, data: UInt): Unit = {
    when(count < capacity.U) {
      cache_addr(tail) := addr
      cache_data(tail) := data
      cache_valid(tail) := true.B
      tail := nextPtr(tail)
      count := count + 1.U
    }.otherwise {
      // cache full, evict oldest (head)
      cache_addr(head) := addr
      cache_data(head) := data
      cache_valid(head) := true.B
      head := nextPtr(head)
    }
  }

  when(waiting) {
    // keep forwarding the mem request til it gets granted
    mem_io.data_req := true.B
    mem_io.data_addr := core_io.data_addr
    mem_io.data_we := pending_we
    mem_io.data_wdata := core_io.data_wdata
    mem_io.data_be := Mux(pending_we, core_io.data_be, "hF".U)

    when(mem_io.data_gnt) {
      waiting := false.B
      core_io.data_gnt := true.B

      when(pending_we) {
        // write done, update cache if addr is in there
        val write_hit_vec =
          VecInit(cache_valid.zip(cache_addr).map { case (v, a) =>
            v && (a === core_io.data_addr)
          })
        val write_hit = write_hit_vec.asUInt.orR
        val write_hit_index = PriorityEncoder(write_hit_vec)

        when(write_hit) {
          // just update the data, dont change position
          cache_data(write_hit_index) := core_io.data_wdata
        }.otherwise {
          insertIntoCache(core_io.data_addr, core_io.data_wdata)
        }
      }.otherwise {
        // read miss resolved, store fetched word in cache
        core_io.data_rdata := mem_io.data_rdata
        insertIntoCache(core_io.data_addr, mem_io.data_rdata)
      }
    }
  }.otherwise {
    when(core_io.data_req) {
      when(core_io.data_we) {
        // writes always go to mem
        mem_io.data_req := true.B
        mem_io.data_addr := core_io.data_addr
        mem_io.data_we := true.B
        mem_io.data_wdata := core_io.data_wdata
        mem_io.data_be := core_io.data_be

        when(mem_io.data_gnt) {
          core_io.data_gnt := true.B

          val write_hit_vec =
            VecInit(cache_valid.zip(cache_addr).map { case (v, a) =>
              v && (a === core_io.data_addr)
            })
          val write_hit = write_hit_vec.asUInt.orR
          val write_hit_index = PriorityEncoder(write_hit_vec)

          when(write_hit) {
            cache_data(write_hit_index) := core_io.data_wdata
          }.otherwise {
            insertIntoCache(core_io.data_addr, core_io.data_wdata)
          }
        }.otherwise {
          // mem didnt respond yet, go into waiting state
          waiting := true.B
          pending_we := true.B
        }
      }.otherwise {
        // read request
        when(hit) {
          // cache hit, return next cycle
          core_io.data_gnt := true.B
          core_io.data_rdata := cache_data(hit_index)
        }.otherwise {
          // cache miss, gotta go to mem
          mem_io.data_req := true.B
          mem_io.data_addr := core_io.data_addr
          mem_io.data_be := "hF".U
          mem_io.data_we := false.B

          when(mem_io.data_gnt) {
            core_io.data_gnt := true.B
            core_io.data_rdata := mem_io.data_rdata
            insertIntoCache(core_io.data_addr, mem_io.data_rdata)
          }.otherwise {
            waiting := true.B
            pending_we := false.B
          }
        }
      }
    }
  }

  // reset everything
  when(~io_reset.rst_n) {
    cache_valid := VecInit(Seq.fill(capacity)(false.B))
    cache_addr := VecInit(Seq.fill(capacity)(0.U(32.W)))
    cache_data := VecInit(Seq.fill(capacity)(0.U(32.W)))
    head := 0.U
    tail := 0.U
    count := 0.U
    waiting := false.B
    pending_we := false.B
  }
}
