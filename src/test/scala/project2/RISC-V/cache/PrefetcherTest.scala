package project2.RISCV.cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import project2.utils.TraceSim
import RISCV.implementation.generic.caches.Prefetcher
import os.write
import RISCV.utils.assembler.InstType.B

class PrefetcherTest
    extends AnyFlatSpec
    with ChiselSim
    with TraceSim
    with Matchers {
  behavior of "Prefetcher"

  def resetPrefetcher(dut: Prefetcher): Unit = {
    dut.core_io.data_req.poke(false.B)
    dut.core_io.data_addr.poke(0.U)
    dut.core_io.data_we.poke(false.B)
    dut.core_io.data_be.poke(0.U)
    dut.core_io.data_wdata.poke(0.U)
    dut.mem_io.data_gnt.poke(false.B)
    dut.mem_io.data_rdata.poke(0.U)
    dut.io_reset.boot_addr.poke(0.U)
    dut.io_reset.rst_n.poke(false.B)
    dut.clock.step()
    dut.io_reset.rst_n.poke(true.B)
    dut.clock.step()
  }

  def performAccess(
      dut: Prefetcher,
      addr: BigInt,
      data: BigInt,
      write: Boolean = false,
      be: BigInt = 0xf,
      lastStep: Boolean = true
  ): Unit = {
    dut.mem_io.data_gnt.poke(false.B)
    dut.core_io.data_req.poke(true.B)
    dut.core_io.data_addr.poke(addr.U)
    dut.core_io.data_we.poke(write.B)
    dut.core_io.data_be.poke(be.U)
    if (write) {
      dut.core_io.data_wdata.poke(data.U)
    } else {
      dut.core_io.data_wdata.poke(0.U)
    }

    dut.mem_io.data_req.expect(true.B)
    dut.mem_io.data_addr.expect(addr.U)
    dut.mem_io.data_we.expect(write.B)
    dut.mem_io.data_be.expect(be.U)
    if (write) {
      dut.mem_io.data_wdata.expect(data.U)
    } else {
      dut.mem_io.data_wdata.expect(0.U)
    }

    dut.clock.step()

    dut.mem_io.data_gnt.poke(true.B)
    dut.mem_io.data_rdata.poke(data.U)

    dut.core_io.data_gnt.expect(true.B)
    dut.core_io.data_rdata.expect(data.U)

    dut.core_io.data_req.poke(false.B)
    if (lastStep) {
      dut.clock.step()
      dut.mem_io.data_gnt.poke(false.B)
    }
  }

  it should "forward a read request to memory" in {
    simulate(new Prefetcher) { dut =>
      enableWaves()
      resetPrefetcher(dut)
      performAccess(dut, 0x1000, BigInt("DEADBEEF", 16))
    }
  }

  it should "prefetch the next block" in {
    simulate(new Prefetcher) { dut =>
      enableWaves()
      resetPrefetcher(dut)
      performAccess(dut, 0x1000, BigInt("DEADBEEF", 16))
      performAccess(dut, 0x2000, BigInt("CAFEBABE", 16))
      performAccess(dut, 0x3000, BigInt("FEEDFACE", 16))

      // The prefetcher should now prefetch 0x4000
      dut.mem_io.data_req.expect(true.B)
      dut.mem_io.data_addr.expect(0x4000.U)
      dut.mem_io.data_we.expect(false.B)
      dut.mem_io.data_be.expect(0xf.U)
    }
  }

  it should "prefetch ignore writes" in {
    simulate(new Prefetcher) { dut =>
      enableWaves()
      resetPrefetcher(dut)
      performAccess(dut, 0x1000, BigInt("DEADBEEF", 16), write = true)
      performAccess(dut, 0x2000, BigInt("CAFEBABE", 16), write = true)
      performAccess(dut, 0x3000, BigInt("FEEDFACE", 16), write = true)

      // The prefetcher should not prefetch writes
      for (i <- 0 until 10) {
        dut.mem_io.data_req.expect(false.B)
        dut.clock.step()
      }
    }
  }

  it should "delay the prefetch to after the write" in {
    simulate(new Prefetcher) { dut =>
      enableWaves()
      resetPrefetcher(dut)
      performAccess(dut, 0x1000, BigInt("DEADBEEF", 16))
      performAccess(dut, 0x2000, BigInt("CAFEBABE", 16))
      performAccess(dut, 0x3000, BigInt("FEEDFACE", 16), lastStep = false)

      performAccess(dut, 0xabcd, BigInt("DEADC0DE", 16), write = true)

      dut.clock.step()

      // The prefetcher should now prefetch 0x4000
      dut.mem_io.data_req.expect(true.B)
      dut.mem_io.data_addr.expect(0x4000.U)
      dut.mem_io.data_we.expect(false.B)
      dut.mem_io.data_be.expect(0xf.U)
    }
  }

  it should "delay the write to after the prefetch" in {
    simulate(new Prefetcher) { dut =>
      enableWaves()
      resetPrefetcher(dut)
      performAccess(dut, 0x1000, BigInt("DEADBEEF", 16))
      performAccess(dut, 0x2000, BigInt("CAFEBABE", 16))
      performAccess(dut, 0x3000, BigInt("FEEDFACE", 16))

      dut.clock.step()

      // The prefetcher should now prefetch 0x4000
      dut.mem_io.data_req.expect(true.B)
      dut.mem_io.data_addr.expect(0x4000.U)
      dut.mem_io.data_we.expect(false.B)
      dut.mem_io.data_be.expect(0xf.U)

      // Now perform the write to 0xabcd, should be delayed
      dut.core_io.data_req.poke(true.B)
      dut.core_io.data_addr.poke(0xabcd.U)
      dut.core_io.data_we.poke(true.B)
      dut.core_io.data_be.poke(0xf.U)
      dut.core_io.data_wdata.poke(BigInt("DEADC0DE", 16).U)

      dut.clock.step()

      // the prefetch is still pending
      dut.mem_io.data_req.expect(true.B)
      dut.mem_io.data_addr.expect(0x4000.U)
      dut.mem_io.data_we.expect(false.B)
      dut.mem_io.data_be.expect(0xf.U)

      dut.core_io.data_gnt.expect(false.B)

      dut.clock.step()

// now the prefetch should complete
      dut.mem_io.data_gnt.poke(true.B)
      dut.mem_io.data_rdata.poke(BigInt("FEEDFACE", 16).U)

      dut.core_io.data_gnt.expect(false.B)

      dut.clock.step()

      dut.mem_io.data_gnt.poke(false.B)
      dut.core_io.data_gnt.expect(false.B)

      dut.clock.step()
// now we expect the write to go through
      dut.mem_io.data_req.expect(true.B)
      dut.mem_io.data_addr.expect(0xabcd.U)
      dut.mem_io.data_we.expect(true.B)
      dut.mem_io.data_be.expect(0xf.U)
      dut.mem_io.data_wdata.expect(BigInt("DEADC0DE", 16).U)
      dut.core_io.data_gnt.expect(false.B)

// complete the write
      dut.clock.step()
      dut.mem_io.data_gnt.poke(true.B)
      dut.core_io.data_gnt.expect(true.B)
    }
  }

  it should "not prefetch on non-sequential accesses" in {
    simulate(new Prefetcher) { dut =>
      enableWaves()
      resetPrefetcher(dut)
      performAccess(dut, 0x1000, BigInt("DEADBEEF", 16))
      performAccess(dut, 0x3000, BigInt("FEEDFACE", 16))
      performAccess(dut, 0x2000, BigInt("FEEDFACE", 16))

      dut.clock.step()

      // The prefetcher should not prefetch on non-sequential accesses
      for (i <- 0 until 10) {
        dut.mem_io.data_req.expect(false.B)
        dut.clock.step()
      }
    }
  }
}
