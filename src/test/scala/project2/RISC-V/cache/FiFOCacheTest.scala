package project2.RISCV.cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import project2.utils.TraceSim
import RISCV.implementation.generic.caches.FIFOCache

class FiFOCacheTest
    extends AnyFlatSpec
    with ChiselSim
    with TraceSim
    with Matchers {
  behavior of "FiFOCache"

  def resetCache(dut: FIFOCache): Unit = {
    dut.io_reset.boot_addr.poke(0.U)
    dut.io_reset.rst_n.poke(false.B)
    dut.mem_io.data_gnt.poke(false.B)
    dut.clock.step()
    dut.io_reset.rst_n.poke(true.B)
    dut.clock.step()
  }

  def expectMiss(dut: FIFOCache, addr: BigInt, data: BigInt): Unit = {
    dut.core_io.data_req.poke(true.B)
    dut.core_io.data_addr.poke(addr.U)
    dut.core_io.data_we.poke(false.B)
    dut.core_io.data_be.poke(0xf.U)

    var missed = false
    while (!dut.core_io.data_gnt.peek().litToBoolean) {
      dut.clock.step()

      if (dut.mem_io.data_req.peek().litToBoolean) {
        missed = true
        dut.mem_io.data_gnt.poke(true.B)
        dut.mem_io.data_rdata.poke(data.U)
      }
    }

    dut.core_io.data_gnt.expect(true.B)
    dut.core_io.data_rdata.expect(data.U)
    dut.core_io.data_req.poke(false.B)

    missed should be(true)

    dut.clock.step()
    dut.mem_io.data_gnt.poke(false.B)
  }

  def expectHit(dut: FIFOCache, addr: BigInt, data: BigInt): Unit = {
    dut.core_io.data_req.poke(true.B)
    dut.core_io.data_addr.poke(addr.U)
    dut.core_io.data_we.poke(false.B)
    dut.core_io.data_be.poke(0xf.U)

    dut.mem_io.data_req.expect(false.B)

    while (!dut.core_io.data_gnt.peek().litToBoolean) {
      dut.clock.step()
    }

    dut.core_io.data_gnt.expect(true.B)
    dut.core_io.data_rdata.expect(data.U)
    dut.core_io.data_req.poke(false.B)
    dut.clock.step()
  }

  it should "miss on the first access" in {
    simulate(new FIFOCache(32)) { dut =>
      enableWaves()
      resetCache(dut)

      expectMiss(dut, 0x1000, BigInt("DEADBEEF", 16))
    }

  }

  it should "hit on the second access" in {
    simulate(new FIFOCache(32)) { dut =>
      enableWaves()
      resetCache(dut)
      expectMiss(dut, 0x1000, BigInt("DEADBEEF", 16))
      expectHit(dut, 0x1000, BigInt("DEADBEEF", 16))
    }
  }

  it should "have size 2" in {
    simulate(new FIFOCache(2)) { dut =>
      enableWaves()

      resetCache(dut)
      expectMiss(dut, 0x1000, BigInt("DEADBEEF", 16))
      expectMiss(dut, 0x2000, BigInt("CAFEBABE", 16))
      expectHit(dut, 0x1000, BigInt("DEADBEEF", 16))
      expectHit(dut, 0x2000, BigInt("CAFEBABE", 16))

      expectMiss(dut, 0x3000, BigInt("FEEDFACE", 16))
      expectHit(dut, 0x3000, BigInt("FEEDFACE", 16))

      expectHit(dut, 0x2000, BigInt("CAFEBABE", 16))

      expectMiss(dut, 0x1000, BigInt("DEADBEEF", 16))
    }
  }
}
