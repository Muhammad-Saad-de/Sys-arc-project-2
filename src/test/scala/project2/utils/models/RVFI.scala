package project2.utils.models

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim

class RVFI {
  var valid: Bool = false.B
  var order: UInt = 0.U
  var insn: UInt = 0.U
  var trap: Bool = false.B
  var halt: Bool = false.B
  var intr: Bool = false.B
  var mode: UInt = 0.U
  var ixl: UInt = 0.U
  var rs1_addr: UInt = 0.U
  var rs2_addr: UInt = 0.U
  var rs1_rdata: UInt = 0.U
  var rs2_rdata: UInt = 0.U
  var rs_seq: Seq[(UInt, UInt)] = Seq()
  var rs_rcount: UInt = 0.U
  var rd_addr: UInt = 0.U
  var rd_wdata: UInt = 0.U
  var rd_seq: Seq[(UInt, UInt)] = Seq()
  var rd_wcount: UInt = 0.U
  var pc_rdata: UInt = 0.U
  var pc_wdata: UInt = 0.U
  var mem_addr: UInt = 0.U
  var mem_rmask: UInt = 0.U
  var mem_wmask: UInt = 0.U
  var mem_rdata: UInt = 0.U
  var mem_wdata: UInt = 0.U
}
