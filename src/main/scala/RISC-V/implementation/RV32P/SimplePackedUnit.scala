package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class SimplePackedUnit extends AbstractExecutionUnit {

  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  ???
}
