package RISCV.implementation

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.implementation.generic._
import RISCV.implementation.generic.caches.Prefetcher
import RISCV.implementation.generic.caches.FIFOCache

class Core(
    genExecutionUnits: Seq[() => AbstractExecutionUnit],
    rdPorts: Int = 2,
    rsPorts: Int = 4
) extends GenericCore(
      genExecutionUnits,
      new ProgramCounter,
      new RegisterFile,
      Seq(),
      rdPorts,
      rsPorts
    ) {
  // Nothing to do here.
}
