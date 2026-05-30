package project2.utils

import chisel3.Module
import chisel3.simulator.HasSimulator
import svsim.verilator.Backend.CompilationSettings.TraceKind.Vcd
import svsim.verilator.Backend.CompilationSettings.TraceStyle
import chisel3.testing.HasTestingDirectory
import chisel3.simulator.ChiselOptionsModifications
import chisel3.simulator.FirtoolOptionsModifications
import svsim.CommonSettingsModifications
import svsim.BackendSettingsModifications
import chisel3.simulator.Settings
import chisel3.simulator.scalatest.ChiselSim

trait TraceSim {
  this: ChiselSim =>

  implicit val verilator: HasSimulator = HasSimulator.simulators
    .verilator(verilatorSettings =
      svsim.verilator.Backend.CompilationSettings.default
        .withDisableFatalExitOnWarnings(true)
        .withTraceStyle(Some(TraceStyle(Vcd, traceUnderscore = true)))
    )
}
