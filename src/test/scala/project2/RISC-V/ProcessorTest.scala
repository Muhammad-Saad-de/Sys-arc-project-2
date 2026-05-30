package project2.RISCV

import chisel3._
import chisel3.util._

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import collection.mutable
import scala.io.Source
import java.io.File

import RISCV.interfaces._
import RISCV.implementation._
import RISCV.implementation.RV32I._
import RISCV.utils.assembler.RISCVAssembler
import project2.utils.models._
import project2.utils.ProcessorTestUtils._
import scala.util.Try
import project2.utils.TraceSim

// sbt "testOnly project2.RISCV.ProcessorTest"

class ProcessorTest
    extends AnyFlatSpec
    with ChiselSim
    with TraceSim
    with Matchers {

  val onlyRun =
    "" // Set to "" to run all tests, or set to a specific test name to run only that test
  //            e.g. "public/permutation/swap"

  def runProgram(folderName: String): Unit = {
    val config: TestConfig = TestConfig.fromJson(
      Source.fromResource(s"project2/$folderName/config.json").mkString
    ) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(error)
    }
    simulate(
      new Core(
        config.modules
      )
    ) { dut =>
      enableWaves()
      var state = new ProcessorState()

      resetCore(dut)
      state = prepareState(
        dut,
        config.initial_reg.map(x => x._1.toInt -> x._2.toLong).toMap
      )
      state.data_mem = config.initial_mem
      state.instr_mem = RISCVAssembler
        .fromString(
          Source.fromResource(s"project2/$folderName/program.s").mkString
        )
        .split("\n")
        .map(line => BigInt(line, 16).U)
        .zipWithIndex
        .map(x => BigInt(x._2 * 4 + state.pc) -> x._1.litValue)
        .to(mutable.Map)
      val instrCount = if (config.executed_instructions == 0) {
        state.instr_mem.size
      } else {
        config.executed_instructions.toInt
      }
      for (i <- 0 until instrCount by 1) {
        val instr = Try(state.instr_mem(state.pc))
          .getOrElse(
            throw new Exception(s"Instruction not found at PC: ${state.pc}")
          )
        state = executeInstruction(dut, instr.U, state, config.mem_delay)
      }
      config.final_reg.foreach(x => assert(state.registers(x._1) == x._2))
      config.final_mem.foreach(x => assert(state.data_mem(x._1) == x._2))
    }
  }

  if (onlyRun != "") {
    it should "only run one test" in {
      runProgram(onlyRun)
    }
  } else {
    val test_types =
      new File("src/test/resources/project2/").listFiles().map(_.getName)

    for (test_type <- test_types) {
      val test_categories = new File(s"src/test/resources/project2/$test_type")
        .listFiles()
        .map(_.getName)
      for (test_category <- test_categories) {
        val test_names = new File(
          s"src/test/resources/project2/$test_type/$test_category"
        ).listFiles().map(_.getName)
        for (test_name <- test_names) {
          it should s"${test_type}_${test_category}_${test_name}" in {
            runProgram(s"$test_type/$test_category/$test_name")
          }
        }
      }
    }
  }
}
