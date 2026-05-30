package project2.utils.models

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._

import scala.collection.mutable.Map
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.implementation.RV32I.RV32I
import RISCV.implementation.RV32I.ControlUnit
import RISCV.implementation.RV32I.BranchUnit
import RISCV.implementation.RV32I.ALU
import project2.utils.LUI_ADDI_Unit

case class TestConfig(
    val executed_instructions: BigInt,
    val initial_reg: Map[BigInt, BigInt],
    val initial_mem: Map[BigInt, BigInt],
    val final_reg: Map[BigInt, BigInt],
    val final_mem: Map[BigInt, BigInt],
    val modules: Seq[() => AbstractExecutionUnit],
    val mem_delay: Int
)

object TestConfig {
  implicit val bigIntDecoder: Decoder[BigInt] = Decoder.decodeString.emap {
    str =>
      try {
        if (str.startsWith("0x") || str.startsWith("0X")) {
          Right(
            BigInt(str.drop(2), 16) & BigInt("FFFFFFFF", 16)
          ) // Parse as hexadecimal
        } else {
          Right(BigInt(str) & BigInt("FFFFFFFF", 16)) // Parse as decimal
        }
      } catch {
        case _: NumberFormatException => Left(s"Invalid number format: $str")
      }
  }
  implicit val bigIntKeyDecoder: KeyDecoder[BigInt] = (key: String) => {
    try {
      if (key.startsWith("0x") || key.startsWith("0X")) {
        Some(BigInt(key.drop(2), 16)) // Parse as hexadecimal
      } else {
        Some(BigInt(key)) // Parse as decimal
      }
    } catch {
      case _: NumberFormatException => None
    }
  }

  // Decoders for the mutable Maps
  implicit val mutableMapDecoder: Decoder[Map[BigInt, BigInt]] =
    Decoder.decodeMap[BigInt, BigInt].map(m => Map(m.toSeq: _*))

  implicit val modulesDecoder: Decoder[Seq[() => AbstractExecutionUnit]] =
    Decoder.decodeArray[String].emap { moduleNames =>
      try {
        var containsRV32I = false
        val units = moduleNames.toIndexedSeq.flatMap {
          case "RV32I" => {
            containsRV32I = true
            Seq(() =>
              new RV32I(
                new ControlUnit,
                new RISCV.implementation.RV32I.Decoder,
                new BranchUnit,
                new ALU
              )
            )
          }
          case other =>
            throw new IllegalArgumentException(s"Unknown module: $other")
        }
        val modules = if (!containsRV32I) {
          units ++ Seq(() => new LUI_ADDI_Unit)
        } else {
          units
        }
        Right(modules)
      } catch {
        case e: IllegalArgumentException => Left(e.getMessage)
      }
    }

  // Decoder for TestConfig
  implicit val configDecoder: Decoder[TestConfig] = deriveDecoder[TestConfig]

  def fromJson(jsonString: String): Either[Error, TestConfig] =
    decode[TestConfig](jsonString)
}
