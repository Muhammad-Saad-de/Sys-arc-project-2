package RISCV.utils.assembler

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

import RISCV.utils.assembler.ObjectUtils._

protected object PreProcess {

  /** Preprocess the assembly code to remove comments and empty lines
    *
    * @param input
    *   the assembly code string
    * @return
    *   the preprocessed assembly code string
    */
  def apply(input: String): String = {
    var output = input.split("\n").toList
    try {
      // Remove everything after // or # (comments)
      output = output.map(line => line.replaceAll("""(//|#).*""", "").trim)
    } catch {
      case e: RISCVAssemblerException => { throw e }
      case e: Exception => {
        throw new RISCVAssemblerException("Error in preprocessing comments.")
      }
    }
    try {
      // duplicate la and li for further processing
      output = output.map(s =>
        if ("(?i)(^|\\W)(l[ai])\\s".r.findFirstIn(s).isDefined)
          s.replaceAll("(?i)(^|\\W)(l[ai])\\s", "$1$21 ") + "\n" + s
            .replaceAll("(?i)(^|\\W)(l[ai])\\s", "$1$22 ")
        else s
      )
    } catch {
      case e: RISCVAssemblerException => { throw e }
      case e: Exception => {
        throw new RISCVAssemblerException(
          "Error in preprocessing la and li instructions."
        )
      }
    }
    // filter empty lines and leading and trailing whitespaces
    output = output.filter(_.nonEmpty).filter(!_.trim().isEmpty()).map(_.trim)

    output.mkString("\n")
  }
}

protected object LineParser {

  /** Parses input string lines to generate the list of instructions, addresses
    * and label addresses
    *
    * @param input
    *   input multiline assembly string
    * @return
    *   a tuple containing:
    *   - `ArrayBuffer[String]` with the assembly instruction
    *   - `ArrayBuffer[String]` with the assembly instruction address
    *   - `Map[String, String]` with the assembly label addresses
    */
  def apply(
      input: String
  ): (ArrayBuffer[String], ArrayBuffer[String], Map[String, String]) = {
    val instList = input
      .split("\n")
      .toList
      .filter(_.nonEmpty)
      .filter(!_.trim().isEmpty())
      .map(_.trim)
    val ignores = Seq(".", "/")

    // Filter lines which begin with characters from `ignores`
    val instListFilter =
      instList.filterNot(l => ignores.contains(l.trim().take(1))).toIndexedSeq

    // Remove inline comments
    val instListNocomment =
      instListFilter.map(_.split("/")(0).trim).toIndexedSeq

    var idx = 0
    val instructions = scala.collection.mutable.ArrayBuffer.empty[String]
    val instructionsAddr = scala.collection.mutable.ArrayBuffer.empty[String]
    val labelIndex = scala.collection.mutable.Map[String, String]()

    instListNocomment.foreach { data =>
      // That's an ugly parser, but works for now :)
      // println(s"-- Processing line: $data, address: ${(idx * 4L).toHexString}")
      val hasLabel = data.indexOf(":")
      if (hasLabel != -1) {
        if (""".+:\s*(\/.*)?$""".r.findFirstIn(data).isDefined) {
          // Has label without code, this label points to next address
          labelIndex(data.split(":")(0).replace(":", "")) =
            ((idx + 0) * 4L).toHexString
          idx += 0
        } else {
          // Has label and code in the same line, this label points to this address
          labelIndex(data.split(':')(0).replace(":", "").trim) =
            (idx * 4L).toHexString
          instructions.append(data.split(':')(1).trim)
          instructionsAddr.append((idx * 4L).toHexString)
          idx += 1
        }
      } else {
        instructions.append(data.trim)
        instructionsAddr.append((idx * 4L).toHexString)
        idx += 1
      }
    }
    (instructions, instructionsAddr, labelIndex.toMap)
  }
}

protected object InstructionParser {

  def parseImmediate(imm: String, input: String): Long = {
    try {
      if (imm.startsWith("0x")) imm.substring(2).h
      else imm.toLong
    } catch {
      case e: Exception =>
        throw new RISCVAssemblerException(
          s"Invalid immediate value ${imm}. (in line: $input)"
        )
    }
  }

  def parseImmediateOrLabel(
      imm: String,
      input: String,
      addr: String,
      labelIndex: Map[String, String]
  ): Long = {
    try {
      imm match {
        case i if i.startsWith("0x")      => i.substring(2).h
        case i if Try(i.toLong).isFailure => labelIndex(i).h - addr.h
        case i                            => i.toLong
      }
    } catch {
      case e: Exception =>
        throw new RISCVAssemblerException(
          s"Invalid immediate value ${imm}. (in line: $input)"
        )
    }
  }

  /** Parse an assembly instruction and return the opcode and opdata
    *
    * @param input
    *   the assembly instruction string
    * @param addr
    *   the assembly instruction address
    * @param labelIndex
    *   the index containing the label addresses
    * @return
    *   a tuple containing the Instruction and opdata
    */
  def apply(
      input: String,
      addr: String = "0",
      labelIndex: Map[String, String] = Map[String, String]()
  ): Option[
    (Instruction, Map[String, Long])
  ] = {
    // println(input) //helpful for debugging; should eventually display parsing errors in front end
    // The regex splits the input into groups (dependind on type):
    // (0) - Instruction name
    // (1) - Instruction rd
    // (2) - Instruction rs1/imm
    // (3) - Instruction rs2/rs
    val parsed = input.trim.split("[\\s,\\(\\)]+").filter(_.nonEmpty)
    var instructionParts = parsed
    try {
      // Check if it's a pseudo-instruction
      instructionParts = PseudoInstructions(parsed) match {
        case Some(pi) => pi
        case None     => parsed
      }
    } catch {
      case e: RISCVAssemblerException =>
        throw new RISCVAssemblerException(e.message + s" (in line: $input)")
    }

    val inst = Instructions(instructionParts(0)) match {
      case Some(i) => i
      case _ =>
        throw new RISCVAssemblerException(
          s"Unable to match instruction. (in line: $input)"
        )
    }

    inst.instType match {
      case InstType.R =>
        if (instructionParts.length != 4)
          throw new RISCVAssemblerException(
            s"R-TYPE instructions should have three arguments. (in line: $input)"
          )
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(2)),
              "rs2" -> RegMap(instructionParts(3))
            )
          )
        )
      case InstType.R1 => {
        if (instructionParts.length != 3)
          throw new RISCVAssemblerException(
            s"R1-TYPE instructions should have two arguments. (in line: $input)"
          )
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(2))
            )
          )
        )
      }
      case InstType.R3 => {
        if (instructionParts.length != 5)
          throw new RISCVAssemblerException(
            s"R3-TYPE instructions should have four arguments. (in line: $input)"
          )
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(2)),
              "rs2" -> RegMap(instructionParts(3)),
              "rs3" -> RegMap(instructionParts(4))
            )
          )
        )
      }
      case InstType.I => {
        if (
          (inst.name == "LA2" | inst.name == "LI2") & instructionParts.length == 3
        ) {
          val imm =
            inst.name match {
              case "LA2" =>
                try {
                  labelIndex(
                    instructionParts(2)
                  ).h & 0xfff // LA2 instruction uses the lower 12 bits of the label address
                } catch {
                  case e: Exception =>
                    throw new RISCVAssemblerException(
                      s"Label ${instructionParts(2)} not found. (in line: $input)"
                    )
                }
              case "LI2" =>
                parseImmediate(
                  instructionParts(2),
                  input
                ) & 0xfff // LI2 instruction uses the lower 12 bits of the immediate value
            }
          return Some(
            (
              Instructions("ADDI").get,
              Map(
                "rd" -> RegMap(instructionParts(1)),
                "rs1" -> RegMap(instructionParts(1)),
                "imm" -> imm
              )
            )
          )
        }
        // First check if instruction has appropriate arguments
        if (
          instructionParts.length != 4 && !Seq(
            "ECALL",
            "EBREAK",
            "FENCE.I",
            "FENCE",
            "MRET",
            "SRET",
            "MNRET",
            "WFI"
          ).contains(
            instructionParts(0).toUpperCase
          )
        )
          throw new RISCVAssemblerException(
            s"I-TYPE instructions should have three arguments. (in line: $input)"
          )
        // Treat instructions that contains offsets (Loads)
        if (inst.hasOffset) {
          val imm =
            try {
              if (instructionParts(2).startsWith("0x"))
                instructionParts(2).substring(2).h
              else instructionParts(2).toLong
            } catch {
              case e: Exception =>
                throw new RISCVAssemblerException(
                  s"Invalid immediate value ${instructionParts(2)}. (in line: $input)"
                )
            }
          Some(
            (
              inst,
              Map(
                "rd" -> RegMap(instructionParts(1)),
                "rs1" -> RegMap(instructionParts(3)),
                "imm" -> imm
              )
            )
          )
        } else {
          // Treat instructions with no arguments
          if (
            Seq("ECALL", "EBREAK", "FENCE.I", "MRET", "SRET", "MNRET", "WFI")
              .contains(instructionParts(0).toUpperCase)
          ) {
            val imm = inst.fixed.b
            Some(
              (
                inst,
                Map(
                  "rd" -> 0,
                  "rs1" -> 0,
                  "imm" -> imm
                )
              )
            )
          } else if (Seq("FENCE").contains(instructionParts(0).toUpperCase)) {
            // Treat FENCE instruction
            val imm = if (instructionParts.length == 3) {
              val pred = instructionParts(1)
                .map(_ match {
                  case bit if bit.toLower == 'i' => 8
                  case bit if bit.toLower == 'o' => 4
                  case bit if bit.toLower == 'r' => 2
                  case bit if bit.toLower == 'w' => 1
                  case _                         => 0
                })
                .sum
                .toBinaryString
              val succ = instructionParts(2)
                .map(_ match {
                  case bit if bit.toLower == 'i' => 8
                  case bit if bit.toLower == 'o' => 4
                  case bit if bit.toLower == 'r' => 2
                  case bit if bit.toLower == 'w' => 1
                  case _                         => 0
                })
                .sum
                .toBinaryString
              ("0000" + pred + succ).b
            } else {
              "000011111111".b
            }
            Some(
              (
                inst,
                Map(
                  "rd" -> 0,
                  "rs1" -> 0,
                  "imm" -> imm
                )
              )
            )
          } else {
            // Treat other I instructions (Shifts)
            val shamt = parseImmediate(instructionParts(3), input)
            val imm = if (inst.fixed != "") {
              // TODO: Support differnt max shamt for different instructions in B extension
              if (shamt >= 64)
                throw new RISCVAssemblerException(
                  s"Shift amount may not be larger than 64. (in line: $input)"
                ) // Shamt has 5 bits
              // If instruction contains fixed imm (like SRAI, SRLI, SLLI), use the fixed imm padded right to fill 12 bits
              (inst.fixed + shamt.toBinaryString.padZero(5).takeRight(5)).b
            } else {
              parseImmediate(instructionParts(3), input)
            }
            Some(
              (
                inst,
                Map(
                  "rd" -> RegMap(instructionParts(1)),
                  "rs1" -> RegMap(instructionParts(2)),
                  "imm" -> imm
                )
              )
            )
          }
        }
      }
      case InstType.I3 => {
        if (instructionParts.length != 5)
          throw new RISCVAssemblerException(
            s"I3-TYPE instructions should have four arguments. (in line: $input)"
          )
        val imm = parseImmediate(instructionParts(4), input)
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(2)),
              "rs3" -> RegMap(instructionParts(3)),
              "imm" -> imm
            )
          )
        )
      }
      case InstType.S => {
        if (instructionParts.length != 4)
          throw new RISCVAssemblerException(
            s"S-TYPE instructions should have three arguments. (in line: $input)"
          )
        val imm = parseImmediate(instructionParts(2), input)
        Some(
          (
            inst,
            Map(
              "rs2" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(3)),
              "imm" -> imm
            )
          )
        )
      }
      case InstType.B => {
        if (instructionParts.length != 4)
          throw new RISCVAssemblerException(
            s"B-TYPE instructions should have three arguments. (in line: $input)"
          )
        val imm =
          parseImmediateOrLabel(instructionParts(3), input, addr, labelIndex)
        Some(
          (
            inst,
            Map(
              "rs1" -> RegMap(instructionParts(1)),
              "rs2" -> RegMap(instructionParts(2)),
              "imm" -> imm
            )
          )
        )
      }
      case InstType.U | InstType.J => {
        if (instructionParts.length != 3)
          throw new RISCVAssemblerException(
            s"U/J-TYPE instructions should have two arguments. (in line: $input)"
          )
        if (inst.name == "LA1" | inst.name == "LI1") {
          val imm =
            inst.name match {
              case "LA1" =>
                (labelIndex(
                  instructionParts(2)
                ).h >> 12) + (if (
                                (labelIndex(instructionParts(2)).h & 0x800) != 0
                              ) 1
                              else
                                0) // LA1 instruction uses the upper 20 bits of the label address
              case "LI1" =>
                (parseImmediate(
                  instructionParts(2),
                  input
                ) >> 12) + (if (
                              (parseImmediate(
                                instructionParts(2),
                                input
                              ) & 0x800) != 0
                            ) 1
                            else
                              0) // LI1 instruction uses the upper 20 bits of the immediate value
            }
          return Some(
            (
              Instructions("LUI").get,
              Map("rd" -> RegMap(instructionParts(1)), "imm" -> imm)
            )
          )
        }
        val imm =
          parseImmediateOrLabel(instructionParts(2), input, addr, labelIndex)
        Some((inst, Map("rd" -> RegMap(instructionParts(1)), "imm" -> imm)))
      }
      case InstType.C => {
        if (instructionParts.length != 4)
          throw new RISCVAssemblerException(
            s"CSR instructions should have three arguments. (in line: $input)"
          )
        val csr = CsrMap(instructionParts(2))
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(3)),
              "csr" -> csr
            )
          )
        )
      }
      case InstType.CI => {
        if (instructionParts.length != 4)
          throw new RISCVAssemblerException(
            s"CSRI instructions should have three arguments. (in line: $input)"
          )
        val csr = CsrMap(instructionParts(2))
        val imm = parseImmediate(instructionParts(3), input)
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "csr" -> csr,
              "imm" -> imm
            )
          )
        )
      }
      case InstType.PLI => {
        if (instructionParts.length != 3)
          throw new RISCVAssemblerException(
            s"Packed load immediate instructions should have two arguments. (in line: $input)"
          )
        val imm = parseImmediate(instructionParts(2), input)
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "imm" -> imm
            )
          )
        )
      }
      case InstType.PI => {
        if (instructionParts.length != 4)
          throw new RISCVAssemblerException(
            s"Packed immediate instructions should have three arguments. (in line: $input)"
          )
        val imm = parseImmediate(instructionParts(3), input)
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(2)),
              "imm" -> imm
            )
          )
        )
      }
      case InstType.PR => {
        if (instructionParts.length != 4)
          throw new RISCVAssemblerException(
            s"Packed register instructions should have three arguments. (in line: $input)"
          )
        Some(
          (
            inst,
            Map(
              "rd" -> RegMap(instructionParts(1)),
              "rs1" -> RegMap(instructionParts(2)),
              "rs2" -> RegMap(instructionParts(3))
            )
          )
        )
      }
      case _ =>
        throw new RISCVAssemblerException(
          s"Unknown instruction type. (in line: $input)"
        )
    }
  }
}

protected object FillInstruction {

  /** Fills the instruction arguments based on instruction type
    *
    * @param op
    *   the instruction opcode and type
    * @param data
    *   the received instruction arguments
    * @param input
    *   the original instruction string (for error messages)
    * @return
    *   the filled instruction binary
    */
  def apply(op: Instruction, data: Map[String, Long], input: String): String =
    op.instType match {
      case InstType.R => {
        val rd = data("rd").toBinaryString.padZero(5)
        val rs1 = data("rs1").toBinaryString.padZero(5)
        val rs2 = data("rs2").toBinaryString.padZero(5)
        op.funct7 + rs2 + rs1 + op.funct3 + rd + op.opcode
      }

      case InstType.R1 => {
        val rd = data("rd").toBinaryString.padZero(5)
        val rs1 = data("rs1").toBinaryString.padZero(5)
        op.funct7 + op.rs2 + rs1 + op.funct3 + rd + op.opcode
      }

      case InstType.R3 => {
        val rd = data("rd").toBinaryString.padZero(5)
        val rs1 = data("rs1").toBinaryString.padZero(5)
        val rs2 = data("rs2").toBinaryString.padZero(5)
        val rs3 = data("rs3").toBinaryString.padZero(5)
        rs3 + op.funct2 + rs2 + rs1 + op.funct3 + rd + op.opcode
      }

      case InstType.I => {
        val rd = data("rd").toBinaryString.padZero(5)
        val rs1 = data("rs1").toBinaryString.padZero(5)
        val imm = data("imm").to32Bit.toBinaryString.padZero(12)
        imm + rs1 + op.funct3 + rd + op.opcode
      }

      case InstType.I3 => {
        val rd = data("rd").toBinaryString.padZero(5)
        val rs1 = data("rs1").toBinaryString.padZero(5)
        val rs3 = data("rs3").toBinaryString.padZero(5)
        val imm = data("imm").to32Bit.toBinaryString.padZero(6)
        rs3 + op.funct1 + imm + rs1 + op.funct3 + rd + op.opcode
      }

      case InstType.S => {
        val rs1 = data("rs1").toBinaryString.padZero(5)
        val rs2 = data("rs2").toBinaryString.padZero(5)
        val imm = data("imm").to32Bit.toBinaryString
          .padZero(12)
          .reverse // reverse to have binary in little endian
        imm.slice(5, 12).reverse + rs2 + rs1 + op.funct3 + imm
          .slice(0, 5)
          .reverse + op.opcode
      }

      case InstType.B => {
        val rs1 = data("rs1").toBinaryString.padZero(5)
        val rs2 = data("rs2").toBinaryString.padZero(5)
        val imm = data("imm").to32Bit.toBinaryString
          .padZero(13)
          .reverse // reverse to have binary in little endian
        imm.slice(12, 13).reverse + imm
          .slice(5, 11)
          .reverse + rs2 + rs1 + op.funct3 +
          imm.slice(1, 5).reverse + imm.slice(11, 12).reverse + op.opcode
      }

      case InstType.U => {
        val rd = data("rd").toBinaryString.padZero(5)
        val imm = data("imm").to32Bit.toBinaryString.padZero(20)
        imm + rd + op.opcode
      }

      case InstType.J => {
        val rd = data("rd").toBinaryString.padZero(5)
        val imm = data("imm").to32Bit.toBinaryString
          .padZero(21)
          .reverse // reverse to have binary in little endian
        imm.slice(20, 21).reverse + imm.slice(1, 11).reverse + imm
          .slice(11, 12)
          .reverse + imm
          .slice(12, 20)
          .reverse + rd + op.opcode
      }

      case InstType.C => {
        val rd = data("rd").toBinaryString.padZero(5)
        val rs1 = data("rs1").toBinaryString.padZero(5)
        val csr = data("csr").toBinaryString.padZero(12)
        csr + rs1 + op.funct3 + rd + op.opcode
      }

      case InstType.CI => {
        val rd = data("rd").toBinaryString.padZero(5)
        val csr = data("csr").toBinaryString.padZero(12)
        val imm = data("imm").to32Bit.toBinaryString.padZero(5)
        csr + imm + op.funct3 + rd + op.opcode
      }

      case InstType.PLI => {
        var rd = data("rd").toBinaryString.padZero(5)
        if (op.rd_lsb != "") {
          val rd_lsb = data("rd").toBinaryString.padZero(5).takeRight(1)
          if (rd_lsb != 0.toBinaryString) {
            throw new RISCVAssemblerException(
              s"Invalid rd for double width instruction. rd should be even. (in line: $input)"
            )
          }
          rd = rd.dropRight(1) + op.rd_lsb
        }
        if (op.name == "PLI.B" | op.name == "PLI.DB") {
          val imm = data("imm").to32Bit.toBinaryString.padZero(8)
          op.funct7 + op.imm_msb + imm + op.imm_lsb + op.funct3 + rd + op.opcode
        } else if (op.name == "PLI.H" | op.name == "PLI.DH") {
          val imm = data("imm").to32Bit.toBinaryString
            .padZero(10)
            .reverse // reverse to have binary in little endian
          op.funct7 + imm.slice(0, 9).reverse + imm
            .slice(9, 10)
            .reverse + op.funct3 + rd + op.opcode
        } else if (op.name == "PLUI.H" | op.name == "PLUI.DH") {
          val imm = data("imm").to32Bit.toBinaryString
            .padZero(10)
            .reverse // reverse to have binary in little endian
          op.funct7 + imm.slice(0, 1).reverse + imm
            .slice(1, 10)
            .reverse + op.funct3 + rd + op.opcode
        } else {
          throw new RISCVAssemblerException(
            s"Unknown packed immediate instruction. (in line: $input)"
          )
        }
      }

      case InstType.PI => {
        var rd = data("rd").toBinaryString.padZero(5)
        if (op.rd_lsb != "") {
          val rd_lsb = data("rd").toBinaryString.padZero(5).takeRight(1)
          if (rd_lsb != 0.toBinaryString) {
            throw new RISCVAssemblerException(
              s"Invalid rd for double width instruction. rd should be even. (in line: $input)"
            )
          }
          rd = rd.dropRight(1) + op.rd_lsb
        }
        var rs1 = data("rs1").toBinaryString.padZero(5)
        if (op.rs1_lsb != "") {
          val rs1_lsb = data("rs1").toBinaryString.padZero(5).takeRight(1)
          if (rs1_lsb != 0.toBinaryString) {
            throw new RISCVAssemblerException(
              s"Invalid rs1 for double width instruction. rs1 should be even. (in line: $input)"
            )
          }
          rs1 = rs1.dropRight(1) + op.rs1_lsb
        }
        var imm = data("imm").to32Bit.toBinaryString.padZero(5)
        if (op.imm_msb != "") {
          imm = data("imm").to32Bit.toBinaryString.padZero(4)
        }
        op.funct7 + op.imm_msb + imm + op.imm_lsb + rs1 + op.funct3 + rd + op.opcode
      }

      case InstType.PR => {
        var rd = data("rd").toBinaryString.padZero(5)
        if (op.rd_lsb != "") {
          val rd_lsb = data("rd").toBinaryString.padZero(5).takeRight(1)
          if (rd_lsb != 0.toBinaryString) {
            throw new RISCVAssemblerException(
              s"Invalid rd for double width instruction. rd should be even. (in line: $input)"
            )
          }
          rd = rd.dropRight(1) + op.rd_lsb
        }
        var rs1 = data("rs1").toBinaryString.padZero(5)
        if (op.rs1_lsb != "") {
          val rs1_lsb = data("rs1").toBinaryString.padZero(5).takeRight(1)
          if (rs1_lsb != 0.toBinaryString) {
            throw new RISCVAssemblerException(
              s"Invalid rs1 for double width instruction. rs1 should be even. (in line: $input)"
            )
          }
          rs1 = rs1.dropRight(1) + op.rs1_lsb
        }
        var rs2 = data("rs2").toBinaryString.padZero(5)
        if (op.rs2_lsb != "") {
          val rs2_lsb = data("rs2").toBinaryString.padZero(5).takeRight(1)
          if (rs2_lsb != 0.toBinaryString) {
            throw new RISCVAssemblerException(
              s"Invalid rs2 for double width instruction. rs2 should be even. (in line: $input)"
            )
          }
          rs2 = rs2.dropRight(1) + op.rs2_lsb
        }
        op.funct7 + rs2 + rs1 + op.funct3 + rd + op.opcode

      }
    }
}

protected object RegMap {

  /** Maps the register name or ABI name to the register number
    *
    * @param regName
    *   the register name
    * @return
    *   the register number
    */
  def apply(input: String): Long =
    try {
      input.toLowerCase match {
        case "x0" | "zero"      => 0
        case "x1" | "ra"        => 1
        case "x2" | "sp"        => 2
        case "x3" | "gp"        => 3
        case "x4" | "tp"        => 4
        case "x5" | "t0"        => 5
        case "x6" | "t1"        => 6
        case "x7" | "t2"        => 7
        case "x8" | "s0" | "fp" => 8
        case "x9" | "s1"        => 9
        case "x10" | "a0"       => 10
        case "x11" | "a1"       => 11
        case "x12" | "a2"       => 12
        case "x13" | "a3"       => 13
        case "x14" | "a4"       => 14
        case "x15" | "a5"       => 15
        case "x16" | "a6"       => 16
        case "x17" | "a7"       => 17
        case "x18" | "s2"       => 18
        case "x19" | "s3"       => 19
        case "x20" | "s4"       => 20
        case "x21" | "s5"       => 21
        case "x22" | "s6"       => 22
        case "x23" | "s7"       => 23
        case "x24" | "s8"       => 24
        case "x25" | "s9"       => 25
        case "x26" | "s10"      => 26
        case "x27" | "s11"      => 27
        case "x28" | "t3"       => 28
        case "x29" | "t4"       => 29
        case "x30" | "t5"       => 30
        case "x31" | "t6"       => 31
      }
    } catch {
      case e: Exception =>
        throw new RISCVAssemblerException(
          s"Error in register mapping. Unknown register $input"
        )
    }
}

protected object CsrMap {

  /** Maps the csr name or ABI name to the csr address
    *
    * @param csrName
    *   the csr name
    * @return
    *   the csr address
    */
  def apply(input: String): Long =
    try {
      input.toLowerCase match {
        case "cycle"    => 0xc00
        case "time"     => 0xc01
        case "instret"  => 0xc02
        case "cycleh"   => 0xc80
        case "timeh"    => 0xc81
        case "instreth" => 0xc82

        case "mvendorid"  => 0xf11
        case "marchid"    => 0xf12
        case "mimpid"     => 0xf13
        case "mhartid"    => 0xf14
        case "mconfigptr" => 0xf15

        case "mstatus"    => 0x300
        case "misa"       => 0x301
        case "medeleg"    => 0x302
        case "mideleg"    => 0x303
        case "mie"        => 0x304
        case "mtvec"      => 0x305
        case "mcounteren" => 0x306
        case "mstatush"   => 0x310
        case "medelegh"   => 0x312

        case "mscratch" => 0x340
        case "mepc"     => 0x341
        case "mcause"   => 0x342
        case "mtval"    => 0x343
        case "mip"      => 0x344
        case "mtinst"   => 0x34a
        case "mtval2"   => 0x34b

        case "mcycle"        => 0xb00
        case "minstret"      => 0xb02
        case "mcycleh"       => 0xb80
        case "minstreth"     => 0xb82
        case "mcountinhibit" => 0x320
      }
    } catch {
      case e: Exception =>
        throw new RISCVAssemblerException(
          s"Error in CSR mapping. Unknown CSR $input"
        )
    }
}
