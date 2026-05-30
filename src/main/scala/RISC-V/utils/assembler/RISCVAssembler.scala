package RISCV.utils.assembler

import scala.io.Source

import RISCV.utils.assembler.ObjectUtils._

object RISCVAssembler {

  /** Generate an hex string output fom the assembly source file
    *
    * Usage:
    *
    * {{{
    * val outputHex = RISCVAssembler.fromFile("input.asm")
    * }}}
    *
    * @param fileName
    *   the assembly source file
    * @return
    *   the output hex string
    */
  def fromFile(filename: String): String =
    fromString(Source.fromFile(filename).getLines().mkString("\n"))

  /** Generate an hex string output fom the assembly string
    *
    * Usage:
    *
    * {{{
    * val input =
    *       """
    *       addi x1 , x0,   1000
    *       addi x2 , x1,   2000
    *       addi x3 , x2,  -1000
    *       addi x4 , x3,  -2000
    *       addi x5 , x4,   1000
    *       """.stripMargin
    *     val outputHex = RISCVAssembler.fromString(input)
    * }}}
    *
    * @param input
    *   input assembly string to assemble (multiline string)
    * @return
    *   the assembled hex string
    */
  def fromString(input: String): String = {
    val (instructions, addresses, labels) = LineParser(PreProcess(input))
    (instructions zip addresses)
      .map { case (i: String, a: String) => { binOutput(i, a, labels) } }
      .map(hexOutput(_))
      .mkString("\n") + "\n"
  }

  def mappingFromString(
      input: String,
      base: BigInt,
      external_labels: Map[String, String]
  ): (Map[BigInt, (String, BigInt)], Map[String, String]) = {
    var (instructions, addresses, labels) = LineParser(PreProcess(input))
    addresses = addresses.map(a => (BigInt(a, 16) + base).toString(16))
    labels =
      labels.map(l => l._1 -> (BigInt(l._2, 16) + base).toString(16)).toMap
    labels = labels ++ external_labels
    (
      (instructions zip addresses).map {
        case (i: String, a: String) => {
          BigInt(a, 16) -> (
            i.replaceAll("(?i)(l[ai])\\d", "$1"),
            BigInt(hexOutput(binOutput(i, a, labels)), 16)
          )
        }
      }.toMap,
      labels
    )
  }

  /** Generate the binary output for the input instruction
    * @param input
    *   the input instruction (eg. "add x1, x2, x3")
    * @return
    *   the binary output in string
    */
  def binOutput(
      instruction: String,
      address: String = "0",
      labelIndex: Map[String, String] = Map[String, String](),
      width: Int = 32
  ): String = {
    val cleanInst =
      "\\/\\*.*\\*\\/".r.replaceAllIn(instruction, "").toLowerCase.trim

    InstructionParser(cleanInst, address, labelIndex) match {
      case Some((op, opdata)) =>
        try {
          FillInstruction(op, opdata, cleanInst).takeRight(width)
        } catch {
          case e: Exception =>
            throw new RISCVAssemblerException(
              "Failed to assemble instruction: " + instruction + " " + e
            )
        }
      case _ =>
        throw new RISCVAssemblerException(
          "Failed to assemble instruction: " + instruction
        )
    }

  }

  /** Generate the hex string of the instruction from binary
    *
    * @param input
    *   the binary string of the instruction
    * @return
    *   the hex string of the instruction in string
    */
  def hexOutput(input: String): String = {
    val x = input.b
    f"0x$x%08X".toString.takeRight(8)
  }
}

final case class RISCVAssemblerException(
    val message: String = "RISCVAssembler: Unknown Error",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)
