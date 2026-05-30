package RISCV.model

import chisel3._
import chisel3.util._

import RISCV.utils.Types.ORTYPE
import scala.collection.mutable.HashSet
import _root_.RISCV.utils.Types.ORTYPE
import io.circe.syntax.KeyOps

object RISCV_OP extends ChiselEnum {
  val LOAD = Value("b0000011".U)
  val MISC_MEM = Value("b0001111".U)
  val OP_IMM = Value("b0010011".U)
  val AUIPC = Value("b0010111".U)
  val OP_IMM_32 = Value("b0011011".U)
  val STORE = Value("b0100011".U)
  val OP = Value("b0110011".U)
  val LUI = Value("b0110111".U)
  val OP_32 = Value("b0111011".U)
  val BRANCH = Value("b1100011".U)
  val JALR = Value("b1100111".U)
  val JAL = Value("b1101111".U)
  val SYSTEM = Value("b1110011".U)
  val UNKNOWN = Value("b1111111".U)

  def apply(i: UInt): RISCV_OP.Type = {
    return Mux(RISCV_OP.safe(i)._2, RISCV_OP.safe(i)._1, RISCV_OP.UNKNOWN)
  }
}

object RISCV_FUNCT3 extends ChiselEnum {
  val F000 = Value("b000".U)
  val F001 = Value("b001".U)
  val F010 = Value("b010".U)
  val F011 = Value("b011".U)
  val F100 = Value("b100".U)
  val F101 = Value("b101".U)
  val F110 = Value("b110".U)
  val F111 = Value("b111".U)

  def apply(i: UInt): RISCV_FUNCT3.Type = {
    return Mux(
      RISCV_FUNCT3.safe(i)._2,
      RISCV_FUNCT3.safe(i)._1,
      RISCV_FUNCT3.F111
    )
  }
}

object RISCV_FUNCT7 extends ChiselEnum {
  val ZERO = Value("b0000000".U)
  val MULDIV = Value("b0000001".U)
  val PWADD_B = Value("b000010".U)
  val SHFL_PWADDA_H_PACK = Value("b0000100".U)
  val MINMAX = Value("b0000101".U)
  val PWADDA_B = Value("b000110".U)
  val PADD_DHS = Value("b0001100".U)
  val PADD_DWS = Value("b0001101".U)
  val PADD_DBS = Value("b0001110".U)
  val SHIFT = Value("b0010000".U)
  val SET = Value("b0010100".U)
  val PLI_DH = Value("b0011000".U)
  val PLI_DB = Value("b0011010".U)
  val SUB_SRA = Value("b0100000".U)
  val CLEAR = Value("b0100100".U)
  val ROTATE_PNCLIPI_B = Value("b0110000".U)
  val PNCLIPI_H = Value("b0110001".U)
  val INV = Value("b0110100".U)
  val PLUI_DH_PNCLIPRI_B = Value("b0111000".U)
  val PNCLIPRI_H = Value("b0111001".U)
  val PADD_H = Value("b1000000".U)
  val PADD_W = Value("b1000001".U)
  val PADD_B = Value("b1000010".U)
  val PM2ADDA_H = Value("b1000100".U)
  val PSADD_H_PADD_HX = Value("b1001000".U)
  val PSADD_W = Value("b1001001".U)
  val PSADD_B = Value("b1001010".U)
  val PADD_HS_PAADD_H_PM2ADDA_HX = Value("b1001100".U)
  val PADD_WS_PAADD_W = Value("b1001101".U)
  val PADD_BS_PAADD_B = Value("b1001110".U)
  val PADDU_H = Value("b1010000".U)
  val MVM_PSSHL_HS_PM2ADDAU_H = Value("b1010100".U)
  val MVMN = Value("b1010101".U)
  val MERGE = Value("b1010110".U)
  val PLI_H = Value("b1011000".U)
  val PLI_B = Value("b1011010".U)
  val PSSHLR_HS = Value("b1011100".U)
  val PSUB_H = Value("b1100000".U)
  val PSUB_W = Value("b1100001".U)
  val PSUB_B_PM2SADD_H = Value("b1100010".U)
  val PM2SADD_HX = Value("b1101010".U)
  val PADDSU_H = Value("b1110000".U)
  val PSSHA_HS_PM2ADDASU_H = Value("b1110100".U)
  val PLUI_H = Value("b1111000".U)
  val PSSHAR_HS = Value("b1111100".U)
  val UNKNOWN = Value("b1111111".U)

  def apply(i: UInt): RISCV_FUNCT7.Type = {
    return Mux(
      RISCV_FUNCT7.safe(i)._2,
      RISCV_FUNCT7.safe(i)._1,
      RISCV_FUNCT7.UNKNOWN
    )
  }
}

object RISCV_FUNCT12 extends ChiselEnum {
  val ECALL = Value("b000000000000".U)
  val EBREAK = Value("b000000000001".U)
  val PAUSE = Value("b000000010000".U)
  val SRET = Value("b000100000010".U)
  val WFI = Value("b000100000101".U)
  val MRET = Value("b001100000010".U)
  val CLZ = Value("b011000000000".U)
  val CTZ = Value("b011000000001".U)
  val CPOP = Value("b011000000010".U)
  val BMATFLIP = Value("b011000000011".U)
  val SEXTB = Value("b011000000100".U)
  val SEXTH = Value("b011000000101".U)
  val CRC32B = Value("b011000010000".U)
  val CRC32H = Value("b011000010001".U)
  val CRC32W = Value("b011000010010".U)
  val CRC32D = Value("b011000010011".U)
  val CRC32CB = Value("b011000011000".U)
  val CRC32CH = Value("b011000011001".U)
  val CRC32CW = Value("b011000011010".U)
  val CRC32CD = Value("b011000011011".U)
  val FENCE_ISO = Value("b100000110011".U)
  val UNKNOWN = Value("b111111111111".U)

  def apply(i: UInt): RISCV_FUNCT12.Type = {
    return Mux(
      RISCV_FUNCT12.safe(i)._2,
      RISCV_FUNCT12.safe(i)._1,
      RISCV_FUNCT12.UNKNOWN
    )
  }
}

object RISCV_FORMAT extends ChiselEnum {
  val R_TYPE = Value(
    "b00000".U
  ) // | funct7 (7) | rs2 (5) | rs1 (5) | funct3 (3) | rd (5) | op (7) |
  val I_TYPE = Value(
    "b00001".U
  ) // | imm[11:0] (12) | rs1 (5) | funct3 (3) | rd (5) | op (7) |
  val S_TYPE = Value(
    "b00010".U
  ) // | imm[11:5] (7) | rs2 (5) | rs1 (5) | funct3 (3) | imm[4:0] (5) | op (7) |
  val B_TYPE = Value(
    "b00011".U
  ) // | imm [12] (1) |imm[10:5] (6) | rs2 (5) | rs1 (5) | funct3 (3) | imm[4:0] (5) | op (7) |
  val U_TYPE = Value("b00100".U) // | imm[31:12] (20) | rd (5) | op (7) |
  val J_TYPE = Value(
    "b00101".U
  ) // | imm[20] (1) | imm[10:1] (10) | imm[11] (1) | imm[19:12] (8) | rd (5) | op (7) |
  val F12_TYPE = Value(
    "b00110".U
  ) // | funct12 (12) | rs1 (5) | funct3 (3) | rd (5) | op (7) |
  val R_TYPE_NNP = Value(
    "b00111".U
  ) // | funct7 (7) | rs2 (5) | rs1 (5) | funct3 (3) | rd_p (4) | p (1) | op (7) |
  val R_TYPE_NPN = Value(
    "b01000".U
  ) // | funct7 (7) | rs2 (5) | rs1_p (4) | p (1) | funct3 (3) | rd (5) | op (7) |
  val R_TYPE_PNN = Value(
    "b01001".U
  ) // | funct7 (7) | rs2_p (4) | p (1) | rs1 (5) | funct3 (3) | rd (5) | op (7) |
  val R_TYPE_NPP = Value(
    "b01010".U
  ) // | funct7 (7) | rs2 (5) | rs1_p (4} | p (1} | funct3 (3} | rd_p (4} | p (1} | op (7} |
  val R_TYPE_PNP = Value(
    "b01011".U
  ) // | funct7 (7) | rs2_p (4) | p (1) | rs1 (5) | funct3 (3) | rd_p (4) | p (1) | op (7) |
  val R_TYPE_PPN = Value(
    "b01100".U
  ) // | funct7 (7) | rs2_p (4) | p (1) | rs1_p (4) | funct3 (3) | rd (5) | op (7) |
  val R_TYPE_PPP = Value(
    "b01101".U
  ) // | funct7 (7) | rs2_p (4) | p (1) | rs1_p (4) | funct3 (3) | rd_p (4) | p (1) | op (7) |
  val I_TYPE_F7 = Value(
    "b01110".U
  ) // | funct7 (7) | imm[4:0] (5) | rs1 (5) | funct3 (3) | rd (5) | op (7) |
  val I_TYPE_BYTE = Value(
    "b01111".U
  ) // | funct8 (8) | imm[7:0] (8) | p (1) | funct3 (3) | rd (5) | op (7) |
  val I_TYPE_F7_P = Value(
    "b10000".U
  ) // | funct7 (7) | imm[4:0] (5) | rs1 (5) | funct3 (3) | rd (4) | p (1) | op (7) |
  val I_TYPE_BYTE_P = Value(
    "b10001".U
  ) // | funct8 (8) | imm[7:0] (8) | p (1) | funct3 (3) | rd (4) | p (1) | op (7) |
  val I_TYPE_HBYTE_P = Value(
    "b10010".U
  ) // | funct8 (8) | imm[3:0] (4) | rs1_p (4) | p (1) | funct3 (3) | rd (5) | op (7) |
  val I_TYPE_5BIT_P = Value(
    "b10011".U
  ) // | funct7 (7) | imm[4:0] (5) | rs1_p (4) | p (1) | funct3 (3) | rd (5) | op (7) |
  val F_TYPE = Value("b10100".U) // | fixed (32} |
  val UNKNOWN = Value("b11111".U)

  def apply(i: UInt): RISCV_FORMAT.Type = {
    return Mux(
      RISCV_FORMAT.safe(i)._2,
      RISCV_FORMAT.safe(i)._1,
      RISCV_FORMAT.UNKNOWN
    )
  }

  def toBitMask(t: RISCV_FORMAT.Type): UInt = {
    t match {
      case R_TYPE         => "b1111111_00000_00000_111_00000_1111111".U(32.W)
      case I_TYPE         => "b000000000000_00000_111_00000_1111111".U(32.W)
      case S_TYPE         => "b0000000_00000_00000_111_00000_1111111".U(32.W)
      case B_TYPE         => "b0000000_00000_00000_111_00000_1111111".U(32.W)
      case U_TYPE         => "b000000000000_00000_000_00000_1111111".U(32.W)
      case J_TYPE         => "b00000000000000000000_000_00000_1111111".U(32.W)
      case F12_TYPE       => "b111111111111_00000_111_00000_1111111".U(32.W)
      case R_TYPE_NNP     => "b1111111_00000_00000_111_0000_1_1111111".U(32.W)
      case R_TYPE_NPN     => "b1111111_00000_0000_1_111_00000_1111111".U(32.W)
      case R_TYPE_PNN     => "b1111111_0000_1_00000_111_00000_1111111".U(32.W)
      case R_TYPE_NPP     => "b1111111_00000_0000_1_111_0000_1_1111111".U(32.W)
      case R_TYPE_PNP     => "b1111111_0000_1_00000_111_0000_1_1111111".U(32.W)
      case R_TYPE_PPN     => "b1111111_0000_1_0000_1_111_00000_1111111".U(32.W)
      case R_TYPE_PPP     => "b1111111_0000_1_0000_1_111_0000_1_1111111".U(32.W)
      case I_TYPE_F7      => "b1111111_00000_00000_111_00000_1111111".U(32.W)
      case I_TYPE_BYTE    => "b11111_11_1_00000000_1_111_00000_1111111".U(32.W)
      case I_TYPE_F7_P    => "b1111111_00000_00000_111_00001_1111111".U(32.W)
      case I_TYPE_BYTE_P  => "b11111_11_1_00000000_1_111_00001_1111111".U(32.W)
      case I_TYPE_HBYTE_P => "b11111_11_1_0000_0000_1_111_00000_1111111".U(32.W)
      case I_TYPE_5BIT_P  => "b1111111_00000_0000_1_111_00000_1111111".U(32.W)
      case F_TYPE         => "b11111111111111111111111111111111".U(32.W)
      case _              => 0.U(32.W)
    }
  }
}

object RISCV_TYPE extends ChiselEnum {
  import scala.language.implicitConversions

  implicit private def enumTypeToBigInt[T <: EnumType](value: T): BigInt =
    value.litValue

  implicit private def bigIntToRiscVType[T <: BigInt](
      value: T
  ): RISCV_TYPE.Type = Value(value.U)

  def getFormat(t: RISCV_TYPE.Type): RISCV_FORMAT.Type = {
    return RISCV_FORMAT(t.asUInt(36, 32))
  }

  def getFormat(t: UInt): UInt = {
    t(36, 32)
  }

  def getFormatOffset(): Int = 32

  def getOP(t: RISCV_TYPE.Type): RISCV_OP.Type = {
    return RISCV_OP(Reverse(t.asUInt(31, 0))(6, 0))
  }

  def getOP(t: UInt): UInt = {
    return Reverse(t(31, 0))(6, 0)
  }

  def getOPOffset(): Int = 0

  def getFunct3(t: RISCV_TYPE.Type): RISCV_FUNCT3.Type = {
    return RISCV_FUNCT3(Reverse(t.asUInt(31, 0))(14, 12))
  }

  def getFunct3(t: UInt): UInt = {
    return Reverse(t(31, 0))(14, 12)
  }

  def getFunct3Offset(): Int = 12

  def getFunct7(t: RISCV_TYPE.Type): RISCV_FUNCT7.Type = {
    return RISCV_FUNCT7(Reverse(t.asUInt(31, 0))(31, 25))
  }

  def getFunct7(t: UInt): UInt = {
    return Reverse(t(31, 0))(31, 25)
  }

  def getFunct7Offset(): Int = 25

  def getFunct12(t: RISCV_TYPE.Type): RISCV_FUNCT12.Type = {
    return RISCV_FUNCT12(Reverse(t.asUInt(31, 0))(31, 20))
  }

  def getFunct12(t: UInt): UInt = {
    return Reverse(t(31, 0))(31, 20)
  }

  def getFunct12Offset(): Int = 20

  private def reverseBits32(n: BigInt): BigInt = {
    var x = n & 0xffffffffL
    var result = BigInt(0)

    for (_ <- 0 until 32) {
      result = (result << 1) | (x & 1)
      x = x >> 1
    }

    result
  }

  // | format (5) | op (7) | 0s (25) |
  def concat(op: RISCV_OP.Type, format: RISCV_FORMAT.Type): RISCV_TYPE.Type =
    reverseBits32(op << getOPOffset()) + (format << getFormatOffset())

  // | format (5) | op (7) | 0s (5) | funct3 (3) | 0s (17) |
  def concat(
      funct3: RISCV_FUNCT3.Type,
      op: RISCV_OP.Type,
      format: RISCV_FORMAT.Type
  ): RISCV_TYPE.Type =
    reverseBits32(
      (op << getOPOffset()) + (funct3 << getFunct3Offset())
    ) + (format << getFormatOffset())

  // | format (5) | op (7) | 0s (5) | funct3 (3) | 0s (10) | funct7 (7) |
  // or
  // | format (5) | op (7) | 0s (5) | funct3 (3) | 0s (5) | funct12 (12) |
  def concat[FUNCT7OR12: ORTYPE[RISCV_FUNCT7.Type, RISCV_FUNCT12.Type]#check](
      funct7or12: FUNCT7OR12,
      funct3: RISCV_FUNCT3.Type,
      op: RISCV_OP.Type,
      format: RISCV_FORMAT.Type
  ): RISCV_TYPE.Type = funct7or12 match {
    case funct7: RISCV_FUNCT7.Type =>
      reverseBits32(
        (op << getOPOffset()) + (funct3 << getFunct3Offset()) + (funct7 << getFunct7Offset())
      ) + (format << getFormatOffset())
    case funct12: RISCV_FUNCT12.Type =>
      reverseBits32(
        (op << getOPOffset()) + (funct3 << getFunct3Offset()) + (funct12 << getFunct12Offset())
      ) + (format << getFormatOffset())
  }

  def instr_to_riscvtype(i: UInt): RISCV_TYPE.Type = {

    val riscvtype = Wire(RISCV_TYPE.Type())
    riscvtype := UNKNOWN
    for (t <- RISCV_FORMAT.all) {
      val mask = RISCV_FORMAT.toBitMask(t)
      val masked = Wire(UInt(32.W))
      masked := mask & i
      when(masked =/= 0.U) {
        val candidate = Cat(t.asUInt, Reverse(masked))
        when(RISCV_TYPE.safe(candidate)._2) {
          riscvtype := RISCV_TYPE.safe(candidate)._1
        }
      }
    }
    return riscvtype
  }

  val add = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val sub = concat(
    RISCV_FUNCT7.SUB_SRA,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val mul = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val xor = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F100,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val xnor = concat(
    RISCV_FUNCT7.SUB_SRA,
    RISCV_FUNCT3.F100,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val sh2add = concat(
    RISCV_FUNCT7.SHIFT,
    RISCV_FUNCT3.F100,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val pack = concat(
    RISCV_FUNCT7.SHFL_PWADDA_H_PACK,
    RISCV_FUNCT3.F100,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val div = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F100,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val min = concat(
    RISCV_FUNCT7.MINMAX,
    RISCV_FUNCT3.F100,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val slt = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val sh1add = concat(
    RISCV_FUNCT7.SHIFT,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val mulhsu = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val or = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val orn = concat(
    RISCV_FUNCT7.SUB_SRA,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val sh3add = concat(
    RISCV_FUNCT7.SHIFT,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val rem = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val max = concat(
    RISCV_FUNCT7.MINMAX,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val sll = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val slo = concat(
    RISCV_FUNCT7.SHIFT,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val rol = concat(
    RISCV_FUNCT7.ROTATE_PNCLIPI_B,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val shfl = concat(
    RISCV_FUNCT7.SHFL_PWADDA_H_PACK,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val bclr = concat(
    RISCV_FUNCT7.CLEAR,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val bset = concat(
    RISCV_FUNCT7.SET,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val binv = concat(
    RISCV_FUNCT7.INV,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val mulh = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val srl = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val sra = concat(
    RISCV_FUNCT7.SUB_SRA,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val sro = concat(
    RISCV_FUNCT7.SHIFT,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val ror = concat(
    RISCV_FUNCT7.ROTATE_PNCLIPI_B,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val unshfl = concat(
    RISCV_FUNCT7.SHFL_PWADDA_H_PACK,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val bext = concat(
    RISCV_FUNCT7.CLEAR,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val gorc = concat(
    RISCV_FUNCT7.SET,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val grev = concat(
    RISCV_FUNCT7.INV,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val divu = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val minu = concat(
    RISCV_FUNCT7.MINMAX,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val sltu = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F011,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val mulhu = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F011,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val and = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F111,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val andn = concat(
    RISCV_FUNCT7.SUB_SRA,
    RISCV_FUNCT3.F111,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  ) // Bitmanip
  val remu = concat(
    RISCV_FUNCT7.MULDIV,
    RISCV_FUNCT3.F111,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )
  val maxu = concat(
    RISCV_FUNCT7.MINMAX,
    RISCV_FUNCT3.F111,
    RISCV_OP.OP,
    RISCV_FORMAT.R_TYPE
  )

  val psshl_hs = concat(
    RISCV_FUNCT7.MVM_PSSHL_HS_PM2ADDAU_H,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE
  )
  val pssha_hs = concat(
    RISCV_FUNCT7.PSSHA_HS_PM2ADDASU_H,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE
  )
  val padd_hs = concat(
    RISCV_FUNCT7.PADD_HS_PAADD_H_PM2ADDA_HX,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE
  )
  val psshlr_hs = concat(
    RISCV_FUNCT7.PSSHLR_HS,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE
  )
  val psshar_hs = concat(
    RISCV_FUNCT7.PSSHAR_HS,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE
  )
  val padd_bs = concat(
    RISCV_FUNCT7.PADD_BS_PAADD_B,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE
  )

  val padd_h = concat(
    RISCV_FUNCT7.PADD_H,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val psub_h = concat(
    RISCV_FUNCT7.PSUB_H,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val psadd_h = concat(
    RISCV_FUNCT7.PSADD_H_PADD_HX,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val paadd_h = concat(
    RISCV_FUNCT7.PADD_HS_PAADD_H_PM2ADDA_HX,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val padd_b = concat(
    RISCV_FUNCT7.PADD_B,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val psub_b = concat(
    RISCV_FUNCT7.PSUB_B_PM2SADD_H,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val psadd_b = concat(
    RISCV_FUNCT7.PSADD_B,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val paadd_b = concat(
    RISCV_FUNCT7.PADD_BS_PAADD_B,
    RISCV_FUNCT3.F000,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )

  val mvm = concat(
    RISCV_FUNCT7.MVM_PSSHL_HS_PM2ADDAU_H,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val merge = concat(
    RISCV_FUNCT7.MERGE,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val mvmn = concat(
    RISCV_FUNCT7.MVMN,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )

  val pm2add_h = concat(
    RISCV_FUNCT7.PADD_H,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val pm2addu_h = concat(
    RISCV_FUNCT7.PADDU_H,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val pm2addsu_h = concat(
    RISCV_FUNCT7.PADDSU_H,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val pm2add_hx = concat(
    RISCV_FUNCT7.PSADD_H_PADD_HX,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )

  val pm2adda_h = concat(
    RISCV_FUNCT7.PM2ADDA_H,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val pm2addau_h = concat(
    RISCV_FUNCT7.MVM_PSSHL_HS_PM2ADDAU_H,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val pm2addasu_h = concat(
    RISCV_FUNCT7.PSSHA_HS_PM2ADDASU_H,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val pm2adda_hx = concat(
    RISCV_FUNCT7.PADD_HS_PAADD_H_PM2ADDA_HX,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )

  val pm2sadd_h = concat(
    RISCV_FUNCT7.PSUB_B_PM2SADD_H,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )
  val pm2sadd_hx = concat(
    RISCV_FUNCT7.PM2SADD_HX,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_32,
    RISCV_FORMAT.R_TYPE
  )

  val lb = concat(RISCV_FUNCT3.F000, RISCV_OP.LOAD, RISCV_FORMAT.I_TYPE)
  val lbu = concat(RISCV_FUNCT3.F100, RISCV_OP.LOAD, RISCV_FORMAT.I_TYPE)
  val lw = concat(RISCV_FUNCT3.F010, RISCV_OP.LOAD, RISCV_FORMAT.I_TYPE)
  val lh = concat(RISCV_FUNCT3.F001, RISCV_OP.LOAD, RISCV_FORMAT.I_TYPE)
  val lhu = concat(RISCV_FUNCT3.F101, RISCV_OP.LOAD, RISCV_FORMAT.I_TYPE)
  val addi = concat(RISCV_FUNCT3.F000, RISCV_OP.OP_IMM, RISCV_FORMAT.I_TYPE)
  val xori = concat(RISCV_FUNCT3.F100, RISCV_OP.OP_IMM, RISCV_FORMAT.I_TYPE)
  val slti = concat(RISCV_FUNCT3.F010, RISCV_OP.OP_IMM, RISCV_FORMAT.I_TYPE)
  val ori = concat(RISCV_FUNCT3.F110, RISCV_OP.OP_IMM, RISCV_FORMAT.I_TYPE)
  val sltiu = concat(RISCV_FUNCT3.F011, RISCV_OP.OP_IMM, RISCV_FORMAT.I_TYPE)
  val andi = concat(RISCV_FUNCT3.F111, RISCV_OP.OP_IMM, RISCV_FORMAT.I_TYPE)
  val csrrs = concat(RISCV_FUNCT3.F010, RISCV_OP.SYSTEM, RISCV_FORMAT.I_TYPE)
  val csrrsi = concat(RISCV_FUNCT3.F110, RISCV_OP.SYSTEM, RISCV_FORMAT.I_TYPE)
  val csrrw = concat(RISCV_FUNCT3.F001, RISCV_OP.SYSTEM, RISCV_FORMAT.I_TYPE)
  val csrrwi = concat(RISCV_FUNCT3.F101, RISCV_OP.SYSTEM, RISCV_FORMAT.I_TYPE)
  val csrrc = concat(RISCV_FUNCT3.F011, RISCV_OP.SYSTEM, RISCV_FORMAT.I_TYPE)
  val csrrci = concat(RISCV_FUNCT3.F111, RISCV_OP.SYSTEM, RISCV_FORMAT.I_TYPE)
  val sb = concat(RISCV_FUNCT3.F000, RISCV_OP.STORE, RISCV_FORMAT.S_TYPE)
  val sw = concat(RISCV_FUNCT3.F010, RISCV_OP.STORE, RISCV_FORMAT.S_TYPE)
  val sh = concat(RISCV_FUNCT3.F001, RISCV_OP.STORE, RISCV_FORMAT.S_TYPE)
  val fence = concat(RISCV_FUNCT3.F000, RISCV_OP.MISC_MEM, RISCV_FORMAT.S_TYPE)
  val pause = concat(
    RISCV_FUNCT12.PAUSE,
    RISCV_FUNCT3.F000,
    RISCV_OP.MISC_MEM,
    RISCV_FORMAT.S_TYPE
  )
  val fence_iso = concat(
    RISCV_FUNCT12.FENCE_ISO,
    RISCV_FUNCT3.F000,
    RISCV_OP.MISC_MEM,
    RISCV_FORMAT.S_TYPE
  )
  val beq = concat(RISCV_FUNCT3.F000, RISCV_OP.BRANCH, RISCV_FORMAT.B_TYPE)
  val blt = concat(RISCV_FUNCT3.F100, RISCV_OP.BRANCH, RISCV_FORMAT.B_TYPE)
  val bltu = concat(RISCV_FUNCT3.F110, RISCV_OP.BRANCH, RISCV_FORMAT.B_TYPE)
  val bne = concat(RISCV_FUNCT3.F001, RISCV_OP.BRANCH, RISCV_FORMAT.B_TYPE)
  val bge = concat(RISCV_FUNCT3.F101, RISCV_OP.BRANCH, RISCV_FORMAT.B_TYPE)
  val bgeu = concat(RISCV_FUNCT3.F111, RISCV_OP.BRANCH, RISCV_FORMAT.B_TYPE)
  val jalr = concat(RISCV_OP.JALR, RISCV_FORMAT.U_TYPE)
  val auipc = concat(RISCV_OP.AUIPC, RISCV_FORMAT.U_TYPE)
  val lui = concat(RISCV_OP.LUI, RISCV_FORMAT.U_TYPE)
  val jal = concat(RISCV_OP.JAL, RISCV_FORMAT.U_TYPE)
  val clz = concat(
    RISCV_FUNCT12.CLZ,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32b = concat(
    RISCV_FUNCT12.CRC32B,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32cb = concat(
    RISCV_FUNCT12.CRC32CB,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val sextb = concat(
    RISCV_FUNCT12.SEXTB,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val cpop = concat(
    RISCV_FUNCT12.CPOP,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32w = concat(
    RISCV_FUNCT12.CRC32W,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32cw = concat(
    RISCV_FUNCT12.CRC32CW,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val ctz = concat(
    RISCV_FUNCT12.CTZ,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32h = concat(
    RISCV_FUNCT12.CRC32H,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32ch = concat(
    RISCV_FUNCT12.CRC32CH,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val sexth = concat(
    RISCV_FUNCT12.SEXTH,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val bmatflip = concat(
    RISCV_FUNCT12.BMATFLIP,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32d = concat(
    RISCV_FUNCT12.CRC32D,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val crc32cd = concat(
    RISCV_FUNCT12.CRC32CD,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.F12_TYPE
  )
  val ecall = concat(
    RISCV_FUNCT12.ECALL,
    RISCV_FUNCT3.F000,
    RISCV_OP.SYSTEM,
    RISCV_FORMAT.F12_TYPE
  )
  val sret = concat(
    RISCV_FUNCT12.SRET,
    RISCV_FUNCT3.F000,
    RISCV_OP.SYSTEM,
    RISCV_FORMAT.F12_TYPE
  )
  val mret = concat(
    RISCV_FUNCT12.MRET,
    RISCV_FUNCT3.F000,
    RISCV_OP.SYSTEM,
    RISCV_FORMAT.F12_TYPE
  )
  val ebreak = concat(
    RISCV_FUNCT12.EBREAK,
    RISCV_FUNCT3.F000,
    RISCV_OP.SYSTEM,
    RISCV_FORMAT.F12_TYPE
  )
  val wfi = concat(
    RISCV_FUNCT12.WFI,
    RISCV_FUNCT3.F000,
    RISCV_OP.SYSTEM,
    RISCV_FORMAT.F12_TYPE
  )

  val pwadd_h: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F010 << getFunct3Offset()) + (RISCV_FUNCT7.ZERO << getFunct7Offset()) + (1 << (getOPOffset() + 7))
  ) + (RISCV_FORMAT.R_TYPE_NNP << getFormatOffset())
  val pwadda_h: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F010 << getFunct3Offset()) + (RISCV_FUNCT7.SHFL_PWADDA_H_PACK << getFunct7Offset()) + (1 << (getOPOffset() + 7))
  ) + (RISCV_FORMAT.R_TYPE_NNP << getFormatOffset())
  val pwadd_b: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F010 << getFunct3Offset()) + (RISCV_FUNCT7.PWADD_B << getFunct7Offset()) + (1 << (getOPOffset() + 7))
  ) + (RISCV_FORMAT.R_TYPE_NNP << getFormatOffset())
  val pwadda_b: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F010 << getFunct3Offset()) + (RISCV_FUNCT7.PWADDA_B << getFunct7Offset()) + (1 << (getOPOffset() + 7))
  ) + (RISCV_FORMAT.R_TYPE_NNP << getFormatOffset())

  val padd_dhs = concat(
    RISCV_FUNCT7.PADD_DHS,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_NPP
  )
  val padd_dbs = concat(
    RISCV_FUNCT7.PADD_DBS,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_NPP
  )
  val padd_dws = concat(
    RISCV_FUNCT7.PADD_DWS,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_NPP
  )

  val padd_dh = concat(
    RISCV_FUNCT7.PADD_H,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val psub_dh = concat(
    RISCV_FUNCT7.PSUB_H,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val psadd_dh = concat(
    RISCV_FUNCT7.PSADD_H_PADD_HX,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val paadd_dh = concat(
    RISCV_FUNCT7.PADD_HS_PAADD_H_PM2ADDA_HX,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val padd_db = concat(
    RISCV_FUNCT7.PADD_B,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val psub_db = concat(
    RISCV_FUNCT7.PSUB_B_PM2SADD_H,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val psadd_db = concat(
    RISCV_FUNCT7.PSADD_B,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val paadd_db = concat(
    RISCV_FUNCT7.PADD_BS_PAADD_B,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val padd_dw = concat(
    RISCV_FUNCT7.PADD_W,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val psub_dw = concat(
    RISCV_FUNCT7.PSUB_W,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val psadd_dw = concat(
    RISCV_FUNCT7.PSADD_W,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )
  val paadd_dw = concat(
    RISCV_FUNCT7.PADD_WS_PAADD_W,
    RISCV_FUNCT3.F110,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.R_TYPE_PPP
  )

  val slli = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val sloi = concat(
    RISCV_FUNCT7.SHIFT,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val shfli = concat(
    RISCV_FUNCT7.SHFL_PWADDA_H_PACK,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val bclri = concat(
    RISCV_FUNCT7.CLEAR,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val bseti = concat(
    RISCV_FUNCT7.SET,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val binvi = concat(
    RISCV_FUNCT7.INV,
    RISCV_FUNCT3.F001,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val srli = concat(
    RISCV_FUNCT7.ZERO,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val srai = concat(
    RISCV_FUNCT7.SUB_SRA,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val sroi = concat(
    RISCV_FUNCT7.SHIFT,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val rori = concat(
    RISCV_FUNCT7.ROTATE_PNCLIPI_B,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val unshfli = concat(
    RISCV_FUNCT7.SHFL_PWADDA_H_PACK,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val bexti = concat(
    RISCV_FUNCT7.CLEAR,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val gorci = concat(
    RISCV_FUNCT7.SET,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )
  val grevi = concat(
    RISCV_FUNCT7.INV,
    RISCV_FUNCT3.F101,
    RISCV_OP.OP_IMM,
    RISCV_FORMAT.I_TYPE_F7
  )

  val pli_h = concat(
    RISCV_FUNCT7.PLI_H,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.I_TYPE_F7
  )
  val plui_h = concat(
    RISCV_FUNCT7.PLUI_H,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.I_TYPE_F7
  )

  val pli_b: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F010 << getFunct3Offset()) + (RISCV_FUNCT7.PLI_B << getFunct7Offset())
  ) + (RISCV_FORMAT.I_TYPE_BYTE << getFormatOffset())

  val pli_dh = concat(
    RISCV_FUNCT7.PLI_DH,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.I_TYPE_F7_P
  )
  val plui_dh = concat(
    RISCV_FUNCT7.PLUI_DH_PNCLIPRI_B,
    RISCV_FUNCT3.F010,
    RISCV_OP.OP_IMM_32,
    RISCV_FORMAT.I_TYPE_F7_P
  )

  val pli_db: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F010 << getFunct3Offset()) + (RISCV_FUNCT7.PLI_DB << getFunct7Offset())
  ) + (RISCV_FORMAT.I_TYPE_BYTE_P << getFormatOffset())

  val pnclipi_b: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F100 << getFunct3Offset()) + (RISCV_FUNCT7.ROTATE_PNCLIPI_B << getFunct7Offset()) + (1 << (getFunct7Offset() - 1)) + (1 << (getFunct3Offset() + 3))
  ) + (RISCV_FORMAT.I_TYPE_HBYTE_P << getFormatOffset())
  val pnclipri_b: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F100 << getFunct3Offset()) + (RISCV_FUNCT7.PLUI_DH_PNCLIPRI_B << getFunct7Offset()) + (1 << (getFunct7Offset() - 1)) + (1 << (getFunct3Offset() + 3))
  ) + (RISCV_FORMAT.I_TYPE_HBYTE_P << getFormatOffset())

  val pnclipi_h: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F100 << getFunct3Offset()) + (RISCV_FUNCT7.PNCLIPI_H << getFunct7Offset()) + (1 << (getFunct3Offset() + 3))
  ) + (RISCV_FORMAT.I_TYPE_5BIT_P << getFormatOffset())
  val pnclipri_h: RISCV_TYPE.Type = reverseBits32(
    (RISCV_OP.OP_IMM_32 << getOPOffset()) + (RISCV_FUNCT3.F100 << getFunct3Offset()) + (RISCV_FUNCT7.PNCLIPRI_H << getFunct7Offset()) + (1 << (getFunct3Offset() + 3))
  ) + (RISCV_FORMAT.I_TYPE_5BIT_P << getFormatOffset())

  val UNKNOWN =
    concat(
      RISCV_FUNCT12.UNKNOWN,
      RISCV_FUNCT3.F111,
      RISCV_OP.UNKNOWN,
      RISCV_FORMAT.UNKNOWN
    )
}

object InstructionSets {
  val RV32I = Seq(
    RISCV_TYPE.lb,
    RISCV_TYPE.lh,
    RISCV_TYPE.lw,
    RISCV_TYPE.lbu,
    RISCV_TYPE.lhu,
    RISCV_TYPE.fence,
    RISCV_TYPE.pause,
    RISCV_TYPE.fence_iso,
    RISCV_TYPE.addi,
    RISCV_TYPE.slli,
    RISCV_TYPE.slti,
    RISCV_TYPE.sltiu,
    RISCV_TYPE.xori,
    RISCV_TYPE.srli,
    RISCV_TYPE.srai,
    RISCV_TYPE.ori,
    RISCV_TYPE.andi,
    RISCV_TYPE.auipc,
    RISCV_TYPE.sb,
    RISCV_TYPE.sh,
    RISCV_TYPE.sw,
    RISCV_TYPE.add,
    RISCV_TYPE.sub,
    RISCV_TYPE.sll,
    RISCV_TYPE.slt,
    RISCV_TYPE.sltu,
    RISCV_TYPE.xor,
    RISCV_TYPE.srl,
    RISCV_TYPE.sra,
    RISCV_TYPE.or,
    RISCV_TYPE.and,
    RISCV_TYPE.lui,
    RISCV_TYPE.beq,
    RISCV_TYPE.bne,
    RISCV_TYPE.blt,
    RISCV_TYPE.bge,
    RISCV_TYPE.bltu,
    RISCV_TYPE.bgeu,
    RISCV_TYPE.jalr,
    RISCV_TYPE.jal
  )

  val RV32M = Seq(
    RISCV_TYPE.mul,
    RISCV_TYPE.mulh,
    RISCV_TYPE.mulhsu,
    RISCV_TYPE.mulhu
  )

  val RV32Div = Seq(
    RISCV_TYPE.div,
    RISCV_TYPE.divu,
    RISCV_TYPE.rem,
    RISCV_TYPE.remu
  )

  val BasicBit = Seq(
    RISCV_TYPE.clz,
    RISCV_TYPE.ctz,
    RISCV_TYPE.cpop,
    RISCV_TYPE.min,
    RISCV_TYPE.max,
    RISCV_TYPE.minu,
    RISCV_TYPE.maxu
  )

  val BitPerm = Seq(
    RISCV_TYPE.grev,
    RISCV_TYPE.rol,
    RISCV_TYPE.ror,
    RISCV_TYPE.grevi,
    RISCV_TYPE.rori,
    RISCV_TYPE.shfl,
    RISCV_TYPE.unshfl,
    RISCV_TYPE.shfli,
    RISCV_TYPE.unshfli
  )

  val MachineMode = Seq(
    RISCV_TYPE.ecall,
    RISCV_TYPE.ebreak,
    RISCV_TYPE.sret,
    RISCV_TYPE.wfi,
    RISCV_TYPE.mret,
    RISCV_TYPE.csrrw,
    RISCV_TYPE.csrrs,
    RISCV_TYPE.csrrc,
    RISCV_TYPE.csrrwi,
    RISCV_TYPE.csrrsi,
    RISCV_TYPE.csrrci
  )

  val BasicPacked = Seq(
    RISCV_TYPE.pli_b,
    RISCV_TYPE.pli_h,
    RISCV_TYPE.plui_h,
    RISCV_TYPE.padd_b,
    RISCV_TYPE.padd_h,
    RISCV_TYPE.padd_bs,
    RISCV_TYPE.padd_hs,
    RISCV_TYPE.psub_b,
    RISCV_TYPE.psub_h,
    RISCV_TYPE.psadd_b,
    RISCV_TYPE.psadd_h,
    RISCV_TYPE.paadd_b,
    RISCV_TYPE.paadd_h
  )

  val DoubleWidthPacked = Seq(
    RISCV_TYPE.pli_db,
    RISCV_TYPE.pli_dh,
    RISCV_TYPE.plui_dh,
    RISCV_TYPE.padd_db,
    RISCV_TYPE.padd_dh,
    RISCV_TYPE.padd_dw,
    RISCV_TYPE.padd_dbs,
    RISCV_TYPE.padd_dhs,
    RISCV_TYPE.padd_dws,
    RISCV_TYPE.psub_db,
    RISCV_TYPE.psub_dh,
    RISCV_TYPE.psub_dw,
    RISCV_TYPE.psadd_db,
    RISCV_TYPE.psadd_dh,
    RISCV_TYPE.psadd_dw,
    RISCV_TYPE.paadd_db,
    RISCV_TYPE.paadd_dh,
    RISCV_TYPE.paadd_dw,
    RISCV_TYPE.pwadd_b,
    RISCV_TYPE.pwadda_b,
    RISCV_TYPE.pwadd_h,
    RISCV_TYPE.pwadda_h
  )

  val Merge = Seq(
    RISCV_TYPE.mvm,
    RISCV_TYPE.mvmn,
    RISCV_TYPE.merge,
    RISCV_TYPE.pack
  )

  val ComplexPacked = Seq(
    RISCV_TYPE.pssha_hs,
    RISCV_TYPE.psshar_hs,
    RISCV_TYPE.psshl_hs,
    RISCV_TYPE.psshlr_hs,
    RISCV_TYPE.pnclipi_b,
    RISCV_TYPE.pnclipri_b,
    RISCV_TYPE.pnclipi_h,
    RISCV_TYPE.pnclipri_h,
    RISCV_TYPE.pm2add_h,
    RISCV_TYPE.pm2addu_h,
    RISCV_TYPE.pm2addsu_h,
    RISCV_TYPE.pm2add_hx,
    RISCV_TYPE.pm2adda_h,
    RISCV_TYPE.pm2addau_h,
    RISCV_TYPE.pm2addasu_h,
    RISCV_TYPE.pm2adda_hx,
    RISCV_TYPE.pm2sadd_h,
    RISCV_TYPE.pm2sadd_hx
  )
}
