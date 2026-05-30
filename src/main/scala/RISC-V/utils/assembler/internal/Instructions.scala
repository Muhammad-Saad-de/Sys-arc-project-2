package RISCV.utils.assembler

object InstType extends Enumeration {
  type Type = Value
  val I, I3, R, R1, R3, B, S, U, J, C, CI, PLI, PI, PR = Value
}

case class Instruction(
    name: String,
    realName: String = "",
    instType: InstType.Type,
    funct3: String = "",
    funct7: String = "",
    rs2: String = "",
    funct2: String = "",
    funct1: String = "",
    opcode: String,
    hasOffset: Boolean = false,
    isCsr: Boolean = false,
    hasImm: Boolean = false,
    fixed: String = "",
    rd_lsb: String = "",
    rs1_lsb: String = "",
    rs2_lsb: String = "",
    imm_lsb: String = "",
    imm_msb: String = ""
)

protected object Instructions {
  def apply(instruction: String): Option[Instruction] = {
    try {
      instructions.find(_.name == instruction.toUpperCase)
    } catch {
      case _: Exception =>
        throw new RISCVAssemblerException(
          s"Instruction $instruction not found."
        )
    }
  }

  // scalafmt: { maxColumn = 130}
  private val instructions = List(
    Instruction(name = "LUI", instType = InstType.U, opcode = "0110111"),
    Instruction(name = "AUIPC", instType = InstType.U, opcode = "0010111", funct3 = "001"),
    Instruction(name = "JAL", instType = InstType.J, opcode = "1101111"),
    Instruction(name = "JALR", instType = InstType.I, opcode = "1100111", funct3 = "000"),
    Instruction(name = "BEQ", instType = InstType.B, opcode = "1100011", funct3 = "000"),
    Instruction(name = "BNE", instType = InstType.B, opcode = "1100011", funct3 = "001"),
    Instruction(name = "BLT", instType = InstType.B, opcode = "1100011", funct3 = "100"),
    Instruction(name = "BGE", instType = InstType.B, opcode = "1100011", funct3 = "101"),
    Instruction(name = "BLTU", instType = InstType.B, opcode = "1100011", funct3 = "110"),
    Instruction(name = "BGEU", instType = InstType.B, opcode = "1100011", funct3 = "111"),
    Instruction(name = "LB", instType = InstType.I, opcode = "0000011", funct3 = "000", hasOffset = true),
    Instruction(name = "LH", instType = InstType.I, opcode = "0000011", funct3 = "001", hasOffset = true),
    Instruction(name = "LW", instType = InstType.I, opcode = "0000011", funct3 = "010", hasOffset = true),
    Instruction(name = "LBU", instType = InstType.I, opcode = "0000011", funct3 = "100", hasOffset = true),
    Instruction(name = "LHU", instType = InstType.I, opcode = "0000011", funct3 = "101", hasOffset = true),
    Instruction(name = "SB", instType = InstType.S, opcode = "0100011", funct3 = "000"),
    Instruction(name = "SH", instType = InstType.S, opcode = "0100011", funct3 = "001"),
    Instruction(name = "SW", instType = InstType.S, opcode = "0100011", funct3 = "010"),
    Instruction(name = "ADDI", instType = InstType.I, opcode = "0010011", funct3 = "000"),
    Instruction(name = "SLTI", instType = InstType.I, opcode = "0010011", funct3 = "010"),
    Instruction(name = "SLTIU", instType = InstType.I, opcode = "0010011", funct3 = "011"),
    Instruction(name = "XORI", instType = InstType.I, opcode = "0010011", funct3 = "100"),
    Instruction(name = "ORI", instType = InstType.I, opcode = "0010011", funct3 = "110"),
    Instruction(name = "ANDI", instType = InstType.I, opcode = "0010011", funct3 = "111"),
    Instruction(name = "SLLI", instType = InstType.I, opcode = "0010011", funct3 = "001", fixed = "0000000"),
    Instruction(name = "SRLI", instType = InstType.I, opcode = "0010011", funct3 = "101", fixed = "0000000"),
    Instruction(name = "SRAI", instType = InstType.I, opcode = "0010011", funct3 = "101", fixed = "0100000"),
    Instruction(name = "ADD", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "000"),
    Instruction(name = "SUB", instType = InstType.R, opcode = "0110011", funct7 = "0100000", funct3 = "000"),
    Instruction(name = "SLL", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "001"),
    Instruction(name = "SLT", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "010"),
    Instruction(name = "SLTU", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "011"),
    Instruction(name = "XOR", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "100"),
    Instruction(name = "SRL", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "101"),
    Instruction(name = "SRA", instType = InstType.R, opcode = "0110011", funct7 = "0100000", funct3 = "101"),
    Instruction(name = "OR", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "110"),
    Instruction(name = "AND", instType = InstType.R, opcode = "0110011", funct7 = "0000000", funct3 = "111"),
    Instruction(name = "FENCE", instType = InstType.I, opcode = "0001111", funct3 = "000"),
    Instruction(name = "FENCE.I", instType = InstType.I, opcode = "0001111", funct3 = "001", fixed = "000000000000"),
    Instruction(name = "ECALL", instType = InstType.I, opcode = "1110011", funct3 = "000", fixed = "000000000000"),
    Instruction(name = "EBREAK", instType = InstType.I, opcode = "1110011", funct3 = "000", fixed = "000000000001"),
    // Instructions below are still not implemented
    Instruction(name = "CSRRW", instType = InstType.C, opcode = "1110011", funct3 = "001", isCsr = true, hasImm = false),
    Instruction(name = "CSRRS", instType = InstType.C, opcode = "1110011", funct3 = "010", isCsr = true, hasImm = false),
    Instruction(name = "CSRRC", instType = InstType.C, opcode = "1110011", funct3 = "011", isCsr = true, hasImm = false),
    Instruction(name = "CSRRWI", instType = InstType.CI, opcode = "1110011", funct3 = "101", isCsr = true, hasImm = true),
    Instruction(name = "CSRRSI", instType = InstType.CI, opcode = "1110011", funct3 = "110", isCsr = true, hasImm = true),
    Instruction(name = "CSRRCI", instType = InstType.CI, opcode = "1110011", funct3 = "111", isCsr = true, hasImm = true),

    // RV32M
    Instruction(name = "MUL", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "000"),
    Instruction(name = "MULH", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "001"),
    Instruction(name = "MULHSU", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "010"),
    Instruction(name = "MULHU", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "011"),
    Instruction(name = "DIV", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "100"),
    Instruction(name = "DIVU", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "101"),
    Instruction(name = "REM", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "110"),
    Instruction(name = "REMU", instType = InstType.R, opcode = "0110011", funct7 = "0000001", funct3 = "111"),
    Instruction(name = "SRET", instType = InstType.I, opcode = "1110011", funct3 = "000", fixed = "000100000010"),
    Instruction(name = "MRET", instType = InstType.I, opcode = "1110011", funct3 = "000", fixed = "001100000010"),
    Instruction(name = "MNRET", instType = InstType.I, opcode = "1110011", funct3 = "000", fixed = "011100000010"),
    Instruction(name = "WFI", instType = InstType.I, opcode = "1110011", funct3 = "000", fixed = "000100000101"),

    // RV32B (as of Draft 0.93, https://github.com/riscv/riscv-bitmanip/releases/download/v0.93/bitmanip-0.93.pdf)

    // Zbb
    Instruction(name = "CLZ", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "00000"),
    Instruction(name = "CTZ", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "00001"),
    Instruction(name = "CPOP", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "00010"),
    Instruction(name = "MIN", instType = InstType.R, opcode = "0110011", funct7 = "0000101", funct3 = "100"),
    Instruction(name = "MINU", instType = InstType.R, opcode = "0110011", funct7 = "0000101", funct3 = "101"),
    Instruction(name = "MAX", instType = InstType.R, opcode = "0110011", funct7 = "0000101", funct3 = "110"),
    Instruction(name = "MAXU", instType = InstType.R, opcode = "0110011", funct7 = "0000101", funct3 = "111"),
    Instruction(name = "SEXT.B", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "00100"),
    Instruction(name = "SEXT.H", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "00101"),
    Instruction(name = "ANDN", instType = InstType.R, opcode = "0110011", funct7 = "0100000", funct3 = "111"),
    Instruction(name = "ORN", instType = InstType.R, opcode = "0110011", funct7 = "0100000", funct3 = "110"),
    Instruction(name = "XNOR", instType = InstType.R, opcode = "0110011", funct7 = "0100000", funct3 = "100"),
    Instruction(name = "ROL", instType = InstType.R, opcode = "0110011", funct7 = "0110000", funct3 = "001"),
    Instruction(name = "ROR", instType = InstType.R, opcode = "0110011", funct7 = "0110000", funct3 = "101"),
    Instruction(
      name = "RORI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "101",
      fixed = "0110000"
    ), // TODO: check fixed part length

    // Zbp
    Instruction(name = "PACK", instType = InstType.R, opcode = "0110011", funct7 = "0000100", funct3 = "100"),
    Instruction(name = "PACKU", instType = InstType.R, opcode = "0110011", funct7 = "0100100", funct3 = "100"),
    Instruction(name = "PACKH", instType = InstType.R, opcode = "0110011", funct7 = "0000100", funct3 = "111"),
    Instruction(name = "GREV", instType = InstType.R, opcode = "0110011", funct7 = "0110100", funct3 = "101"),
    Instruction(
      name = "GREVI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "101",
      fixed = "0110100"
    ), // TODO: check fixed part length

    Instruction(name = "GORC", instType = InstType.R, opcode = "0110011", funct7 = "0010100", funct3 = "101"),
    Instruction(
      name = "GORCI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "101",
      fixed = "0010100"
    ), // TODO: check fixed part length

    Instruction(name = "SHFL", instType = InstType.R, opcode = "0110011", funct7 = "0000100", funct3 = "001"),
    Instruction(
      name = "SHFLI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "001",
      fixed = "0000100"
    ), // TODO: check fixed part length
    Instruction(name = "UNSHFL", instType = InstType.R, opcode = "0110011", funct7 = "0000100", funct3 = "101"),
    Instruction(
      name = "UNSHFLI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "101",
      fixed = "0000100"
    ), // TODO: check fixed part length

    Instruction(name = "XPERM.N", instType = InstType.R, opcode = "0110011", funct7 = "0010100", funct3 = "010"),
    Instruction(name = "XPERM.B", instType = InstType.R, opcode = "0110011", funct7 = "0010100", funct3 = "100"),
    Instruction(name = "XPERM.H", instType = InstType.R, opcode = "0110011", funct7 = "0010100", funct3 = "110"),

    // Zbs
    Instruction(name = "BSET", instType = InstType.R, opcode = "0110011", funct7 = "0010100", funct3 = "001"),
    Instruction(
      name = "BSETI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "001",
      fixed = "0010100"
    ), // TODO: check fixed part length
    Instruction(name = "BCLR", instType = InstType.R, opcode = "0110011", funct7 = "0100100", funct3 = "001"),
    Instruction(
      name = "BCLRI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "001",
      fixed = "0100100"
    ), // TODO: check fixed part length
    Instruction(name = "BINV", instType = InstType.R, opcode = "0110011", funct7 = "0110100", funct3 = "001"),
    Instruction(
      name = "BINVI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "001",
      fixed = "0110100"
    ), // TODO: check fixed part length
    Instruction(name = "BEXT", instType = InstType.R, opcode = "0110011", funct7 = "0100100", funct3 = "101"),
    Instruction(
      name = "BEXTI",
      instType = InstType.I,
      opcode = "0010011",
      funct3 = "101",
      fixed = "0100100"
    ), // TODO: check fixed part length

    // Zba
    Instruction(name = "SH1ADD", instType = InstType.R, opcode = "0110011", funct7 = "0010000", funct3 = "010"),
    Instruction(name = "SH2ADD", instType = InstType.R, opcode = "0110011", funct7 = "0010000", funct3 = "100"),
    Instruction(name = "SH3ADD", instType = InstType.R, opcode = "0110011", funct7 = "0010000", funct3 = "110"),

    // Zbe
    // TODO: there seem to be two conflicting encodings for BEXT
    // Instruction(name = "BEXT", instType = InstType.R, opcode = "0110011", funct7 = "0000100", funct3 = "110"),
    Instruction(name = "BDEP", instType = InstType.R, opcode = "0110011", funct7 = "0100100", funct3 = "110"),

    // Zbf
    Instruction(name = "BFP", instType = InstType.R, opcode = "0110011", funct7 = "0100100", funct3 = "111"),

    // Zbc
    Instruction(name = "CLMUL", instType = InstType.R, opcode = "0110011", funct7 = "0000101", funct3 = "001"),
    Instruction(name = "CLMULH", instType = InstType.R, opcode = "0110011", funct7 = "0000101", funct3 = "011"),
    Instruction(name = "CLMULR", instType = InstType.R, opcode = "0110011", funct7 = "0000101", funct3 = "010"),

    // Zbr
    Instruction(name = "CRC32.B", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "10000"),
    Instruction(name = "CRC32C.B", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "11000"),
    Instruction(name = "CRC32.H", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "10001"),
    Instruction(name = "CRC32C.H", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "11001"),
    Instruction(name = "CRC32.W", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "10010"),
    Instruction(name = "CRC32C.W", instType = InstType.R1, opcode = "0010011", funct7 = "0110000", funct3 = "001", rs2 = "11010"),

    // Zbt
    Instruction(name = "CMOV", instType = InstType.R3, opcode = "0110011", funct3 = "101", funct2 = "11"),
    Instruction(name = "CMIX", instType = InstType.R3, opcode = "0110011", funct3 = "001", funct2 = "11"),
    Instruction(name = "FSL", instType = InstType.R3, opcode = "0110011", funct3 = "001", funct2 = "10"),
    Instruction(name = "FSR", instType = InstType.R3, opcode = "0110011", funct3 = "101", funct2 = "10"),
    Instruction(name = "FSRI", instType = InstType.I3, opcode = "0010011", funct3 = "101", funct1 = "1"),

    // Intermediate instructions for LA and LI
    Instruction(name = "LA1", instType = InstType.U, opcode = "0110111"),
    Instruction(name = "LA2", instType = InstType.I, opcode = "0010011", funct3 = "000"),
    Instruction(name = "LI1", instType = InstType.U, opcode = "0110111"),
    Instruction(name = "LI2", instType = InstType.I, opcode = "0010011", funct3 = "000"),

    // RV32P
    Instruction(
      name = "PLI.B",
      instType = InstType.PLI,
      opcode = "0011011",
      funct3 = "010",
      funct7 = "1011010",
      imm_lsb = "0",
      imm_msb = "0"
    ),
    Instruction(name = "PLI.H", instType = InstType.PLI, opcode = "0011011", funct3 = "010", funct7 = "1011000"),
    Instruction(name = "PLUI.H", instType = InstType.PLI, opcode = "0011011", funct3 = "010", funct7 = "1111000"),
    Instruction(name = "PADD.B", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1000010"),
    Instruction(name = "PADD.H", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1000000"),
    Instruction(name = "PADD.BS", instType = InstType.PR, opcode = "0011011", funct3 = "010", funct7 = "1001110"),
    Instruction(name = "PADD.HS", instType = InstType.PR, opcode = "0011011", funct3 = "010", funct7 = "1001100"),
    Instruction(name = "PSUB.B", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1100010"),
    Instruction(name = "PSUB.H", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1100000"),
    Instruction(name = "PSADD.B", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1001010"),
    Instruction(name = "PSADD.H", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1001000"),
    Instruction(name = "PAADD.B", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1001110"),
    Instruction(name = "PAADD.H", instType = InstType.PR, opcode = "0111011", funct3 = "000", funct7 = "1001100"),
    Instruction(
      name = "PLI.DB",
      instType = InstType.PLI,
      opcode = "0011011",
      funct3 = "010",
      funct7 = "0011010",
      imm_lsb = "0",
      imm_msb = "0",
      rd_lsb = "0"
    ),
    Instruction(name = "PLI.DH", instType = InstType.PLI, opcode = "0011011", funct3 = "010", funct7 = "0011000", rd_lsb = "0"),
    Instruction(name = "PLUI.DH", instType = InstType.PLI, opcode = "0011011", funct3 = "010", funct7 = "0111000", rd_lsb = "0"),
    Instruction(
      name = "PADD.DB",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1000010",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PADD.DH",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1000000",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PADD.DW",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1000001",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PADD.DBS",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "0001110",
      rd_lsb = "0",
      rs1_lsb = "0"
    ),
    Instruction(
      name = "PADD.DHS",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "0001100",
      rd_lsb = "0",
      rs1_lsb = "0"
    ),
    Instruction(
      name = "PADD.DWS",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "0001101",
      rd_lsb = "0",
      rs1_lsb = "0"
    ),
    Instruction(
      name = "PSUB.DB",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1100010",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PSUB.DH",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1100000",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PSUB.DW",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1100001",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PSADD.DB",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1001010",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PSADD.DH",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1001000",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PSADD.DW",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1001001",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PAADD.DB",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1001110",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PAADD.DH",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1001100",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PAADD.DW",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "110",
      funct7 = "1001101",
      rd_lsb = "0",
      rs1_lsb = "0",
      rs2_lsb = "0"
    ),
    Instruction(
      name = "PWADD.B",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "010",
      funct7 = "0000010",
      rd_lsb = "1"
    ),
    Instruction(
      name = "PWADD.H",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "010",
      funct7 = "0000000",
      rd_lsb = "1"
    ),
    Instruction(
      name = "PWADDA.B",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "010",
      funct7 = "0000110",
      rd_lsb = "1"
    ),
    Instruction(
      name = "PWADDA.H",
      instType = InstType.PR,
      opcode = "0011011",
      funct3 = "010",
      funct7 = "0000100",
      rd_lsb = "1"
    ),
    Instruction(name = "MVM", instType = InstType.PR, opcode = "0111011", funct3 = "001", funct7 = "1010100"),
    Instruction(name = "MVMN", instType = InstType.PR, opcode = "0111011", funct3 = "001", funct7 = "1010101"),
    Instruction(name = "MERGE", instType = InstType.PR, opcode = "0111011", funct3 = "001", funct7 = "1010110"),
    Instruction(name = "PACK", instType = InstType.PR, opcode = "0110011", funct3 = "100", funct7 = "0000100"),
    Instruction(name = "PSSHA.HS", instType = InstType.PR, opcode = "0011011", funct3 = "010", funct7 = "1110100"),
    Instruction(name = "PSSHAR.HS", instType = InstType.PR, opcode = "0011011", funct3 = "010", funct7 = "1111100"),
    Instruction(name = "PSSHL.HS", instType = InstType.PR, opcode = "0011011", funct3 = "010", funct7 = "1010100"),
    Instruction(name = "PSSHLR.HS", instType = InstType.PR, opcode = "0011011", funct3 = "010", funct7 = "1011100"),
    Instruction(
      name = "PNCLIPI.B",
      instType = InstType.PI,
      opcode = "0011011",
      funct3 = "100",
      funct7 = "0110000",
      imm_msb = "1",
      rs1_lsb = "1"
    ),
    Instruction(
      name = "PNCLIPI.H",
      instType = InstType.PI,
      opcode = "0011011",
      funct3 = "100",
      funct7 = "0110001",
      rs1_lsb = "1"
    ),
    Instruction(
      name = "PNCLIPRI.B",
      instType = InstType.PI,
      opcode = "0011011",
      funct3 = "100",
      funct7 = "0111000",
      imm_msb = "1",
      rs1_lsb = "1"
    ),
    Instruction(
      name = "PNCLIPRI.H",
      instType = InstType.PI,
      opcode = "0011011",
      funct3 = "100",
      funct7 = "0111001",
      rs1_lsb = "1"
    ),
    Instruction(name = "PM2ADD.H", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1000000"),
    Instruction(name = "PM2ADDSU.H", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1110000"),
    Instruction(name = "PM2ADDU.H", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1010000"),
    Instruction(name = "PM2ADD.HX", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1001000"),
    Instruction(name = "PM2SADD.H", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1100010"),
    Instruction(name = "PM2SADD.HX", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1101010"),
    Instruction(name = "PM2ADDA.H", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1000100"),
    Instruction(name = "PM2ADDASU.H", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1110100"),
    Instruction(name = "PM2ADDAU.H", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1010100"),
    Instruction(name = "PM2ADDA.HX", instType = InstType.PR, opcode = "0111011", funct3 = "101", funct7 = "1001100")
  )
}

/** This function transforms a pseudo instruction to it's real conterpart
  */
protected object PseudoInstructions {
  def apply(instructionData: Array[String]): Option[Array[String]] =
    if (instructionData.length < 1) throw new RISCVAssemblerException("Instruction data is empty, cannot parse it.")
    else
      instructionData(0).toUpperCase match {
        // Map received params to the corresponding RISC-V instruction
        case "NOP" => {
          if (instructionData.length != 1) throw new RISCVAssemblerException("NOP should not have extra arguments.")
          else Some(Array("addi", "x0", "x0", "0"))
        }
        case "MV" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("MV should have exactly two arguments.")
          else Some(Array("addi", instructionData(1), instructionData(2), "0"))
        }
        case "NOT" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("NOT should have exactly two arguments.")
          else Some(Array("xori", instructionData(1), instructionData(2), "-1"))
        }
        case "NEG" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("NEG should have exactly two arguments.")
          else Some(Array("sub", instructionData(1), "x0", instructionData(2)))
        }
        case "SEQZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("SEQZ should have exactly two arguments.")
          else Some(Array("sltiu", instructionData(1), instructionData(2), "1"))
        }
        case "SNEZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("SNEZ should have exactly two arguments.")
          else Some(Array("sltu", instructionData(1), "x0", instructionData(2)))
        }
        case "SLTZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("SLTZ should have exactly two arguments.")
          else Some(Array("slt", instructionData(1), instructionData(2), "x0"))
        }
        case "SGTZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("SGTZ should have exactly two arguments.")
          else Some(Array("slt", instructionData(1), "x0", instructionData(2)))
        }
        case "BEQZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("BEQZ should have exactly two arguments.")
          else Some(Array("beq", instructionData(1), "x0", instructionData(2)))
        }
        case "BNEZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("BNEZ should have exactly two arguments.")
          else Some(Array("bne", instructionData(1), "x0", instructionData(2)))
        }
        case "BLEZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("BLEZ should have exactly two arguments.")
          else Some(Array("bge", "x0", instructionData(1), instructionData(2)))
        }
        case "BGEZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("BGEZ should have exactly two arguments.")
          else Some(Array("bge", instructionData(1), "x0", instructionData(2)))
        }
        case "BLTZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("BLTZ should have exactly two arguments.")
          else Some(Array("blt", instructionData(1), "x0", instructionData(2)))
        }
        case "BGTZ" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("BGTZ should have exactly two arguments.")
          else Some(Array("blt", "x0", instructionData(1), instructionData(2)))
        }
        case "BGT" => {
          if (instructionData.length != 4) throw new RISCVAssemblerException("BGT should have exactly three arguments.")
          else Some(Array("blt", instructionData(2), instructionData(1), instructionData(3)))
        }
        case "BLE" => {
          if (instructionData.length != 4) throw new RISCVAssemblerException("BLE should have exactly three arguments.")
          else Some(Array("bge", instructionData(2), instructionData(1), instructionData(3)))
        }
        case "BGTU" => {
          if (instructionData.length != 4) throw new RISCVAssemblerException("BGTU should have exactly three arguments.")
          else Some(Array("bltu", instructionData(2), instructionData(1), instructionData(3)))
        }
        case "BLEU" => {
          if (instructionData.length != 4) throw new RISCVAssemblerException("BLEU should have exactly three arguments.")
          else Some(Array("bgeu", instructionData(2), instructionData(1), instructionData(3)))
        }
        case "J" => {
          if (instructionData.length != 2) throw new RISCVAssemblerException("J should have exactly one argument.")
          else Some(Array("jal", "x0", instructionData(1)))
        }
        case "JR" => {
          if (instructionData.length != 2) throw new RISCVAssemblerException("JR should have exactly one argument.")
          else Some(Array("jalr", "x0", instructionData(1), "0"))
        }
        case "RET" => {
          if (instructionData.length != 1) throw new RISCVAssemblerException("RET should not have extra arguments.")
          else Some(Array("jalr", "x0", "x1", "0"))
        }
        case "CSRR" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("CSRR should have exactly two arguments.")
          else Some(Array("csrrs", instructionData(1), instructionData(2), "x0"))
        }
        case "CSRW" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("CSRW should have exactly two arguments.")
          else Some(Array("csrrw", "x0", instructionData(1), instructionData(2)))
        }
        case "CSRWI" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("CSRWI should have exactly two arguments.")
          else Some(Array("csrrwi", "x0", instructionData(1), instructionData(2)))
        }
        case "CSRS" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("CSRS should have exactly two arguments.")
          else Some(Array("csrrs", "x0", instructionData(1), instructionData(2)))
        }
        case "CSRC" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("CSRC should have exactly two arguments.")
          else Some(Array("csrrc", "x0", instructionData(1), instructionData(2)))
        }
        case "CSRSI" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("CSRSI should have exactly two arguments.")
          else Some(Array("csrrsi", "x0", instructionData(1), instructionData(2)))
        }
        case "CSRCI" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("CSRCI should have exactly two arguments.")
          else Some(Array("csrrci", "x0", instructionData(1), instructionData(2)))
        }
        case "ZEXT.B" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("ZEXT.B should have exactly two arguments.")
          else Some(Array("andi", instructionData(1), instructionData(2), "0xFF"))
        }
        case "ZEXT.H" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("ZEXT.H should have exactly two arguments.")
          else Some(Array("pack", instructionData(1), instructionData(2), "x0"))
        }
        case "REV8" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("REV8 should have exactly two arguments.")
          else Some(Array("grevi", instructionData(1), instructionData(2), "24"))
        }
        case "ORC.B" => {
          if (instructionData.length != 3) throw new RISCVAssemblerException("ORC.B should have exactly two arguments.")
          else Some(Array("gorci", instructionData(1), instructionData(2), "7"))
        }
        case _ => None
      }
}
