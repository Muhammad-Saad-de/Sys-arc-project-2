package project2.utils

import chisel3._
import chisel3.simulator.ChiselSim
import chisel3.reflect.DataMirror
import chisel3.experimental.BundleLiterals
import chisel3.experimental.hierarchy.{
  Definition,
  Instance,
  instantiable,
  public
}
import scala.util.Random
import scala.language.implicitConversions
import chisel3.util.log2Ceil

object ChiselUtils extends ChiselSim {
  private def cat(i1: TestInt, i2: TestInt): TestInt =
    new TestInt(i1.bits ++ i2.bits)

  def cat(ints: TestInt*): TestInt = new TestInt(ints.flatMap(_.bits))

  implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

  implicit def bool2bigint(b: Boolean): BigInt = if (b) 1 else 0

  implicit def testint2bigint(b: TestInt): BigInt = b.value

  implicit def uint2testint(u: UInt): TestInt = {
    TestInt(u.peek().litValue, u.getWidth)
  }

  class TestInt(val bits: Seq[Boolean]) {
    val value = bits.reverse.zipWithIndex
      .foldLeft(BigInt(0)) { case (acc, (b, i)) =>
        acc + ((b: BigInt) << i)
      }

    val width: Int = bits.length

    val signedValue: BigInt = {
      if (value.testBit(width - 1)) {
        value - (BigInt(1) << width)
      } else {
        value
      }
    }

    def apply(i: Int): Boolean = value.testBit(i)

    def apply(msb: Int, lsb: Int): TestInt = {
      new TestInt(bits.reverse.slice(lsb, msb + 1).reverse)
    }

    // Returns an iterator of bits in ascending order from lsb to msb
    // e.g. 0b1011 -> Iterator(1, 1, 0, 1)
    def getAscBitIterator: Iterator[Boolean] = {
      bits.reverse.iterator
    }

    def getDescBitIterator: Iterator[Boolean] = {
      bits.iterator
    }

    def ##(that: TestInt): TestInt = cat(this, that)

    def unary_~ = {
      new TestInt(bits.map(!_))
    }

    def &(that: TestInt): TestInt = {
      new TestInt(
        this.bits
          .zip(that.bits)
          .map({ case (a, b) => a && b })
      )
    }

    def |(that: TestInt): TestInt = {
      new TestInt(
        this.bits
          .zip(that.bits)
          .map({ case (a, b) => a || b })
      )
    }

    def ^(that: TestInt): TestInt = {
      new TestInt(
        this.bits
          .zip(that.bits)
          .map({ case (a, b) => a ^ b })
      )
    }

    def withWidth(newWidth: Int): TestInt = {
      if (newWidth < width) {
        new TestInt(
          bits.reverse.slice(0, newWidth).reverse
        )
      }
      new TestInt(Seq.fill(newWidth - width)(false) ++ bits)
    }

    def zeroExtend(newWidth: Int): TestInt = {
      this.withWidth(newWidth)
    }

    def signExtend(newWidth: Int): TestInt = {
      new TestInt(
        Seq.fill(newWidth - width)(this(width - 1)) ++ bits
      )
    }

    override def equals(num: Any): Boolean = {
      num match {
        case num: TestInt => this.bits == num.bits
        case num: BigInt =>
          if (num < 0) {
            this.signedValue == num
          } else {
            this.unisgnedValue == num
          }
        case _ => false
      }
    }

    val unisgnedValue = value

    def bitSring: String = {
      getDescBitIterator
        .map(_.booleanValue)
        .foldLeft("") { case (acc, b) => acc + (if (b) "1" else "0") }
    }

    def setBit(i: Int, b: Boolean): TestInt = {
      new TestInt(
        bits.updated(i, b)
      )
    }

    def toUInt(): UInt = {
      this.value.U(this.width.W)
    }

  }

  object TestInt {
    private def bitsOfBigInt(value: BigInt): Seq[Boolean] = {
      (if (value < 0) {
         BigInt(2).pow(log2Ceil.apply(value.abs) + 1) - value.abs
       } else {
         value
       })
        .toString(2)
        .map(c => c == '1')
        .toSeq
    }

    def apply(value: BigInt) = {
      new TestInt(bitsOfBigInt(value))
    }

    def apply(value: BigInt, width: Int) = {
      var bits = bitsOfBigInt(value)
      bits = if (bits.length < width) {
        val filler = if (value < 0) {
          bits(0) // sign-extend
        } else {
          false // zero-extend
        }
        Seq.fill(width - bits.length)(filler) ++ bits
      } else {
        bits.reverse.slice(0, width).reverse
      }
      new TestInt(bits)
    }

    def apply(str: String): TestInt = {
      TestInt(BigInt(str, 2), str.length)
    }

    def apply(str: String, radix: Int, width: Int): TestInt = {
      TestInt(BigInt(str, radix), width)
    }

    def getRandom(r: Random, width: Int): TestInt = {
      TestInt(BigInt(width, r), width)
    }
  }

  def setRandomInput(io: Bundle, r: Random): Unit = {
    io.getElements
      .filter(data =>
        DataMirror.specifiedDirectionOf(data) == SpecifiedDirection.Input
      )
      .foreach {
        setRandomInput(_, r)
      }
  }

  def setRandomInput(input: Data, r: Random): Unit = {
    input match {
      case bool: Bool =>
        bool.poke(r.nextBoolean().B)
      case uint: UInt =>
        uint.poke(BigInt(uint.getWidth, r).U)
      case sint: SInt =>
        sint.poke(
          (BigInt(sint.getWidth, r) - (1 << (sint.getWidth - 1))).S
        ) // quick hack
      case _ =>
    }
  }
}
