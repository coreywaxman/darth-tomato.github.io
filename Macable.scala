///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// File:     Macable.scala                                                                                           //
// Package:  tbdl.types.macableTyping                                                                                //
//                                                                                                                   //
// Description: Type class for implementing various data types supporting Multiply ACcumulate (MAC) operations.      //
//                                                                                                                   //
// Author:   Corey Waxman                                                                                            //
// E-mail:   corey@computer.org                                                                                      //
// Date:     February 12, 2021                                                                                       //
//                                                                                                                   //
// Copyright by the author(s).  Not yet licensed for release.                                                        //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package tbdl.types

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.internal.requireIsHardware
import chisel3.util._

package object macableTyping {
    /** Abstract MAC module, to be implemented within type class implementations.
      * @param inputIO The decoupled input activation signal.
      * @param weightIO The decoupled weight signal.
      * @param outputIO The decoupled output signal.
      * @param clearIO Signal to reset the internal accumulator.
      */
  abstract class MacModule[T <: Data, U <: Data](inputIO:  DecoupledIO[T],
                                                 weightIO: DecoupledIO[U],
                                                 outputIO: DecoupledIO[T],
                                                 clearIO:  Bool) extends Module {
    val io = IO(new Bundle {
      val input  = Decoupled(Input(chiselTypeOf(inputIO.bits))) // chiselTypeOf() creates a new value of the same type
      val weight = Decoupled(Input(chiselTypeOf(weightIO.bits)))  // TODO: This probably won't set the width properly.  Maybe we can call ev.newSignal()?
      val clear  = Input(Bool())
      val output = Flipped(Decoupled(Output(chiselTypeOf(outputIO.bits))))  // TODO: see if chiselTypeOf() is necessary
    })

    io.input <> inputIO
    io.weight <> weightIO
    outputIO <> io.output
    io.clear := clearIO
  }

  /******************************
  **** TYPE CLASS BEGINS HERE ***
  ******************************/
  // Example of Type Class: typelevel.org/blog/2013/07/07/generic-numeric-programming.html

  /** Type class that provides Multiply-Accumulate (MAC) functionality for Chisel data types.
    *
    * To use this type class, create an implicit value that implements this `Macable[T]` trait.  Additional operations are
    * provided automatically by the MacableOps[T] implicit class.
    *
    * If you are working with your own custom datatype, first define your datatype by writing a Chisel Bundle, and use
    * that as your type parameter.  For example:
    * {{{
    *   class MyBundle extends Bundle {
    *     val field1 = UInt(3.W)
    *     val field2 = UInt(5.W)
    *   }
    *   implicit val MyMacableType: Macable[MyBundle] = new Macable[Bundle] {
    *   implicit object MyMacableType extends Macable[MyBundle] {
    *     ...
    *   }
    * }}}
    * 
    * @tparam T The Chisel datatype wrapped by this type class.
    */
  trait Macable[T <: Data] {
    /** Single-cycle addition
      * @param x The first operand
      * @param y The second operand
      * @return The sum
      * @note Can be called as T.+ via the MacableOps implicit class
      */
    def plus(x: T, y: T): T

    /** Single-cycle multiplication
      * @param x The first operand
      * @param y The second operand
      * @return The product
      * @note Can be called as T.* via the MacableOps implicit class
      */
    def times(x: T, y: T): T

    /** Multi-cycle multiplication
      * @param x The first operand
      * @param y The second operand
      * @return The product
      */
    def timesDec(x: DecoupledIO[T], y: DecoupledIO[T]) : DecoupledIO[T]

    /** A comparator
      * @param x The first operand
      * @param y The second operand
      * @return The product
      */
    def isEqual(x: T, y: T): Bool

    /** Takes the negative of a value (equvalent to x*-1, but generally more efficient)
      * @param x The operand to be negated
      * @return The negated result
      */
    def neg(x: T): T

    /** Checks if a value is less than 1 (equvalent to x<0, but generally more efficient)
      * @param x The operand to be checked
      * @return True if and only if the reult is less than 0
      */
    def isNeg(x: T): Bool

    /** A generator for signals of this type (such as T.apply(), if so defined) */
    def newSignal(): T

    /** A generator for signals of this type (such as T.apply(Int), if so defined)
      * @param lit The integer literal
      * @return A new literal encoded as type T
      */
    def newSignal(lit: Float): T

    /** Generator for the MAC, defined within this function as an anonymous module.  Implementations of the `Macable`
     * typeclass may override this method.
      * @param inputIO The decoupled input activation signal.
      * @param weightIO The decoupled weight signal.
      * @param outputIO The decoupled output signal.
      * @param clearIO Signal to reset the internal accumulator.
      * @return An instance of the MAC module
      */
    def generateMac(inputIO:  DecoupledIO[T],
                    weightIO: DecoupledIO[T],
                    outputIO: DecoupledIO[T],
                    clearIO:  Bool): MacModule[T, T] = {
      class DefaultMacModule extends MacModule[T, T](inputIO, weightIO, outputIO, clearIO) {
        val accum = RegInit(newSignal(0))

        val product = times(inputIO.bits, weightIO.bits)
        val load = inputIO.ready && weightIO.ready

        when (clearIO && load) {
          accum := product
        }.elsewhen(clearIO) {
          accum := newSignal(0)
        }.elsewhen(load) {
          accum := plus(accum, product)
        }
      }
      new DefaultMacModule
    }
  }

  /** Generate a Multiply-ACcumulate (MAC) unit.
    * @param input The data to be accumulated.
    * @param weight The value by which to scale the current input value.
    * @param clear Resets the accumulator to starts a new MAC.  May be asserted alongside the new input.
    * @tparam T The Chisel datatype that is a member of the Macable type class
    * @return The result of the MAC operation.
    */
  def mac[T <: Data : Macable](input:  DecoupledIO[T],
                               weight: DecoupledIO[T],
                               clear:  Bool): DecoupledIO[T] = {
    requireIsHardware(input, "MAC activation input")
    requireIsHardware(weight, "MAC weight input")
    requireIsHardware(clear, "MAC accumulator clear signal")

    val ev = implicitly[Macable[T]]

    val output = Decoupled(ev.newSignal())
    val macmod = ev.generateMac(input, weight, output, clear)
    output
  }

  /*****************************
  **** TYPE CLASS ENDS HERE ****
  *****************************/
  
  /** Implicit class that wraps the `Macable[T]` type class, making it more flexible by allowing for infix operations.
    * @param lhs Objects of this type will be implicity converted into a `MacableOps` object
    * @tparam T The Chisel datatype that is a member of the Macable type class
    */
  implicit class MacableOps[T <: Data : Macable](lhs: T) {
    val ev = implicitly[Macable[T]]

    /** Infix addition operator (single cycle).
      * @param rhs The operand to be added.
      * @return The sum of `lhs` and `rhs`.
      * @note For types already defining a `+` operator, the existing operator will take priority. */
    def +(rhs: T): T = ev.plus(lhs, rhs)

    /** Infix multiplication operator (single cycle).
      * @param rhs The operand to be multiplied.
      * @return The product of `lhs` and `rhs`.
      * @note For types already defining a `*` operator, the existing operator will take priority. */
    def *(rhs: T): T = ev.times(lhs, rhs)

    /** Infix equality operator.
      * @param rhs The value to be compared against.
      * @return True if and only if `lhs` is equal to `rhs`.
      * @note For types already defining a `===` operator, the existing operator will take priority. */
    def ===(rhs: T): Bool = ev.isEqual(lhs, rhs)

    /** Infix negation (additive complement) operator.
      * @return The negative value of `lhs`. */
    def inv: T = ev.neg(lhs)

    // Comparators are derived from other operations:

    /** Infix greater-than comparison operator.
      * @param rhs The value to be compared against.
      * @return True if and only if `lhs` is greater than `rhs`.
      * @note For types already defining a `>` operator, the existing operator will take priority. */
    def >(rhs: T): Bool = {
      val diff: T = ev.plus(this.inv, rhs)  // cannot do "inv(rhs)" since implict class not yet defined, but "this.<(rhs)" okay
      ev.isNeg(diff)
    }

    /** Infix less-than-or-equal-to comparison operator.
      * @param rhs The value to be compared against.
      * @return True if and only if `lhs` is less than or equal to `rhs`.
      * @note For types already defining a `<=` operator, the existing operator will take priority. */
    def <=(rhs: T): Bool = !this.>(rhs)

    /** Infix greater-than-or-equal-to comparison operator.
      * @param rhs The value to be compared against.
      * @return True if and only if `lhs` is greater than or equal to `rhs`.
      * @note For types already defining a `>=` operator, the existing operator will take priority. */
    def >=(rhs: T): Bool = this.>(rhs) || ev.isEqual(lhs, rhs)

    /** Infix less-than comparison operator.
      * @param rhs The value to be compared against.
      * @return True if and only if `lhs` is less than `rhs`.
      * @note For types already defining a `<` operator, the existing operator will take priority. */
    def <(rhs: T): Bool = !this.>=(rhs)
  }
}
