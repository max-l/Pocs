
package poc

import java.util.Date

trait TARNumeric
trait TAROptionBigDecimal extends TARNumeric
trait TARBigDecimal extends TAROptionBigDecimal with TARNonOption

trait TAROptionDouble extends TAROptionBigDecimal
trait TARDouble extends TAROptionDouble with TARBigDecimal with TARNonOption

trait TAROptionLong extends TAROptionDouble
trait TARLong extends TAROptionLong with TARDouble with TARNonOption

trait TAROptionFloat extends TAROptionDouble
trait TARFloat extends TAROptionFloat with TARDouble with TARNonOption

trait TAROptionInt extends TAROptionLong with TAROptionFloat
trait TARInt extends TAROptionInt with TARLong with TARNonOption with TARFloat

trait TAROptionByte extends TAROptionInt
trait TARByte extends TAROptionByte with TARInt with TARNonOption

trait TAROption extends TAROptionByte with TAROptionInt with TAROptionFloat with TAROptionLong with TAROptionDouble with TAROptionBigDecimal
trait TARNonOption

trait TARNonNumeric

trait TARString extends TARNonNumeric with TAROptionString
trait TARDate extends TARNonNumeric with TAROptionDate

trait TAROptionString extends TARNonNumeric
trait TAROptionDate extends TARNonNumeric


trait CanEqual[-A1,-A2]

object Impls {

  implicit def fi2b(f: Byte) = s1.create(f)
  implicit def fi2b(f: Int) = s3.create(f)
  implicit def fi2b(f: Long) = s4.create(f)
  implicit def fi2b(f: Float) = s5.create(f)
  implicit def fi2b(f: Double) = s2.create(f)
  implicit def fi2b1(f: Option[Byte]) = s6.create(f)
  implicit def fi2b2(f: Option[Int]) = s8.create(f)
  implicit def fi2b3(f: Option[Long]) = s9.create(f)
  implicit def fi2b4(f: Option[Float]) = s10.create(f)
  implicit def fi2b5(f: Option[Double]) = s7.create(f)
  implicit def fi2b6(f: Option[BigDecimal]) = s11.create(f)
  implicit def fi2b7(f: BigDecimal) = s12.create(f)
  
  implicit def snn1c(s: String) = snn1.create(s)
  implicit def snn2c(s: Option[String]) = snn2.create(s)
    
  implicit val snn1 = new TypedExpressionFactory[String,TARString] {
    def create(v: String) = new ConstantTypedExpression[String,TARString](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[String,TARString](v,this)
    def sample = "": String
  }
  
  implicit val snn2 = new TypedExpressionFactory[Option[String],TAROptionString] {
    def create(v: Option[String]) = new ConstantTypedExpression[Option[String],TAROptionString](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[String],TAROptionString](v,this)
    def sample = Option("")
  }

  implicit val snnd1 = new TypedExpressionFactory[Date,TARDate] {
    def create(v: Date) = new ConstantTypedExpression[Date,TARDate](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Date,TARDate](v,this)
    def sample = new Date
  }

  implicit val snnd2 = new TypedExpressionFactory[Option[Date],TAROptionDate] {
    def create(v: Option[Date]) = new ConstantTypedExpression[Option[Date],TAROptionDate](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Date],TAROptionDate](v,this)
    def sample = Option(new Date)
  }
  
  implicit def snn4c(s: Option[Date]) = snnd2.create(s)
  implicit def snn3c(s: Date) = snnd1.create(s)
  
  
  implicit val s1 = new TypedExpressionFactory[Byte,TARByte] {
    def create(v: Byte) = new ConstantTypedExpression[Byte,TARByte](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Byte,TARByte](v,this)
    def sample = 1: Byte
  }

  implicit val s2 = new FloatTypedExpressionFactory[Double,TARDouble] {
    def create(v: Double) = new ConstantTypedExpression[Double,TARDouble](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Double,TARDouble](v,this)
    def sample = 1D
  }

  implicit val s3 = new TypedExpressionFactory[Int,TARInt] {
    def create(v: Int) = new ConstantTypedExpression[Int,TARInt](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Int,TARInt](v,this)
    def sample = 1
  }

  implicit val s4 = new TypedExpressionFactory[Long,TARLong] {
    def create(v: Long) = new ConstantTypedExpression[Long,TARLong](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Long,TARLong](v,this)
    def sample = 1L
  }

  implicit val s5 = new FloatTypedExpressionFactory[Float,TARFloat] {
    def create(v: Float) = new ConstantTypedExpression[Float,TARFloat](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TARFloat](v,this)
    def sample = 1F
  }
  
  
  implicit val s6 = new OptionTypedExpressionFactory[Byte,TARByte,TAROptionByte](s1) {
    def create(v: Option[Byte]) = new ConstantTypedExpression[Option[Byte],TAROptionByte](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Byte],TAROptionByte](v,this)
  }

  implicit val s7 = new FloatOptionTypedExpressionFactory[Double,TARDouble,TAROptionDouble](s2) {
    def create(v: Option[Double]) = new ConstantTypedExpression[Option[Double],TAROptionDouble](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Double],TAROptionDouble](v,this)
  }

  implicit val s8 = new OptionTypedExpressionFactory[Int,TARInt,TAROptionInt](s3) {
    def create(v: Option[Int]) = new ConstantTypedExpression[Option[Int],TAROptionInt](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Int],TAROptionInt](v,this)
  }

  implicit val s9 = new OptionTypedExpressionFactory[Long,TARLong,TAROptionLong](s4) {
    def create(v: Option[Long]) = new ConstantTypedExpression[Option[Long],TAROptionLong](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Long],TAROptionLong](v,this)
  }

  implicit val s10 = new FloatOptionTypedExpressionFactory[Float,TARFloat,TAROptionFloat](s5) {
    def create(v: Option[Float]) = new ConstantTypedExpression[Option[Float],TAROptionFloat](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TAROptionFloat](v,this)
  }

  implicit val s12 = new FloatTypedExpressionFactory[BigDecimal,TARBigDecimal] {
    def create(v: BigDecimal) = new ConstantTypedExpression[BigDecimal,TARBigDecimal](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[BigDecimal,TARBigDecimal](v,this)
    def sample = BigDecimal(1)
  }

  implicit val s11 = new FloatOptionTypedExpressionFactory[BigDecimal,TARBigDecimal,TAROptionBigDecimal](s12) {
    def create(v: Option[BigDecimal]) = new ConstantTypedExpression[Option[BigDecimal],TAROptionBigDecimal](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[BigDecimal],TAROptionBigDecimal](v,this)
  }

  implicit val z1 = new Floatifier[TARByte,Float,TARFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TARFloat](v,null)
  }

  implicit val z2 = new Floatifier[TAROptionByte,Option[Float],TAROptionFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TAROptionFloat](v,null)
  }
  
  implicit val z3 = new Floatifier[TARLong,Double,TARDouble] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Double,TARDouble](v,null)
  }

  implicit val z4 = new Floatifier[TAROptionLong,Option[Double],TAROptionDouble] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Double],TAROptionDouble](v,null)
  }
  
  implicit val z5 = new Floatifier[TARInt,Float,TARFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TARFloat](v,null)
  }  
  
  implicit val z6 = new Floatifier[TAROptionInt,Option[Float],TAROptionFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TAROptionFloat](v,null)
  }  

    
  def max[G >: TAROption, E <: G, B1, B2]
         (b: TypedExpression[B1,E])
         (implicit bs: TypedExpressionFactory[B2,G]) = bs.convert(b)

  def avg[G >: TAROptionFloat, E <: G, B1, B2]
         (b: TypedExpression[B1,E])
         (implicit bs: TypedExpressionFactory[B2,G]) = bs.convert(b)

  def nvl[A1 <: Option[_],
          A2,
          A3,
          E4 <: TARNonOption,
          E1 >: TAROption,
          E3 >: E1,
          E2 <: E3]
         (a: TypedExpression[A1,E1],
          b: TypedExpression[A2,E2])
         (implicit bs: OptionTypedExpressionFactory[A3,E4,E3]): TypedExpression[A3,E4] = bs.related.convert(a)

  implicit val ce1 = new CanEqual[TARNumeric, TARNumeric] {}         
  implicit val ce2 = new CanEqual[TARDate, TARDate] {}
  implicit val ce3 = new CanEqual[TAROptionString, TAROptionString] {}
}

trait TypedExpression[A,F1] {

  def add[F2 >: F1 <: TARNumeric, E <: F2, B1, B2]
         (b: TypedExpression[B1,E])
         (implicit bs: TypedExpressionFactory[B2,F2]): TypedExpression[B2,F2] = bs.convert(b)


  def div[F2 >: F1 <: TARNumeric, E <: F2, B1, B2, Z1, Z2]
         (b: TypedExpression[B1,E])
         (implicit bs: TypedExpressionFactory[B2,F2],
                   tf: Floatifier[F2,Z1,Z2]): TypedExpression[Z1,Z2] = tf.convert(b)

  def s: A
  
  def ===[B,F2](b: TypedExpression[B,F2])(implicit ev: CanEqual[F1, F2]) = 0
}

class ConstantTypedExpression[A,B](val a: A) extends TypedExpression[A,B] {
  def s = a
}


object Tests2 {
  import Impls._

  1 === 1

  "" === ""
    
  Option("") === ""
  "" === Option("")
  
  1 === Option(1)
}


class TypedExpressionConversion[A,B](val b: TypedExpression[_,_], bf: TypedExpressionFactory[A,B]) extends TypedExpression[A,B] {
  def s = bf.sample
}

trait Floatifier[F,G,H] {
  def convert(v: TypedExpression[_,_]): TypedExpressionConversion[G,H]
}

trait FloatTypedExpressionFactory[F,G] extends TypedExpressionFactory[F,G] with Floatifier[G,F,G]

trait TypedExpressionFactory[F,G] {

  def create(f: F) : TypedExpression[F,G]
  def convert(v: TypedExpression[_,_]): TypedExpressionConversion[F,G]
  def sample: F
  def sampleB = create(sample)
}

abstract class OptionTypedExpressionFactory[F,G1,G2](val related: TypedExpressionFactory[F,G1]) extends TypedExpressionFactory[Option[F],G2] {
  def sample = Some(related.sample)
}

abstract class FloatOptionTypedExpressionFactory[F,G1,G2](_related: TypedExpressionFactory[F,G1]) 
extends OptionTypedExpressionFactory[F,G1,G2](_related)
with Floatifier[G2,Option[F],G2]
