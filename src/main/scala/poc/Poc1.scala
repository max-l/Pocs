
package poc

import java.util.Date

trait TNumeric
trait TOptionBigDecimal extends TNumeric
trait TBigDecimal extends TOptionBigDecimal with TNonOption

trait TOptionDouble extends TOptionBigDecimal
trait TDouble extends TOptionDouble with TBigDecimal with TNonOption

trait TOptionLong extends TOptionDouble
trait TLong extends TOptionLong with TDouble with TNonOption

trait TOptionFloat extends TOptionDouble
trait TFloat extends TOptionFloat with TDouble with TNonOption

trait TOptionInt extends TOptionLong with TOptionFloat
trait TInt extends TOptionInt with TLong with TNonOption with TFloat

trait TOptionByte extends TOptionInt
trait TByte extends TOptionByte with TInt with TNonOption

trait TOption 
 extends TOptionByte with TOptionInt with TOptionFloat with TOptionLong with TOptionDouble with TOptionBigDecimal
 with TOptionDate with TOptionString

trait TNonOption


trait TString extends TOptionString
trait TDate extends TOptionDate
trait TOptionString 
trait TOptionDate 


trait CanCompare[-A1,-A2]

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
    
  implicit val snn1 = new TypedExpressionFactory[String,TString] {
    def create(v: String) = new ConstantTypedExpression[String,TString](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[String,TString](v,this)
    def sample = "": String
  }
  
  implicit val snn2 = new TypedExpressionFactory[Option[String],TOptionString] {
    def create(v: Option[String]) = new ConstantTypedExpression[Option[String],TOptionString](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[String],TOptionString](v,this)
    def sample = Option("")
  }

  implicit val snnd1 = new TypedExpressionFactory[Date,TDate] {
    def create(v: Date) = new ConstantTypedExpression[Date,TDate](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Date,TDate](v,this)
    def sample = new Date
  }

  implicit val snnd2 = new TypedExpressionFactory[Option[Date],TOptionDate] {
    def create(v: Option[Date]) = new ConstantTypedExpression[Option[Date],TOptionDate](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Date],TOptionDate](v,this)
    def sample = Option(new Date)
  }
  
  implicit def snn4c(s: Option[Date]) = snnd2.create(s)
  implicit def snn3c(s: Date) = snnd1.create(s)
  
  
  implicit val s1 = new TypedExpressionFactory[Byte,TByte] {
    def create(v: Byte) = new ConstantTypedExpression[Byte,TByte](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Byte,TByte](v,this)
    def sample = 1: Byte
  }

  implicit val s2 = new FloatTypedExpressionFactory[Double,TDouble] {
    def create(v: Double) = new ConstantTypedExpression[Double,TDouble](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Double,TDouble](v,this)
    def sample = 1D
  }

  implicit val s3 = new TypedExpressionFactory[Int,TInt] {
    def create(v: Int) = new ConstantTypedExpression[Int,TInt](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Int,TInt](v,this)
    def sample = 1
  }

  implicit val s4 = new TypedExpressionFactory[Long,TLong] {
    def create(v: Long) = new ConstantTypedExpression[Long,TLong](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Long,TLong](v,this)
    def sample = 1L
  }

  implicit val s5 = new FloatTypedExpressionFactory[Float,TFloat] {
    def create(v: Float) = new ConstantTypedExpression[Float,TFloat](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TFloat](v,this)
    def sample = 1F
  }
  
  
  implicit val s6 = new OptionTypedExpressionFactory[Byte,TByte,TOptionByte](s1) {
    def create(v: Option[Byte]) = new ConstantTypedExpression[Option[Byte],TOptionByte](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Byte],TOptionByte](v,this)
  }

  implicit val s7 = new FloatOptionTypedExpressionFactory[Double,TDouble,TOptionDouble](s2) {
    def create(v: Option[Double]) = new ConstantTypedExpression[Option[Double],TOptionDouble](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Double],TOptionDouble](v,this)
  }

  implicit val s8 = new OptionTypedExpressionFactory[Int,TInt,TOptionInt](s3) {
    def create(v: Option[Int]) = new ConstantTypedExpression[Option[Int],TOptionInt](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Int],TOptionInt](v,this)
  }

  implicit val s9 = new OptionTypedExpressionFactory[Long,TLong,TOptionLong](s4) {
    def create(v: Option[Long]) = new ConstantTypedExpression[Option[Long],TOptionLong](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Long],TOptionLong](v,this)
  }

  implicit val s10 = new FloatOptionTypedExpressionFactory[Float,TFloat,TOptionFloat](s5) {
    def create(v: Option[Float]) = new ConstantTypedExpression[Option[Float],TOptionFloat](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TOptionFloat](v,this)
  }

  implicit val s12 = new FloatTypedExpressionFactory[BigDecimal,TBigDecimal] {
    def create(v: BigDecimal) = new ConstantTypedExpression[BigDecimal,TBigDecimal](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[BigDecimal,TBigDecimal](v,this)
    def sample = BigDecimal(1)
  }

  implicit val s11 = new FloatOptionTypedExpressionFactory[BigDecimal,TBigDecimal,TOptionBigDecimal](s12) {
    def create(v: Option[BigDecimal]) = new ConstantTypedExpression[Option[BigDecimal],TOptionBigDecimal](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[BigDecimal],TOptionBigDecimal](v,this)
  }

  implicit val z1 = new Floatifier[TByte,Float,TFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TFloat](v,null)
  }

  implicit val z2 = new Floatifier[TOptionByte,Option[Float],TOptionFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TOptionFloat](v,null)
  }
  
  implicit val z3 = new Floatifier[TLong,Double,TDouble] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Double,TDouble](v,null)
  }

  implicit val z4 = new Floatifier[TOptionLong,Option[Double],TOptionDouble] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Double],TOptionDouble](v,null)
  }
  
  implicit val z5 = new Floatifier[TInt,Float,TFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TFloat](v,null)
  }  
  
  implicit val z6 = new Floatifier[TOptionInt,Option[Float],TOptionFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TOptionFloat](v,null)
  }  

    
  def max[T2 >: TOption, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def avg[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def nvl[T4 <: TNonOption,
          T1 >: TOption,
          T3 >: T1,
          T2 <: T3,
          A1,A2,A3]
         (a: TypedExpression[A1,T1],
          b: TypedExpression[A2,T2])
         (implicit bs: OptionTypedExpressionFactory[A3,T4,T3]): TypedExpression[A3,T4] = bs.related.convert(a)

  implicit val ce1 = new CanCompare[TNumeric, TNumeric] {}         
  implicit val ce2 = new CanCompare[TDate, TDate] {}
  implicit val ce3 = new CanCompare[TOptionString, TOptionString] {}
}

trait TypedExpression[A1,T1] {

  def add[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = f.convert(e)


  def div[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3, A4, T4]
         (e: TypedExpression[A2,T2])
         (implicit f:  TypedExpressionFactory[A3,T3], 
                   tf: Floatifier[T3,A4,T4]): TypedExpression[A4,T4] = tf.convert(e)

  def s: A1
  
  def ===[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = 0
  
  def between[A2,T2,A3,T3](b1: TypedExpression[A2,T2], 
                           b2: TypedExpression[A3,T3])
                          (implicit ev1: CanCompare[T1, T2], 
                                    ev2: CanCompare[T2, T3]) = 0
}

class ConstantTypedExpression[A1,T1](val a: A1) extends TypedExpression[A1,T1] {
  def s = a
}

class TypedExpressionConversion[A1,T1](val b: TypedExpression[_,_], bf: TypedExpressionFactory[A1,T1]) extends TypedExpression[A1,T1] {
  def s = bf.sample
}

trait Floatifier[T1,A2,T2] {
  def convert(v: TypedExpression[_,_]): TypedExpressionConversion[A2,T2]
}

trait IdentityFloatifier[A1,T1] extends Floatifier[T1,A1,T1]

trait FloatTypedExpressionFactory[A1,T1] extends TypedExpressionFactory[A1,T1] with IdentityFloatifier[A1,T1]

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
