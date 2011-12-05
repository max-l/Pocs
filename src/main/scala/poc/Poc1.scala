
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

trait TNumericLowerTypeBound 
  extends TByte with TInt with TFloat with TLong with TDouble with TBigDecimal
 
trait TNonOption

trait TEnumValue[A] //extends TOptionEnumValue[A]
trait TOptionEnumValue[A] extends TEnumValue[A]

trait TString extends TOptionString
trait TDate extends TOptionDate
trait TOptionString 
trait TOptionDate 

@scala.annotation.implicitNotFound("The left side of the comparison (===, <>, between,...) is not compatible with the right side")
trait CanCompare[-A1,-A2]

object Impls {
  
  // =========================== Non Numerical =========================== 
  
  implicit val stringTEF = new TypedExpressionFactory[String,TString] {
    def create(v: String) = new ConstantTypedExpression[String,TString](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[String,TString](v,this)
    def sample = "": String
  }
    
  implicit def stringToTE(s: String) = stringTEF.create(s)
  
  implicit val optionStringTEF = new TypedExpressionFactory[Option[String],TOptionString] {
    def create(v: Option[String]) = new ConstantTypedExpression[Option[String],TOptionString](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[String],TOptionString](v,this)
    def sample = Option("")
  }
  
  implicit def optionStringToTE(s: Option[String]) = optionStringTEF.create(s)  

  implicit val dateTEF = new TypedExpressionFactory[Date,TDate] {
    def create(v: Date) = new ConstantTypedExpression[Date,TDate](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Date,TDate](v,this)
    def sample = new Date
  }
  
  implicit def dateToTE(s: Date) = dateTEF.create(s)  

  implicit val optionDateTEF = new TypedExpressionFactory[Option[Date],TOptionDate] {
    def create(v: Option[Date]) = new ConstantTypedExpression[Option[Date],TOptionDate](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Date],TOptionDate](v,this)
    def sample = Option(new Date)
  }
  
  implicit def optionDateToTE(s: Option[Date]) = optionDateTEF.create(s)  
  
  // =========================== Numerical Integral =========================== 

  implicit val byteTEF = new IntegralTypedExpressionFactory[Byte,TByte,Float,TFloat] {
    def create(v: Byte) = new ConstantTypedExpression[Byte,TByte](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Byte,TByte](v,this)
    def sample = 1: Byte
    def floatifyer = floatTEF
  }
  
  implicit def byteToTE(f: Byte) = byteTEF.create(f)  

  implicit val optionByteTEF = new IntegralTypedExpressionFactory[Option[Byte],TOptionByte, Option[Float], TOptionFloat] with DeOptionizer[Byte, TByte, Option[Byte], TOptionByte]{
    def create(v: Option[Byte]) = new ConstantTypedExpression[Option[Byte],TOptionByte](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Byte],TOptionByte](v,this)
    def deOptionizer = byteTEF
    def sample = Option(0 : Byte)
    def floatifyer = optionFloatTEF
  }
  
  implicit def optionByteToTE(f: Option[Byte]) = optionByteTEF.create(f)  
  
  implicit val intTEF = new IntegralTypedExpressionFactory[Int,TInt,Float,TFloat] {
    def create(v: Int) = new ConstantTypedExpression[Int,TInt](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Int,TInt](v,this)
    def sample = 1
    def floatifyer = floatTEF
  }
  
  implicit def intToTE(f: Int) = intTEF.create(f)  

  implicit val optionIntTEF = new IntegralTypedExpressionFactory[Option[Int],TOptionInt,Option[Float],TOptionFloat] with DeOptionizer[Int,TInt,Option[Int],TOptionInt] {
    def create(v: Option[Int]) = new ConstantTypedExpression[Option[Int],TOptionInt](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Int],TOptionInt](v,this)
    def deOptionizer = intTEF
    def sample = Option(0)
    def floatifyer = optionFloatTEF
  }

  implicit def optionIntToTE(f: Option[Int]) = optionIntTEF.create(f)
  
  implicit val longTEF = new IntegralTypedExpressionFactory[Long,TLong,Double,TDouble] {
    def create(v: Long) = new ConstantTypedExpression[Long,TLong](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Long,TLong](v,this)
    def sample = 1L
    def floatifyer = doubleTEF
  }
  
  implicit def longToTE(f: Long) = longTEF.create(f)

  implicit val optionLongTEF = new IntegralTypedExpressionFactory[Option[Long],TOptionLong,Option[Double],TOptionDouble] with DeOptionizer[Long,TLong,Option[Long],TOptionLong] {
    def create(v: Option[Long]) = new ConstantTypedExpression[Option[Long],TOptionLong](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Long],TOptionLong](v,this)
    def deOptionizer = longTEF
    def sample = Option(0L)
    def floatifyer = optionDoubleTEF
  }
  
  implicit def optionLongToTE(f: Option[Long]) = optionLongTEF.create(f)  
  
  // =========================== Numerical Floating Point =========================== 
  
  implicit val floatTEF = new FloatTypedExpressionFactory[Float,TFloat] {
    def create(v: Float) = new ConstantTypedExpression[Float,TFloat](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TFloat](v,this)
    def sample = 1F
  }
    
  implicit def floatToTE(f: Float) = floatTEF.create(f)
  
  implicit val optionFloatTEF = new FloatTypedExpressionFactory[Option[Float],TOptionFloat] with DeOptionizer[Float,TFloat,Option[Float],TOptionFloat] {
    def create(v: Option[Float]) = new ConstantTypedExpression[Option[Float],TOptionFloat](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TOptionFloat](v,this)
    def deOptionizer = floatTEF
    def sample = Option(0F)
  }

  implicit def optionFloatToTE(f: Option[Float]) = optionFloatTEF.create(f)
  
  implicit val doubleTEF = new FloatTypedExpressionFactory[Double,TDouble] {
    def create(v: Double) = new ConstantTypedExpression[Double,TDouble](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Double,TDouble](v,this)
    def sample = 1D
  }

  implicit def doubleToTE(f: Double) = doubleTEF.create(f)    
  
  implicit val optionDoubleTEF = new FloatTypedExpressionFactory[Option[Double],TOptionDouble] with DeOptionizer[Double,TDouble,Option[Double],TOptionDouble] {
    def create(v: Option[Double]) = new ConstantTypedExpression[Option[Double],TOptionDouble](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Double],TOptionDouble](v,this)
    def deOptionizer = doubleTEF
    def sample = Option(0D)
  }
  
  implicit def optionDoubleToTE(f: Option[Double]) = optionDoubleTEF.create(f)
  
  implicit val bigDecimalTEF = new FloatTypedExpressionFactory[BigDecimal,TBigDecimal] {
    def create(v: BigDecimal) = new ConstantTypedExpression[BigDecimal,TBigDecimal](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[BigDecimal,TBigDecimal](v,this)
    def sample = BigDecimal(1)
  }

  implicit def bigDecimalToTE(f: BigDecimal) = bigDecimalTEF.create(f)
  
  implicit val optionBigDecimalTEF = new FloatTypedExpressionFactory[Option[BigDecimal],TOptionBigDecimal] with DeOptionizer[BigDecimal,TBigDecimal,Option[BigDecimal],TOptionBigDecimal] {
    def create(v: Option[BigDecimal]) = new ConstantTypedExpression[Option[BigDecimal],TOptionBigDecimal](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[BigDecimal],TOptionBigDecimal](v,this)
    def deOptionizer = bigDecimalTEF
    def sample = Option(BigDecimal(0))
  }
    
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]) = optionBigDecimalTEF.create(f)

  def max[T2 >: TOption, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def min[T2 >: TOption, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def avg[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def sum[T2 >: TOption, T1 >: TNumericLowerTypeBound <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def nvl[T4 <: TNonOption,
          T1 >: TOption,
          T3 >: T1,
          T2 <: T3,
          A1,A2,A3]
         (a: TypedExpression[A1,T1],
          b: TypedExpression[A2,T2])
         (implicit d: DeOptionizer[A3,T4,_,T3]): TypedExpression[A3,T4] = d.deOptionizer.convert(a)
  
  implicit val ce1 = new CanCompare[TNumeric, TNumeric] {}         
  implicit val ce2 = new CanCompare[TDate, TDate] {}
  implicit val ce3 = new CanCompare[TOptionString, TOptionString] {}
  
  implicit def enumcValueTEF[A <: Enumeration#Value] = new TypedExpressionFactory[A,TEnumValue[A]] {
    def create(v: A) = new ConstantTypedExpression[A,TEnumValue[A]](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[A,TEnumValue[A]](v,this)
    def sample: A = sys.error("!")
  }
  
  implicit def optionEnumValueTEF[A <: Enumeration#Value] = new TypedExpressionFactory[Option[A],TOptionEnumValue[A]] {
    def create(v: Option[A]) = new ConstantTypedExpression[Option[A],TOptionEnumValue[A]](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[A],TOptionEnumValue[A]](v,this)
    def sample: Option[A] = sys.error("!")
  }

  implicit def enumcValueToTE[A <: Enumeration#Value](e: A) = enumcValueTEF.create(e)
  implicit def optionEnumcValueToTE[A <: Enumeration#Value](e: Option[A]) = optionEnumValueTEF.create(e)
  
  implicit def ce4[A] = new CanCompare[TEnumValue[A],TEnumValue[A]] {}
  
}

trait TypedExpression[A1,T1] {

  def add[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = f.convert(e)

  def div[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3, A4, T4]
         (e: TypedExpression[A2,T2])
         (implicit f:  TypedExpressionFactory[A3,T3], 
                   tf: Floatifier[T3,A4,T4]): TypedExpression[A4,T4] = tf.floatify(e)

  def value: A1
  
  def ===[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = 0
  
  def between[A2,T2,A3,T3](b1: TypedExpression[A2,T2], 
                           b2: TypedExpression[A3,T3])
                          (implicit ev1: CanCompare[T1, T2], 
                                    ev2: CanCompare[T2, T3]) = 0
}

class ConstantTypedExpression[A1,T1](val a: A1) extends TypedExpression[A1,T1] {
  def value = a
}

class TypedExpressionConversion[A1,T1](val b: TypedExpression[_,_], bf: TypedExpressionFactory[A1,T1]) extends TypedExpression[A1,T1] {
  def value = bf.sample
}

trait Floatifier[T1,A2,T2] {
  def floatify(v: TypedExpression[_,_]): TypedExpressionConversion[A2,T2]
}

trait IdentityFloatifier[A1,T1] extends Floatifier[T1,A1,T1]

trait FloatTypedExpressionFactory[A1,T1] extends TypedExpressionFactory[A1,T1] with IdentityFloatifier[A1,T1] {
  def floatify(v: TypedExpression[_,_]): TypedExpressionConversion[A1,T1] = convert(v)
}

trait TypedExpressionFactory[A,T] {

  def create(f: A) : TypedExpression[A,T]
  def convert(v: TypedExpression[_,_]): TypedExpressionConversion[A,T]
  def sample: A
  def sampleB = create(sample)
}

trait IntegralTypedExpressionFactory[A1,T1,A2,T2] 
  extends TypedExpressionFactory[A1,T1] with Floatifier[T1,A2,T2] {
  
  def floatify(v: TypedExpression[_,_]): TypedExpressionConversion[A2,T2] = floatifyer.convert(v)
  def floatifyer: TypedExpressionFactory[A2,T2]
}


trait DeOptionizer[A1,T1,A2 <: Option[A1],T2] {
  self: TypedExpressionFactory[A2,T2] =>
    
  def deOptionizer: TypedExpressionFactory[A1,T1]
}
