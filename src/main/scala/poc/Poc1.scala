
package poc

trait TARRoot
trait TAROptionBigDecimal extends TARRoot
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

  //implicit def v2b[A,B](a: A)(implicit bf: BasketFactory[A,B]): Basket[A,B] = bf.create(a)

  implicit val s1 = new BasketFactory[Byte,TARByte] {
    def create(v: Byte) = new ConstantBasket[Byte,TARByte](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Byte,TARByte](v,this)
    def sample = 1: Byte
  }

  implicit val s2 = new FloatBasketFactory[Double,TARDouble] {
    def create(v: Double) = new ConstantBasket[Double,TARDouble](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Double,TARDouble](v,this)
    def sample = 1D
  }

  implicit val s3 = new BasketFactory[Int,TARInt] {
    def create(v: Int) = new ConstantBasket[Int,TARInt](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Int,TARInt](v,this)
    def sample = 1
  }

  implicit val s4 = new BasketFactory[Long,TARLong] {
    def create(v: Long) = new ConstantBasket[Long,TARLong](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Long,TARLong](v,this)
    def sample = 1L
  }

  implicit val s5 = new FloatBasketFactory[Float,TARFloat] {
    def create(v: Float) = new ConstantBasket[Float,TARFloat](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Float,TARFloat](v,this)
    def sample = 1F
  }
  
  
  implicit val s6 = new OptionBasketFactory[Byte,TARByte,TAROptionByte](s1) {
    def create(v: Option[Byte]) = new ConstantBasket[Option[Byte],TAROptionByte](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Byte],TAROptionByte](v,this)
  }

  implicit val s7 = new FloatOptionBasketFactory[Double,TARDouble,TAROptionDouble](s2) {
    def create(v: Option[Double]) = new ConstantBasket[Option[Double],TAROptionDouble](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Double],TAROptionDouble](v,this)
  }

  implicit val s8 = new OptionBasketFactory[Int,TARInt,TAROptionInt](s3) {
    def create(v: Option[Int]) = new ConstantBasket[Option[Int],TAROptionInt](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Int],TAROptionInt](v,this)
  }

  implicit val s9 = new OptionBasketFactory[Long,TARLong,TAROptionLong](s4) {
    def create(v: Option[Long]) = new ConstantBasket[Option[Long],TAROptionLong](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Long],TAROptionLong](v,this)
  }

  implicit val s10 = new FloatOptionBasketFactory[Float,TARFloat,TAROptionFloat](s5) {
    def create(v: Option[Float]) = new ConstantBasket[Option[Float],TAROptionFloat](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Float],TAROptionFloat](v,this)
  }

  implicit val s12 = new FloatBasketFactory[BigDecimal,TARBigDecimal] {
    def create(v: BigDecimal) = new ConstantBasket[BigDecimal,TARBigDecimal](v)
    def convert(v: Basket[_,_]) = new BasketConversion[BigDecimal,TARBigDecimal](v,this)
    def sample = BigDecimal(1)
  }

  implicit val s11 = new FloatOptionBasketFactory[BigDecimal,TARBigDecimal,TAROptionBigDecimal](s12) {
    def create(v: Option[BigDecimal]) = new ConstantBasket[Option[BigDecimal],TAROptionBigDecimal](v)
    def convert(v: Basket[_,_]) = new BasketConversion[Option[BigDecimal],TAROptionBigDecimal](v,this)
  }

  implicit val z1 = new Floatifier[TARByte,Float,TARFloat] {
    def convert(v: Basket[_,_]) = new BasketConversion[Float,TARFloat](v,null)
  }

  implicit val z2 = new Floatifier[TAROptionByte,Option[Float],TAROptionFloat] {
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Float],TAROptionFloat](v,null)
  }
  
  implicit val z3 = new Floatifier[TARLong,Double,TARDouble] {
    def convert(v: Basket[_,_]) = new BasketConversion[Double,TARDouble](v,null)
  }

  implicit val z4 = new Floatifier[TAROptionLong,Option[Double],TAROptionDouble] {
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Double],TAROptionDouble](v,null)
  }
  
  implicit val z5 = new Floatifier[TARInt,Float,TARFloat] {
    def convert(v: Basket[_,_]) = new BasketConversion[Float,TARFloat](v,null)
  }  
  
  implicit val z6 = new Floatifier[TAROptionInt,Option[Float],TAROptionFloat] {
    def convert(v: Basket[_,_]) = new BasketConversion[Option[Float],TAROptionFloat](v,null)
  }  

    
  def max[G >: TAROption, E <: G, B1, B2]
         (b: Basket[B1,E])
         (implicit bs: BasketFactory[B2,G]) = bs.convert(b)

  def avg[G >: TAROptionFloat, E <: G, B1, B2]
         (b: Basket[B1,E])
         (implicit bs: BasketFactory[B2,G]) = bs.convert(b)

  def nvl[A1 <: Option[_],
          A2,
          A3,
          E4 <: TARNonOption,
          E1 >: TAROption,
          E3 >: E1,
          E2 <: E3]
         (a: Basket[A1,E1],
          b: Basket[A2,E2])
         (implicit bs: OptionBasketFactory[A3,E4,E3]): Basket[A3,E4] = bs.related.convert(a)

}

import Impls._

trait Basket[A,F1] {

  def add[F2 >: F1 <: TARRoot, E <: F2, B1, B2]
         (b: Basket[B1,E])
         (implicit bs: BasketFactory[B2,F2]): Basket[B2,F2] = bs.convert(b)


  def div[F2 >: F1 <: TARRoot, E <: F2, B1, B2, Z1, Z2]
         (b: Basket[B1,E])
         (implicit bs: BasketFactory[B2,F2],
                   tf: Floatifier[F2,Z1,Z2]): Basket[Z1,Z2] = tf.convert(b)

  def s: A
}

class ConstantBasket[A,B](val a: A) extends Basket[A,B] {
  def s = a
}

class BasketConversion[A,B](val b: Basket[_,_], bf: BasketFactory[A,B]) extends Basket[A,B] {
  def s = bf.sample
}

trait Floatifier[F,G,H] {
  def convert(v: Basket[_,_]): BasketConversion[G,H]
}

trait FloatBasketFactory[F,G] extends BasketFactory[F,G] with Floatifier[G,F,G]

trait BasketFactory[F,G] {

  def create(f: F) : Basket[F,G]
  def convert(v: Basket[_,_]): BasketConversion[F,G]
  def sample: F
  def sampleB = create(sample)
}

abstract class OptionBasketFactory[F,G1,G2](val related: BasketFactory[F,G1]) extends BasketFactory[Option[F],G2] {
  def sample = Some(related.sample)
}

//OptionBasketFactory[BigDecimal,TARBigDecimal,TAROptionBigDecimal](s12)
abstract class FloatOptionBasketFactory[F,G1,G2](_related: BasketFactory[F,G1]) 
extends OptionBasketFactory[F,G1,G2](_related)
with Floatifier[G2,Option[F],G2]


object Main {

  case class NumType(size: Int, isFloat: Boolean, isOption: Boolean, func: String, name: String)
  
  val fNumType = NumType(32, true, false, "aFloat", "Float")
  
  val p0 = Seq(
      NumType(8, false, false, "aByte", "Byte"),
      NumType(32, false, false, "aInt", "Int"),
      NumType(64, false, false, "aLong", "Long"),
      fNumType,
      NumType(64, true, false, "aDouble", "Double"),
      NumType(99, true, false, "aBigDecimal", "BigDecimal")
  )
  
  val p = p0 ++ p0.map(nt => NumType(nt.size, nt.isFloat, true, "a" + nt.name + "O", "Option[" + nt.name + "]"))
  
  
  def absorber(nt1: NumType, nt2: NumType, isOption: Boolean, isFloat: Boolean) = {
    
    val size0 = 
      if (nt1.size > nt2.size) nt1.size
      else nt2.size

    val size = 
      (size0, isFloat, isOption) match {
          case (8,true, true) => 32
          case (8,true, false) => 32
          case _ => size0 
      }
    
    p.find(nt => nt.size == size && nt.isFloat == isFloat && nt.isOption == isOption).
            getOrElse(sys.error(nt1 + " " + nt2 + "," + isOption + ", " + isFloat))    
      
  }
  
  def absorber(nt1: NumType, nt2: NumType): NumType =
    absorber(nt1, nt2, nt1.isOption || nt2.isOption, nt1.isFloat || nt2.isFloat)
  
  val binOps = Seq("div", "add")
  
  def generateTypeLevelTests = {
    
    for(p1 <- p; p2 <- p; bo <- binOps) {
      print("(")
      print(p1.func)
      print(" ")
      print(bo)
      print(" ")
      print(p2.func)
      print(").s : ")
      bo match {
        case "div" => print(absorber(absorber(p1,p2),fNumType).name)
        case "add" => print(absorber(p1,p2).name)
      }
      
      print("\n")
    }
    
    for(p1 <- p; p2 <- p; bo <- binOps) {
      print("avg(")
      print(p1.func)
      print(" ")
      print(bo)
      print(" ")
      print(p2.func)
      print(").s : ")
      print(absorber(p1,p2,true,true).name)      
      
      print("\n")
    }    
    
    for(p1 <- p; p2 <- p; bo <- binOps) {
      print("max(")
      print(p1.func)
      print(" ")
      print(bo)
      print(" ")
      print(p2.func)
      print(").s : ")
      print(absorber(p1,p2,true,p1.isFloat || p2.isFloat || bo == "div").name)      
      
      print("\n")
    }    
  }
  
  def main(args: Array[String]): Unit = {
    generateTypeLevelTests
  }

  def aByte = 1: Byte
  def aInt = 1: Int
  def aLong = 1: Long
  def aFloat = 1: Float
  def aDouble = 1: Double
  def aBigDecimal = BigDecimal(1)
  
  def aByteO = Some(1: Byte)
  def aIntO = Some(1: Int)
  def aLongO = Some(1: Long)
  def aFloatO = Some(1: Float)
  def aDoubleO = Some(1: Double)
  def aBigDecimalO = Some(BigDecimal(1))
  
  def zmain(a: Array[String]) = {

    1L.s
    1.s
    1.0F.s
    1.0D.s

    val bLong = s4.sampleB

    (bLong add 3).s : Long
    (1L add 3).s : Long
    (1L add 2L).s : Long

    (bLong add 3.5F).s : Double
    (1L add 3.5D).s : Double
    (1 add 3).s : Int

    (bLong add Some(3)).s : Option[Long]
    (1L add Some(3.4F)).s : Option[Double]
    (Some(1L) add 2L).s : Option[Long]

    (Some(1L) add 3.5F).s : Option[Double]
    (Some(1L) add 3.5D).s : Option[Double]
    (Some(1) add 3).s : Option[Int]


    (1 div 2L ).s : Double
    ((1:Int) div(2:Int)).s : Float
    (1 div(2 :Byte)).s : Float
    (1 div(2 :Long)).s : Double

    (Some(1L) div(2 :Int)).s : Option[Double]
    (1L div(2 :Int)).s : Double
    (Some(1L) div(Some(1 :Int))).s : Option[Double]

    max(bLong).s : Option[Long]
    max(1).s : Option[Int]
    max(2L).s : Option[Long]
    max(3.5D).s : Option[Double]
    max(3.0F).s : Option[Float]

    (bLong add 3).s : Long
    (1L add 3).s : Long
    (1L add 2L).s : Long

    max(bLong add 3.5F).s : Option[Double]
    max(1L add 3.5D).s : Option[Double]
    max(1 add 3).s : Option[Int]

    max(1 div 2L ).s : Option[Double]
    max((1:Int) div(2:Int)).s : Option[Float]
    max(1 div(2 :Byte)).s : Option[Float]
    max(1 div(2 :Long)).s : Option[Double]

    aByte.div(aFloat).s : Float
    
    max(aByte.div(aFloat)).s : Option[Float]
    
    max((1:Int) div(2:Int)).s : Option[Float]
    max(1 div(2 :Byte)).s : Option[Float]
    max(1 div(2 :Long)).s : Option[Double]

    
    avg(bLong add 3.5F).s : Option[Double]
    avg(1L add 3.5D).s : Option[Double]
    avg(1 add 3).s : Option[Float]

    avg(1 add 2L ).s : Option[Double]
    avg((1:Int) add(2:Int)).s : Option[Float]
    avg(1 add(2 :Byte)).s : Option[Float]
    avg(1 add(2 :Long)).s : Option[Double]

    avg(BigDecimal(1) add(2 :Long)).s : Option[BigDecimal]

    avg(3.0D).s : Option[Double]
    avg(Some(3.2D)).s : Option[Double]
    avg(3L).s : Option[Double]
    avg(Some(3L)).s : Option[Double]
    avg(1).s : Option[Float]
    avg(Some(1)).s : Option[Float]
    avg(1F).s : Option[Float]
    avg(Some(1F)).s : Option[Float]
    avg(1: Byte).s : Option[Float]
    avg(Some(1: Byte)).s : Option[Float]

    avg(BigDecimal(1)).s : Option[BigDecimal]
    avg(Some(BigDecimal(1))).s : Option[BigDecimal]

    max(BigDecimal(1) add(2 :Long)).s : Option[BigDecimal]

    (BigDecimal(1) add(2 :Long)).s: BigDecimal
    (BigDecimal(1) add(2 :Byte)).s: BigDecimal

    nvl(Some(1.0F), 0).s : Float
    nvl(Some(1.0F), 0 : Long).s : Double
  }
  
  
  def genC = {
    (aByte div aByte).s : Float
    (aByte add aByte).s : Byte
    (aByte div aInt).s : Float
    (aByte add aInt).s : Int
    (aByte div aLong).s : Double
    (aByte add aLong).s : Long
    (aByte div aFloat).s : Float
    (aByte add aFloat).s : Float
    (aByte div aDouble).s : Double
    (aByte add aDouble).s : Double
    (aByte div aBigDecimal).s : BigDecimal
    (aByte add aBigDecimal).s : BigDecimal
    (aByte div aByteO).s : Option[Float]
    (aByte add aByteO).s : Option[Byte]
    (aByte div aIntO).s : Option[Float]
    (aByte add aIntO).s : Option[Int]
    (aByte div aLongO).s : Option[Double]
    (aByte add aLongO).s : Option[Long]
    (aByte div aFloatO).s : Option[Float]
    (aByte add aFloatO).s : Option[Float]
    (aByte div aDoubleO).s : Option[Double]
    (aByte add aDoubleO).s : Option[Double]
    (aByte div aBigDecimalO).s : Option[BigDecimal]
    (aByte add aBigDecimalO).s : Option[BigDecimal]
    (aInt div aByte).s : Float
    (aInt add aByte).s : Int
    (aInt div aInt).s : Float
    (aInt add aInt).s : Int
    (aInt div aLong).s : Double
    (aInt add aLong).s : Long
    (aInt div aFloat).s : Float
    (aInt add aFloat).s : Float
    (aInt div aDouble).s : Double
    (aInt add aDouble).s : Double
    (aInt div aBigDecimal).s : BigDecimal
    (aInt add aBigDecimal).s : BigDecimal
    (aInt div aByteO).s : Option[Float]
    (aInt add aByteO).s : Option[Int]
    (aInt div aIntO).s : Option[Float]
    (aInt add aIntO).s : Option[Int]
    (aInt div aLongO).s : Option[Double]
    (aInt add aLongO).s : Option[Long]
    (aInt div aFloatO).s : Option[Float]
    (aInt add aFloatO).s : Option[Float]
    (aInt div aDoubleO).s : Option[Double]
    (aInt add aDoubleO).s : Option[Double]
    (aInt div aBigDecimalO).s : Option[BigDecimal]
    (aInt add aBigDecimalO).s : Option[BigDecimal]
    (aLong div aByte).s : Double
    (aLong add aByte).s : Long
    (aLong div aInt).s : Double
    (aLong add aInt).s : Long
    (aLong div aLong).s : Double
    (aLong add aLong).s : Long
    (aLong div aFloat).s : Double
    (aLong add aFloat).s : Double
    (aLong div aDouble).s : Double
    (aLong add aDouble).s : Double
    (aLong div aBigDecimal).s : BigDecimal
    (aLong add aBigDecimal).s : BigDecimal
    (aLong div aByteO).s : Option[Double]
    (aLong add aByteO).s : Option[Long]
    (aLong div aIntO).s : Option[Double]
    (aLong add aIntO).s : Option[Long]
    (aLong div aLongO).s : Option[Double]
    (aLong add aLongO).s : Option[Long]
    (aLong div aFloatO).s : Option[Double]
    (aLong add aFloatO).s : Option[Double]
    (aLong div aDoubleO).s : Option[Double]
    (aLong add aDoubleO).s : Option[Double]
    (aLong div aBigDecimalO).s : Option[BigDecimal]
    (aLong add aBigDecimalO).s : Option[BigDecimal]
    (aFloat div aByte).s : Float
    (aFloat add aByte).s : Float
    (aFloat div aInt).s : Float
    (aFloat add aInt).s : Float
    (aFloat div aLong).s : Double
    (aFloat add aLong).s : Double
    (aFloat div aFloat).s : Float
    (aFloat add aFloat).s : Float
    (aFloat div aDouble).s : Double
    (aFloat add aDouble).s : Double
    (aFloat div aBigDecimal).s : BigDecimal
    (aFloat add aBigDecimal).s : BigDecimal
    (aFloat div aByteO).s : Option[Float]
    (aFloat add aByteO).s : Option[Float]
    (aFloat div aIntO).s : Option[Float]
    (aFloat add aIntO).s : Option[Float]
    (aFloat div aLongO).s : Option[Double]
    (aFloat add aLongO).s : Option[Double]
    (aFloat div aFloatO).s : Option[Float]
    (aFloat add aFloatO).s : Option[Float]
    (aFloat div aDoubleO).s : Option[Double]
    (aFloat add aDoubleO).s : Option[Double]
    (aFloat div aBigDecimalO).s : Option[BigDecimal]
    (aFloat add aBigDecimalO).s : Option[BigDecimal]
    (aDouble div aByte).s : Double
    (aDouble add aByte).s : Double
    (aDouble div aInt).s : Double
    (aDouble add aInt).s : Double
    (aDouble div aLong).s : Double
    (aDouble add aLong).s : Double
    (aDouble div aFloat).s : Double
    (aDouble add aFloat).s : Double
    (aDouble div aDouble).s : Double
    (aDouble add aDouble).s : Double
    (aDouble div aBigDecimal).s : BigDecimal
    (aDouble add aBigDecimal).s : BigDecimal
    (aDouble div aByteO).s : Option[Double]
    (aDouble add aByteO).s : Option[Double]
    (aDouble div aIntO).s : Option[Double]
    (aDouble add aIntO).s : Option[Double]
    (aDouble div aLongO).s : Option[Double]
    (aDouble add aLongO).s : Option[Double]
    (aDouble div aFloatO).s : Option[Double]
    (aDouble add aFloatO).s : Option[Double]
    (aDouble div aDoubleO).s : Option[Double]
    (aDouble add aDoubleO).s : Option[Double]
    (aDouble div aBigDecimalO).s : Option[BigDecimal]
    (aDouble add aBigDecimalO).s : Option[BigDecimal]
    (aBigDecimal div aByte).s : BigDecimal
    (aBigDecimal add aByte).s : BigDecimal
    (aBigDecimal div aInt).s : BigDecimal
    (aBigDecimal add aInt).s : BigDecimal
    (aBigDecimal div aLong).s : BigDecimal
    (aBigDecimal add aLong).s : BigDecimal
    (aBigDecimal div aFloat).s : BigDecimal
    (aBigDecimal add aFloat).s : BigDecimal
    (aBigDecimal div aDouble).s : BigDecimal
    (aBigDecimal add aDouble).s : BigDecimal
    (aBigDecimal div aBigDecimal).s : BigDecimal
    (aBigDecimal add aBigDecimal).s : BigDecimal
    (aBigDecimal div aByteO).s : Option[BigDecimal]
    (aBigDecimal add aByteO).s : Option[BigDecimal]
    (aBigDecimal div aIntO).s : Option[BigDecimal]
    (aBigDecimal add aIntO).s : Option[BigDecimal]
    (aBigDecimal div aLongO).s : Option[BigDecimal]
    (aBigDecimal add aLongO).s : Option[BigDecimal]
    (aBigDecimal div aFloatO).s : Option[BigDecimal]
    (aBigDecimal add aFloatO).s : Option[BigDecimal]
    (aBigDecimal div aDoubleO).s : Option[BigDecimal]
    (aBigDecimal add aDoubleO).s : Option[BigDecimal]
    (aBigDecimal div aBigDecimalO).s : Option[BigDecimal]
    (aBigDecimal add aBigDecimalO).s : Option[BigDecimal]
    (aByteO div aByte).s : Option[Float]
    (aByteO add aByte).s : Option[Byte]
    (aByteO div aInt).s : Option[Float]
    (aByteO add aInt).s : Option[Int]
    (aByteO div aLong).s : Option[Double]
    (aByteO add aLong).s : Option[Long]
    (aByteO div aFloat).s : Option[Float]
    (aByteO add aFloat).s : Option[Float]
    (aByteO div aDouble).s : Option[Double]
    (aByteO add aDouble).s : Option[Double]
    (aByteO div aBigDecimal).s : Option[BigDecimal]
    (aByteO add aBigDecimal).s : Option[BigDecimal]
    (aByteO div aByteO).s : Option[Float]
    (aByteO add aByteO).s : Option[Byte]
    (aByteO div aIntO).s : Option[Float]
    (aByteO add aIntO).s : Option[Int]
    (aByteO div aLongO).s : Option[Double]
    (aByteO add aLongO).s : Option[Long]
    (aByteO div aFloatO).s : Option[Float]
    (aByteO add aFloatO).s : Option[Float]
    (aByteO div aDoubleO).s : Option[Double]
    (aByteO add aDoubleO).s : Option[Double]
    (aByteO div aBigDecimalO).s : Option[BigDecimal]
    (aByteO add aBigDecimalO).s : Option[BigDecimal]
    (aIntO div aByte).s : Option[Float]
    (aIntO add aByte).s : Option[Int]
    (aIntO div aInt).s : Option[Float]
    (aIntO add aInt).s : Option[Int]
    (aIntO div aLong).s : Option[Double]
    (aIntO add aLong).s : Option[Long]
    (aIntO div aFloat).s : Option[Float]
    (aIntO add aFloat).s : Option[Float]
    (aIntO div aDouble).s : Option[Double]
    (aIntO add aDouble).s : Option[Double]
    (aIntO div aBigDecimal).s : Option[BigDecimal]
    (aIntO add aBigDecimal).s : Option[BigDecimal]
    (aIntO div aByteO).s : Option[Float]
    (aIntO add aByteO).s : Option[Int]
    (aIntO div aIntO).s : Option[Float]
    (aIntO add aIntO).s : Option[Int]
    (aIntO div aLongO).s : Option[Double]
    (aIntO add aLongO).s : Option[Long]
    (aIntO div aFloatO).s : Option[Float]
    (aIntO add aFloatO).s : Option[Float]
    (aIntO div aDoubleO).s : Option[Double]
    (aIntO add aDoubleO).s : Option[Double]
    (aIntO div aBigDecimalO).s : Option[BigDecimal]
    (aIntO add aBigDecimalO).s : Option[BigDecimal]
    (aLongO div aByte).s : Option[Double]
    (aLongO add aByte).s : Option[Long]
    (aLongO div aInt).s : Option[Double]
    (aLongO add aInt).s : Option[Long]
    (aLongO div aLong).s : Option[Double]
    (aLongO add aLong).s : Option[Long]
    (aLongO div aFloat).s : Option[Double]
    (aLongO add aFloat).s : Option[Double]
    (aLongO div aDouble).s : Option[Double]
    (aLongO add aDouble).s : Option[Double]
    (aLongO div aBigDecimal).s : Option[BigDecimal]
    (aLongO add aBigDecimal).s : Option[BigDecimal]
    (aLongO div aByteO).s : Option[Double]
    (aLongO add aByteO).s : Option[Long]
    (aLongO div aIntO).s : Option[Double]
    (aLongO add aIntO).s : Option[Long]
    (aLongO div aLongO).s : Option[Double]
    (aLongO add aLongO).s : Option[Long]
    (aLongO div aFloatO).s : Option[Double]
    (aLongO add aFloatO).s : Option[Double]
    (aLongO div aDoubleO).s : Option[Double]
    (aLongO add aDoubleO).s : Option[Double]
    (aLongO div aBigDecimalO).s : Option[BigDecimal]
    (aLongO add aBigDecimalO).s : Option[BigDecimal]
    (aFloatO div aByte).s : Option[Float]
    (aFloatO add aByte).s : Option[Float]
    (aFloatO div aInt).s : Option[Float]
    (aFloatO add aInt).s : Option[Float]
    (aFloatO div aLong).s : Option[Double]
    (aFloatO add aLong).s : Option[Double]
    (aFloatO div aFloat).s : Option[Float]
    (aFloatO add aFloat).s : Option[Float]
    (aFloatO div aDouble).s : Option[Double]
    (aFloatO add aDouble).s : Option[Double]
    (aFloatO div aBigDecimal).s : Option[BigDecimal]
    (aFloatO add aBigDecimal).s : Option[BigDecimal]
    (aFloatO div aByteO).s : Option[Float]
    (aFloatO add aByteO).s : Option[Float]
    (aFloatO div aIntO).s : Option[Float]
    (aFloatO add aIntO).s : Option[Float]
    (aFloatO div aLongO).s : Option[Double]
    (aFloatO add aLongO).s : Option[Double]
    (aFloatO div aFloatO).s : Option[Float]
    (aFloatO add aFloatO).s : Option[Float]
    (aFloatO div aDoubleO).s : Option[Double]
    (aFloatO add aDoubleO).s : Option[Double]
    (aFloatO div aBigDecimalO).s : Option[BigDecimal]
    (aFloatO add aBigDecimalO).s : Option[BigDecimal]
    (aDoubleO div aByte).s : Option[Double]
    (aDoubleO add aByte).s : Option[Double]
    (aDoubleO div aInt).s : Option[Double]
    (aDoubleO add aInt).s : Option[Double]
    (aDoubleO div aLong).s : Option[Double]
    (aDoubleO add aLong).s : Option[Double]
    (aDoubleO div aFloat).s : Option[Double]
    (aDoubleO add aFloat).s : Option[Double]
    (aDoubleO div aDouble).s : Option[Double]
    (aDoubleO add aDouble).s : Option[Double]
    (aDoubleO div aBigDecimal).s : Option[BigDecimal]
    (aDoubleO add aBigDecimal).s : Option[BigDecimal]
    (aDoubleO div aByteO).s : Option[Double]
    (aDoubleO add aByteO).s : Option[Double]
    (aDoubleO div aIntO).s : Option[Double]
    (aDoubleO add aIntO).s : Option[Double]
    (aDoubleO div aLongO).s : Option[Double]
    (aDoubleO add aLongO).s : Option[Double]
    (aDoubleO div aFloatO).s : Option[Double]
    (aDoubleO add aFloatO).s : Option[Double]
    (aDoubleO div aDoubleO).s : Option[Double]
    (aDoubleO add aDoubleO).s : Option[Double]
    (aDoubleO div aBigDecimalO).s : Option[BigDecimal]
    (aDoubleO add aBigDecimalO).s : Option[BigDecimal]
    (aBigDecimalO div aByte).s : Option[BigDecimal]
    (aBigDecimalO add aByte).s : Option[BigDecimal]
    (aBigDecimalO div aInt).s : Option[BigDecimal]
    (aBigDecimalO add aInt).s : Option[BigDecimal]
    (aBigDecimalO div aLong).s : Option[BigDecimal]
    (aBigDecimalO add aLong).s : Option[BigDecimal]
    (aBigDecimalO div aFloat).s : Option[BigDecimal]
    (aBigDecimalO add aFloat).s : Option[BigDecimal]
    (aBigDecimalO div aDouble).s : Option[BigDecimal]
    (aBigDecimalO add aDouble).s : Option[BigDecimal]
    (aBigDecimalO div aBigDecimal).s : Option[BigDecimal]
    (aBigDecimalO add aBigDecimal).s : Option[BigDecimal]
    (aBigDecimalO div aByteO).s : Option[BigDecimal]
    (aBigDecimalO add aByteO).s : Option[BigDecimal]
    (aBigDecimalO div aIntO).s : Option[BigDecimal]
    (aBigDecimalO add aIntO).s : Option[BigDecimal]
    (aBigDecimalO div aLongO).s : Option[BigDecimal]
    (aBigDecimalO add aLongO).s : Option[BigDecimal]
    (aBigDecimalO div aFloatO).s : Option[BigDecimal]
    (aBigDecimalO add aFloatO).s : Option[BigDecimal]
    (aBigDecimalO div aDoubleO).s : Option[BigDecimal]
    (aBigDecimalO add aDoubleO).s : Option[BigDecimal]
    (aBigDecimalO div aBigDecimalO).s : Option[BigDecimal]
    (aBigDecimalO add aBigDecimalO).s : Option[BigDecimal]
    avg(aByte div aByte).s : Option[Float]
    avg(aByte add aByte).s : Option[Float]
    avg(aByte div aInt).s : Option[Float]
    avg(aByte add aInt).s : Option[Float]
    avg(aByte div aLong).s : Option[Double]
    avg(aByte add aLong).s : Option[Double]
    avg(aByte div aFloat).s : Option[Float]
    avg(aByte add aFloat).s : Option[Float]
    avg(aByte div aDouble).s : Option[Double]
    avg(aByte add aDouble).s : Option[Double]
    avg(aByte div aBigDecimal).s : Option[BigDecimal]
    avg(aByte add aBigDecimal).s : Option[BigDecimal]
    avg(aByte div aByteO).s : Option[Float]
    avg(aByte add aByteO).s : Option[Float]
    avg(aByte div aIntO).s : Option[Float]
    avg(aByte add aIntO).s : Option[Float]
    avg(aByte div aLongO).s : Option[Double]
    avg(aByte add aLongO).s : Option[Double]
    avg(aByte div aFloatO).s : Option[Float]
    avg(aByte add aFloatO).s : Option[Float]
    avg(aByte div aDoubleO).s : Option[Double]
    avg(aByte add aDoubleO).s : Option[Double]
    avg(aByte div aBigDecimalO).s : Option[BigDecimal]
    avg(aByte add aBigDecimalO).s : Option[BigDecimal]
    avg(aInt div aByte).s : Option[Float]
    avg(aInt add aByte).s : Option[Float]
    avg(aInt div aInt).s : Option[Float]
    avg(aInt add aInt).s : Option[Float]
    avg(aInt div aLong).s : Option[Double]
    avg(aInt add aLong).s : Option[Double]
    avg(aInt div aFloat).s : Option[Float]
    avg(aInt add aFloat).s : Option[Float]
    avg(aInt div aDouble).s : Option[Double]
    avg(aInt add aDouble).s : Option[Double]
    avg(aInt div aBigDecimal).s : Option[BigDecimal]
    avg(aInt add aBigDecimal).s : Option[BigDecimal]
    avg(aInt div aByteO).s : Option[Float]
    avg(aInt add aByteO).s : Option[Float]
    avg(aInt div aIntO).s : Option[Float]
    avg(aInt add aIntO).s : Option[Float]
    avg(aInt div aLongO).s : Option[Double]
    avg(aInt add aLongO).s : Option[Double]
    avg(aInt div aFloatO).s : Option[Float]
    avg(aInt add aFloatO).s : Option[Float]
    avg(aInt div aDoubleO).s : Option[Double]
    avg(aInt add aDoubleO).s : Option[Double]
    avg(aInt div aBigDecimalO).s : Option[BigDecimal]
    avg(aInt add aBigDecimalO).s : Option[BigDecimal]
    avg(aLong div aByte).s : Option[Double]
    avg(aLong add aByte).s : Option[Double]
    avg(aLong div aInt).s : Option[Double]
    avg(aLong add aInt).s : Option[Double]
    avg(aLong div aLong).s : Option[Double]
    avg(aLong add aLong).s : Option[Double]
    avg(aLong div aFloat).s : Option[Double]
    avg(aLong add aFloat).s : Option[Double]
    avg(aLong div aDouble).s : Option[Double]
    avg(aLong add aDouble).s : Option[Double]
    avg(aLong div aBigDecimal).s : Option[BigDecimal]
    avg(aLong add aBigDecimal).s : Option[BigDecimal]
    avg(aLong div aByteO).s : Option[Double]
    avg(aLong add aByteO).s : Option[Double]
    avg(aLong div aIntO).s : Option[Double]
    avg(aLong add aIntO).s : Option[Double]
    avg(aLong div aLongO).s : Option[Double]
    avg(aLong add aLongO).s : Option[Double]
    avg(aLong div aFloatO).s : Option[Double]
    avg(aLong add aFloatO).s : Option[Double]
    avg(aLong div aDoubleO).s : Option[Double]
    avg(aLong add aDoubleO).s : Option[Double]
    avg(aLong div aBigDecimalO).s : Option[BigDecimal]
    avg(aLong add aBigDecimalO).s : Option[BigDecimal]
    avg(aFloat div aByte).s : Option[Float]
    avg(aFloat add aByte).s : Option[Float]
    avg(aFloat div aInt).s : Option[Float]
    avg(aFloat add aInt).s : Option[Float]
    avg(aFloat div aLong).s : Option[Double]
    avg(aFloat add aLong).s : Option[Double]
    avg(aFloat div aFloat).s : Option[Float]
    avg(aFloat add aFloat).s : Option[Float]
    avg(aFloat div aDouble).s : Option[Double]
    avg(aFloat add aDouble).s : Option[Double]
    avg(aFloat div aBigDecimal).s : Option[BigDecimal]
    avg(aFloat add aBigDecimal).s : Option[BigDecimal]
    avg(aFloat div aByteO).s : Option[Float]
    avg(aFloat add aByteO).s : Option[Float]
    avg(aFloat div aIntO).s : Option[Float]
    avg(aFloat add aIntO).s : Option[Float]
    avg(aFloat div aLongO).s : Option[Double]
    avg(aFloat add aLongO).s : Option[Double]
    avg(aFloat div aFloatO).s : Option[Float]
    avg(aFloat add aFloatO).s : Option[Float]
    avg(aFloat div aDoubleO).s : Option[Double]
    avg(aFloat add aDoubleO).s : Option[Double]
    avg(aFloat div aBigDecimalO).s : Option[BigDecimal]
    avg(aFloat add aBigDecimalO).s : Option[BigDecimal]
    avg(aDouble div aByte).s : Option[Double]
    avg(aDouble add aByte).s : Option[Double]
    avg(aDouble div aInt).s : Option[Double]
    avg(aDouble add aInt).s : Option[Double]
    avg(aDouble div aLong).s : Option[Double]
    avg(aDouble add aLong).s : Option[Double]
    avg(aDouble div aFloat).s : Option[Double]
    avg(aDouble add aFloat).s : Option[Double]
    avg(aDouble div aDouble).s : Option[Double]
    avg(aDouble add aDouble).s : Option[Double]
    avg(aDouble div aBigDecimal).s : Option[BigDecimal]
    avg(aDouble add aBigDecimal).s : Option[BigDecimal]
    avg(aDouble div aByteO).s : Option[Double]
    avg(aDouble add aByteO).s : Option[Double]
    avg(aDouble div aIntO).s : Option[Double]
    avg(aDouble add aIntO).s : Option[Double]
    avg(aDouble div aLongO).s : Option[Double]
    avg(aDouble add aLongO).s : Option[Double]
    avg(aDouble div aFloatO).s : Option[Double]
    avg(aDouble add aFloatO).s : Option[Double]
    avg(aDouble div aDoubleO).s : Option[Double]
    avg(aDouble add aDoubleO).s : Option[Double]
    avg(aDouble div aBigDecimalO).s : Option[BigDecimal]
    avg(aDouble add aBigDecimalO).s : Option[BigDecimal]
    avg(aBigDecimal div aByte).s : Option[BigDecimal]
    avg(aBigDecimal add aByte).s : Option[BigDecimal]
    avg(aBigDecimal div aInt).s : Option[BigDecimal]
    avg(aBigDecimal add aInt).s : Option[BigDecimal]
    avg(aBigDecimal div aLong).s : Option[BigDecimal]
    avg(aBigDecimal add aLong).s : Option[BigDecimal]
    avg(aBigDecimal div aFloat).s : Option[BigDecimal]
    avg(aBigDecimal add aFloat).s : Option[BigDecimal]
    avg(aBigDecimal div aDouble).s : Option[BigDecimal]
    avg(aBigDecimal add aDouble).s : Option[BigDecimal]
    avg(aBigDecimal div aBigDecimal).s : Option[BigDecimal]
    avg(aBigDecimal add aBigDecimal).s : Option[BigDecimal]
    avg(aBigDecimal div aByteO).s : Option[BigDecimal]
    avg(aBigDecimal add aByteO).s : Option[BigDecimal]
    avg(aBigDecimal div aIntO).s : Option[BigDecimal]
    avg(aBigDecimal add aIntO).s : Option[BigDecimal]
    avg(aBigDecimal div aLongO).s : Option[BigDecimal]
    avg(aBigDecimal add aLongO).s : Option[BigDecimal]
    avg(aBigDecimal div aFloatO).s : Option[BigDecimal]
    avg(aBigDecimal add aFloatO).s : Option[BigDecimal]
    avg(aBigDecimal div aDoubleO).s : Option[BigDecimal]
    avg(aBigDecimal add aDoubleO).s : Option[BigDecimal]
    avg(aBigDecimal div aBigDecimalO).s : Option[BigDecimal]
    avg(aBigDecimal add aBigDecimalO).s : Option[BigDecimal]
    avg(aByteO div aByte).s : Option[Float]
    avg(aByteO add aByte).s : Option[Float]
    avg(aByteO div aInt).s : Option[Float]
    avg(aByteO add aInt).s : Option[Float]
    avg(aByteO div aLong).s : Option[Double]
    avg(aByteO add aLong).s : Option[Double]
    avg(aByteO div aFloat).s : Option[Float]
    avg(aByteO add aFloat).s : Option[Float]
    avg(aByteO div aDouble).s : Option[Double]
    avg(aByteO add aDouble).s : Option[Double]
    avg(aByteO div aBigDecimal).s : Option[BigDecimal]
    avg(aByteO add aBigDecimal).s : Option[BigDecimal]
    avg(aByteO div aByteO).s : Option[Float]
    avg(aByteO add aByteO).s : Option[Float]
    avg(aByteO div aIntO).s : Option[Float]
    avg(aByteO add aIntO).s : Option[Float]
    avg(aByteO div aLongO).s : Option[Double]
    avg(aByteO add aLongO).s : Option[Double]
    avg(aByteO div aFloatO).s : Option[Float]
    avg(aByteO add aFloatO).s : Option[Float]
    avg(aByteO div aDoubleO).s : Option[Double]
    avg(aByteO add aDoubleO).s : Option[Double]
    avg(aByteO div aBigDecimalO).s : Option[BigDecimal]
    avg(aByteO add aBigDecimalO).s : Option[BigDecimal]
    avg(aIntO div aByte).s : Option[Float]
    avg(aIntO add aByte).s : Option[Float]
    avg(aIntO div aInt).s : Option[Float]
    avg(aIntO add aInt).s : Option[Float]
    avg(aIntO div aLong).s : Option[Double]
    avg(aIntO add aLong).s : Option[Double]
    avg(aIntO div aFloat).s : Option[Float]
    avg(aIntO add aFloat).s : Option[Float]
    avg(aIntO div aDouble).s : Option[Double]
    avg(aIntO add aDouble).s : Option[Double]
    avg(aIntO div aBigDecimal).s : Option[BigDecimal]
    avg(aIntO add aBigDecimal).s : Option[BigDecimal]
    avg(aIntO div aByteO).s : Option[Float]
    avg(aIntO add aByteO).s : Option[Float]
    avg(aIntO div aIntO).s : Option[Float]
    avg(aIntO add aIntO).s : Option[Float]
    avg(aIntO div aLongO).s : Option[Double]
    avg(aIntO add aLongO).s : Option[Double]
    avg(aIntO div aFloatO).s : Option[Float]
    avg(aIntO add aFloatO).s : Option[Float]
    avg(aIntO div aDoubleO).s : Option[Double]
    avg(aIntO add aDoubleO).s : Option[Double]
    avg(aIntO div aBigDecimalO).s : Option[BigDecimal]
    avg(aIntO add aBigDecimalO).s : Option[BigDecimal]
    avg(aLongO div aByte).s : Option[Double]
    avg(aLongO add aByte).s : Option[Double]
    avg(aLongO div aInt).s : Option[Double]
    avg(aLongO add aInt).s : Option[Double]
    avg(aLongO div aLong).s : Option[Double]
    avg(aLongO add aLong).s : Option[Double]
    avg(aLongO div aFloat).s : Option[Double]
    avg(aLongO add aFloat).s : Option[Double]
    avg(aLongO div aDouble).s : Option[Double]
    avg(aLongO add aDouble).s : Option[Double]
    avg(aLongO div aBigDecimal).s : Option[BigDecimal]
    avg(aLongO add aBigDecimal).s : Option[BigDecimal]
    avg(aLongO div aByteO).s : Option[Double]
    avg(aLongO add aByteO).s : Option[Double]
    avg(aLongO div aIntO).s : Option[Double]
    avg(aLongO add aIntO).s : Option[Double]
    avg(aLongO div aLongO).s : Option[Double]
    avg(aLongO add aLongO).s : Option[Double]
    avg(aLongO div aFloatO).s : Option[Double]
    avg(aLongO add aFloatO).s : Option[Double]
    avg(aLongO div aDoubleO).s : Option[Double]
    avg(aLongO add aDoubleO).s : Option[Double]
    avg(aLongO div aBigDecimalO).s : Option[BigDecimal]
    avg(aLongO add aBigDecimalO).s : Option[BigDecimal]
    avg(aFloatO div aByte).s : Option[Float]
    avg(aFloatO add aByte).s : Option[Float]
    avg(aFloatO div aInt).s : Option[Float]
    avg(aFloatO add aInt).s : Option[Float]
    avg(aFloatO div aLong).s : Option[Double]
    avg(aFloatO add aLong).s : Option[Double]
    avg(aFloatO div aFloat).s : Option[Float]
    avg(aFloatO add aFloat).s : Option[Float]
    avg(aFloatO div aDouble).s : Option[Double]
    avg(aFloatO add aDouble).s : Option[Double]
    avg(aFloatO div aBigDecimal).s : Option[BigDecimal]
    avg(aFloatO add aBigDecimal).s : Option[BigDecimal]
    avg(aFloatO div aByteO).s : Option[Float]
    avg(aFloatO add aByteO).s : Option[Float]
    avg(aFloatO div aIntO).s : Option[Float]
    avg(aFloatO add aIntO).s : Option[Float]
    avg(aFloatO div aLongO).s : Option[Double]
    avg(aFloatO add aLongO).s : Option[Double]
    avg(aFloatO div aFloatO).s : Option[Float]
    avg(aFloatO add aFloatO).s : Option[Float]
    avg(aFloatO div aDoubleO).s : Option[Double]
    avg(aFloatO add aDoubleO).s : Option[Double]
    avg(aFloatO div aBigDecimalO).s : Option[BigDecimal]
    avg(aFloatO add aBigDecimalO).s : Option[BigDecimal]
    avg(aDoubleO div aByte).s : Option[Double]
    avg(aDoubleO add aByte).s : Option[Double]
    avg(aDoubleO div aInt).s : Option[Double]
    avg(aDoubleO add aInt).s : Option[Double]
    avg(aDoubleO div aLong).s : Option[Double]
    avg(aDoubleO add aLong).s : Option[Double]
    avg(aDoubleO div aFloat).s : Option[Double]
    avg(aDoubleO add aFloat).s : Option[Double]
    avg(aDoubleO div aDouble).s : Option[Double]
    avg(aDoubleO add aDouble).s : Option[Double]
    avg(aDoubleO div aBigDecimal).s : Option[BigDecimal]
    avg(aDoubleO add aBigDecimal).s : Option[BigDecimal]
    avg(aDoubleO div aByteO).s : Option[Double]
    avg(aDoubleO add aByteO).s : Option[Double]
    avg(aDoubleO div aIntO).s : Option[Double]
    avg(aDoubleO add aIntO).s : Option[Double]
    avg(aDoubleO div aLongO).s : Option[Double]
    avg(aDoubleO add aLongO).s : Option[Double]
    avg(aDoubleO div aFloatO).s : Option[Double]
    avg(aDoubleO add aFloatO).s : Option[Double]
    avg(aDoubleO div aDoubleO).s : Option[Double]
    avg(aDoubleO add aDoubleO).s : Option[Double]
    avg(aDoubleO div aBigDecimalO).s : Option[BigDecimal]
    avg(aDoubleO add aBigDecimalO).s : Option[BigDecimal]
    avg(aBigDecimalO div aByte).s : Option[BigDecimal]
    avg(aBigDecimalO add aByte).s : Option[BigDecimal]
    avg(aBigDecimalO div aInt).s : Option[BigDecimal]
    avg(aBigDecimalO add aInt).s : Option[BigDecimal]
    avg(aBigDecimalO div aLong).s : Option[BigDecimal]
    avg(aBigDecimalO add aLong).s : Option[BigDecimal]
    avg(aBigDecimalO div aFloat).s : Option[BigDecimal]
    avg(aBigDecimalO add aFloat).s : Option[BigDecimal]
    avg(aBigDecimalO div aDouble).s : Option[BigDecimal]
    avg(aBigDecimalO add aDouble).s : Option[BigDecimal]
    avg(aBigDecimalO div aBigDecimal).s : Option[BigDecimal]
    avg(aBigDecimalO add aBigDecimal).s : Option[BigDecimal]
    avg(aBigDecimalO div aByteO).s : Option[BigDecimal]
    avg(aBigDecimalO add aByteO).s : Option[BigDecimal]
    avg(aBigDecimalO div aIntO).s : Option[BigDecimal]
    avg(aBigDecimalO add aIntO).s : Option[BigDecimal]
    avg(aBigDecimalO div aLongO).s : Option[BigDecimal]
    avg(aBigDecimalO add aLongO).s : Option[BigDecimal]
    avg(aBigDecimalO div aFloatO).s : Option[BigDecimal]
    avg(aBigDecimalO add aFloatO).s : Option[BigDecimal]
    avg(aBigDecimalO div aDoubleO).s : Option[BigDecimal]
    avg(aBigDecimalO add aDoubleO).s : Option[BigDecimal]
    avg(aBigDecimalO div aBigDecimalO).s : Option[BigDecimal]
    avg(aBigDecimalO add aBigDecimalO).s : Option[BigDecimal]
    max(aByte div aByte).s : Option[Float]
    max(aByte add aByte).s : Option[Byte]
    max(aByte div aInt).s : Option[Float]
    max(aByte add aInt).s : Option[Int]
    max(aByte div aLong).s : Option[Double]
    max(aByte add aLong).s : Option[Long]
    max(aByte div aFloat).s : Option[Float]
    max(aByte add aFloat).s : Option[Float]
    max(aByte div aDouble).s : Option[Double]
    max(aByte add aDouble).s : Option[Double]
    max(aByte div aBigDecimal).s : Option[BigDecimal]
    max(aByte add aBigDecimal).s : Option[BigDecimal]
    max(aByte div aByteO).s : Option[Float]
    max(aByte add aByteO).s : Option[Byte]
    max(aByte div aIntO).s : Option[Float]
    max(aByte add aIntO).s : Option[Int]
    max(aByte div aLongO).s : Option[Double]
    max(aByte add aLongO).s : Option[Long]
    max(aByte div aFloatO).s : Option[Float]
    max(aByte add aFloatO).s : Option[Float]
    max(aByte div aDoubleO).s : Option[Double]
    max(aByte add aDoubleO).s : Option[Double]
    max(aByte div aBigDecimalO).s : Option[BigDecimal]
    max(aByte add aBigDecimalO).s : Option[BigDecimal]
    max(aInt div aByte).s : Option[Float]
    max(aInt add aByte).s : Option[Int]
    max(aInt div aInt).s : Option[Float]
    max(aInt add aInt).s : Option[Int]
    max(aInt div aLong).s : Option[Double]
    max(aInt add aLong).s : Option[Long]
    max(aInt div aFloat).s : Option[Float]
    max(aInt add aFloat).s : Option[Float]
    max(aInt div aDouble).s : Option[Double]
    max(aInt add aDouble).s : Option[Double]
    max(aInt div aBigDecimal).s : Option[BigDecimal]
    max(aInt add aBigDecimal).s : Option[BigDecimal]
    max(aInt div aByteO).s : Option[Float]
    max(aInt add aByteO).s : Option[Int]
    max(aInt div aIntO).s : Option[Float]
    max(aInt add aIntO).s : Option[Int]
    max(aInt div aLongO).s : Option[Double]
    max(aInt add aLongO).s : Option[Long]
    max(aInt div aFloatO).s : Option[Float]
    max(aInt add aFloatO).s : Option[Float]
    max(aInt div aDoubleO).s : Option[Double]
    max(aInt add aDoubleO).s : Option[Double]
    max(aInt div aBigDecimalO).s : Option[BigDecimal]
    max(aInt add aBigDecimalO).s : Option[BigDecimal]
    max(aLong div aByte).s : Option[Double]
    max(aLong add aByte).s : Option[Long]
    max(aLong div aInt).s : Option[Double]
    max(aLong add aInt).s : Option[Long]
    max(aLong div aLong).s : Option[Double]
    max(aLong add aLong).s : Option[Long]
    max(aLong div aFloat).s : Option[Double]
    max(aLong add aFloat).s : Option[Double]
    max(aLong div aDouble).s : Option[Double]
    max(aLong add aDouble).s : Option[Double]
    max(aLong div aBigDecimal).s : Option[BigDecimal]
    max(aLong add aBigDecimal).s : Option[BigDecimal]
    max(aLong div aByteO).s : Option[Double]
    max(aLong add aByteO).s : Option[Long]
    max(aLong div aIntO).s : Option[Double]
    max(aLong add aIntO).s : Option[Long]
    max(aLong div aLongO).s : Option[Double]
    max(aLong add aLongO).s : Option[Long]
    max(aLong div aFloatO).s : Option[Double]
    max(aLong add aFloatO).s : Option[Double]
    max(aLong div aDoubleO).s : Option[Double]
    max(aLong add aDoubleO).s : Option[Double]
    max(aLong div aBigDecimalO).s : Option[BigDecimal]
    max(aLong add aBigDecimalO).s : Option[BigDecimal]
    max(aFloat div aByte).s : Option[Float]
    max(aFloat add aByte).s : Option[Float]
    max(aFloat div aInt).s : Option[Float]
    max(aFloat add aInt).s : Option[Float]
    max(aFloat div aLong).s : Option[Double]
    max(aFloat add aLong).s : Option[Double]
    max(aFloat div aFloat).s : Option[Float]
    max(aFloat add aFloat).s : Option[Float]
    max(aFloat div aDouble).s : Option[Double]
    max(aFloat add aDouble).s : Option[Double]
    max(aFloat div aBigDecimal).s : Option[BigDecimal]
    max(aFloat add aBigDecimal).s : Option[BigDecimal]
    max(aFloat div aByteO).s : Option[Float]
    max(aFloat add aByteO).s : Option[Float]
    max(aFloat div aIntO).s : Option[Float]
    max(aFloat add aIntO).s : Option[Float]
    max(aFloat div aLongO).s : Option[Double]
    max(aFloat add aLongO).s : Option[Double]
    max(aFloat div aFloatO).s : Option[Float]
    max(aFloat add aFloatO).s : Option[Float]
    max(aFloat div aDoubleO).s : Option[Double]
    max(aFloat add aDoubleO).s : Option[Double]
    max(aFloat div aBigDecimalO).s : Option[BigDecimal]
    max(aFloat add aBigDecimalO).s : Option[BigDecimal]
    max(aDouble div aByte).s : Option[Double]
    max(aDouble add aByte).s : Option[Double]
    max(aDouble div aInt).s : Option[Double]
    max(aDouble add aInt).s : Option[Double]
    max(aDouble div aLong).s : Option[Double]
    max(aDouble add aLong).s : Option[Double]
    max(aDouble div aFloat).s : Option[Double]
    max(aDouble add aFloat).s : Option[Double]
    max(aDouble div aDouble).s : Option[Double]
    max(aDouble add aDouble).s : Option[Double]
    max(aDouble div aBigDecimal).s : Option[BigDecimal]
    max(aDouble add aBigDecimal).s : Option[BigDecimal]
    max(aDouble div aByteO).s : Option[Double]
    max(aDouble add aByteO).s : Option[Double]
    max(aDouble div aIntO).s : Option[Double]
    max(aDouble add aIntO).s : Option[Double]
    max(aDouble div aLongO).s : Option[Double]
    max(aDouble add aLongO).s : Option[Double]
    max(aDouble div aFloatO).s : Option[Double]
    max(aDouble add aFloatO).s : Option[Double]
    max(aDouble div aDoubleO).s : Option[Double]
    max(aDouble add aDoubleO).s : Option[Double]
    max(aDouble div aBigDecimalO).s : Option[BigDecimal]
    max(aDouble add aBigDecimalO).s : Option[BigDecimal]
    max(aBigDecimal div aByte).s : Option[BigDecimal]
    max(aBigDecimal add aByte).s : Option[BigDecimal]
    max(aBigDecimal div aInt).s : Option[BigDecimal]
    max(aBigDecimal add aInt).s : Option[BigDecimal]
    max(aBigDecimal div aLong).s : Option[BigDecimal]
    max(aBigDecimal add aLong).s : Option[BigDecimal]
    max(aBigDecimal div aFloat).s : Option[BigDecimal]
    max(aBigDecimal add aFloat).s : Option[BigDecimal]
    max(aBigDecimal div aDouble).s : Option[BigDecimal]
    max(aBigDecimal add aDouble).s : Option[BigDecimal]
    max(aBigDecimal div aBigDecimal).s : Option[BigDecimal]
    max(aBigDecimal add aBigDecimal).s : Option[BigDecimal]
    max(aBigDecimal div aByteO).s : Option[BigDecimal]
    max(aBigDecimal add aByteO).s : Option[BigDecimal]
    max(aBigDecimal div aIntO).s : Option[BigDecimal]
    max(aBigDecimal add aIntO).s : Option[BigDecimal]
    max(aBigDecimal div aLongO).s : Option[BigDecimal]
    max(aBigDecimal add aLongO).s : Option[BigDecimal]
    max(aBigDecimal div aFloatO).s : Option[BigDecimal]
    max(aBigDecimal add aFloatO).s : Option[BigDecimal]
    max(aBigDecimal div aDoubleO).s : Option[BigDecimal]
    max(aBigDecimal add aDoubleO).s : Option[BigDecimal]
    max(aBigDecimal div aBigDecimalO).s : Option[BigDecimal]
    max(aBigDecimal add aBigDecimalO).s : Option[BigDecimal]
    max(aByteO div aByte).s : Option[Float]
    max(aByteO add aByte).s : Option[Byte]
    max(aByteO div aInt).s : Option[Float]
    max(aByteO add aInt).s : Option[Int]
    max(aByteO div aLong).s : Option[Double]
    max(aByteO add aLong).s : Option[Long]
    max(aByteO div aFloat).s : Option[Float]
    max(aByteO add aFloat).s : Option[Float]
    max(aByteO div aDouble).s : Option[Double]
    max(aByteO add aDouble).s : Option[Double]
    max(aByteO div aBigDecimal).s : Option[BigDecimal]
    max(aByteO add aBigDecimal).s : Option[BigDecimal]
    max(aByteO div aByteO).s : Option[Float]
    max(aByteO add aByteO).s : Option[Byte]
    max(aByteO div aIntO).s : Option[Float]
    max(aByteO add aIntO).s : Option[Int]
    max(aByteO div aLongO).s : Option[Double]
    max(aByteO add aLongO).s : Option[Long]
    max(aByteO div aFloatO).s : Option[Float]
    max(aByteO add aFloatO).s : Option[Float]
    max(aByteO div aDoubleO).s : Option[Double]
    max(aByteO add aDoubleO).s : Option[Double]
    max(aByteO div aBigDecimalO).s : Option[BigDecimal]
    max(aByteO add aBigDecimalO).s : Option[BigDecimal]
    max(aIntO div aByte).s : Option[Float]
    max(aIntO add aByte).s : Option[Int]
    max(aIntO div aInt).s : Option[Float]
    max(aIntO add aInt).s : Option[Int]
    max(aIntO div aLong).s : Option[Double]
    max(aIntO add aLong).s : Option[Long]
    max(aIntO div aFloat).s : Option[Float]
    max(aIntO add aFloat).s : Option[Float]
    max(aIntO div aDouble).s : Option[Double]
    max(aIntO add aDouble).s : Option[Double]
    max(aIntO div aBigDecimal).s : Option[BigDecimal]
    max(aIntO add aBigDecimal).s : Option[BigDecimal]
    max(aIntO div aByteO).s : Option[Float]
    max(aIntO add aByteO).s : Option[Int]
    max(aIntO div aIntO).s : Option[Float]
    max(aIntO add aIntO).s : Option[Int]
    max(aIntO div aLongO).s : Option[Double]
    max(aIntO add aLongO).s : Option[Long]
    max(aIntO div aFloatO).s : Option[Float]
    max(aIntO add aFloatO).s : Option[Float]
    max(aIntO div aDoubleO).s : Option[Double]
    max(aIntO add aDoubleO).s : Option[Double]
    max(aIntO div aBigDecimalO).s : Option[BigDecimal]
    max(aIntO add aBigDecimalO).s : Option[BigDecimal]
    max(aLongO div aByte).s : Option[Double]
    max(aLongO add aByte).s : Option[Long]
    max(aLongO div aInt).s : Option[Double]
    max(aLongO add aInt).s : Option[Long]
    max(aLongO div aLong).s : Option[Double]
    max(aLongO add aLong).s : Option[Long]
    max(aLongO div aFloat).s : Option[Double]
    max(aLongO add aFloat).s : Option[Double]
    max(aLongO div aDouble).s : Option[Double]
    max(aLongO add aDouble).s : Option[Double]
    max(aLongO div aBigDecimal).s : Option[BigDecimal]
    max(aLongO add aBigDecimal).s : Option[BigDecimal]
    max(aLongO div aByteO).s : Option[Double]
    max(aLongO add aByteO).s : Option[Long]
    max(aLongO div aIntO).s : Option[Double]
    max(aLongO add aIntO).s : Option[Long]
    max(aLongO div aLongO).s : Option[Double]
    max(aLongO add aLongO).s : Option[Long]
    max(aLongO div aFloatO).s : Option[Double]
    max(aLongO add aFloatO).s : Option[Double]
    max(aLongO div aDoubleO).s : Option[Double]
    max(aLongO add aDoubleO).s : Option[Double]
    max(aLongO div aBigDecimalO).s : Option[BigDecimal]
    max(aLongO add aBigDecimalO).s : Option[BigDecimal]
    max(aFloatO div aByte).s : Option[Float]
    max(aFloatO add aByte).s : Option[Float]
    max(aFloatO div aInt).s : Option[Float]
    max(aFloatO add aInt).s : Option[Float]
    max(aFloatO div aLong).s : Option[Double]
    max(aFloatO add aLong).s : Option[Double]
    max(aFloatO div aFloat).s : Option[Float]
    max(aFloatO add aFloat).s : Option[Float]
    max(aFloatO div aDouble).s : Option[Double]
    max(aFloatO add aDouble).s : Option[Double]
    max(aFloatO div aBigDecimal).s : Option[BigDecimal]
    max(aFloatO add aBigDecimal).s : Option[BigDecimal]
    max(aFloatO div aByteO).s : Option[Float]
    max(aFloatO add aByteO).s : Option[Float]
    max(aFloatO div aIntO).s : Option[Float]
    max(aFloatO add aIntO).s : Option[Float]
    max(aFloatO div aLongO).s : Option[Double]
    max(aFloatO add aLongO).s : Option[Double]
    max(aFloatO div aFloatO).s : Option[Float]
    max(aFloatO add aFloatO).s : Option[Float]
    max(aFloatO div aDoubleO).s : Option[Double]
    max(aFloatO add aDoubleO).s : Option[Double]
    max(aFloatO div aBigDecimalO).s : Option[BigDecimal]
    max(aFloatO add aBigDecimalO).s : Option[BigDecimal]
    max(aDoubleO div aByte).s : Option[Double]
    max(aDoubleO add aByte).s : Option[Double]
    max(aDoubleO div aInt).s : Option[Double]
    max(aDoubleO add aInt).s : Option[Double]
    max(aDoubleO div aLong).s : Option[Double]
    max(aDoubleO add aLong).s : Option[Double]
    max(aDoubleO div aFloat).s : Option[Double]
    max(aDoubleO add aFloat).s : Option[Double]
    max(aDoubleO div aDouble).s : Option[Double]
    max(aDoubleO add aDouble).s : Option[Double]
    max(aDoubleO div aBigDecimal).s : Option[BigDecimal]
    max(aDoubleO add aBigDecimal).s : Option[BigDecimal]
    max(aDoubleO div aByteO).s : Option[Double]
    max(aDoubleO add aByteO).s : Option[Double]
    max(aDoubleO div aIntO).s : Option[Double]
    max(aDoubleO add aIntO).s : Option[Double]
    max(aDoubleO div aLongO).s : Option[Double]
    max(aDoubleO add aLongO).s : Option[Double]
    max(aDoubleO div aFloatO).s : Option[Double]
    max(aDoubleO add aFloatO).s : Option[Double]
    max(aDoubleO div aDoubleO).s : Option[Double]
    max(aDoubleO add aDoubleO).s : Option[Double]
    max(aDoubleO div aBigDecimalO).s : Option[BigDecimal]
    max(aDoubleO add aBigDecimalO).s : Option[BigDecimal]
    max(aBigDecimalO div aByte).s : Option[BigDecimal]
    max(aBigDecimalO add aByte).s : Option[BigDecimal]
    max(aBigDecimalO div aInt).s : Option[BigDecimal]
    max(aBigDecimalO add aInt).s : Option[BigDecimal]
    max(aBigDecimalO div aLong).s : Option[BigDecimal]
    max(aBigDecimalO add aLong).s : Option[BigDecimal]
    max(aBigDecimalO div aFloat).s : Option[BigDecimal]
    max(aBigDecimalO add aFloat).s : Option[BigDecimal]
    max(aBigDecimalO div aDouble).s : Option[BigDecimal]
    max(aBigDecimalO add aDouble).s : Option[BigDecimal]
    max(aBigDecimalO div aBigDecimal).s : Option[BigDecimal]
    max(aBigDecimalO add aBigDecimal).s : Option[BigDecimal]
    max(aBigDecimalO div aByteO).s : Option[BigDecimal]
    max(aBigDecimalO add aByteO).s : Option[BigDecimal]
    max(aBigDecimalO div aIntO).s : Option[BigDecimal]
    max(aBigDecimalO add aIntO).s : Option[BigDecimal]
    max(aBigDecimalO div aLongO).s : Option[BigDecimal]
    max(aBigDecimalO add aLongO).s : Option[BigDecimal]
    max(aBigDecimalO div aFloatO).s : Option[BigDecimal]
    max(aBigDecimalO add aFloatO).s : Option[BigDecimal]
    max(aBigDecimalO div aDoubleO).s : Option[BigDecimal]
    max(aBigDecimalO add aDoubleO).s : Option[BigDecimal]
    max(aBigDecimalO div aBigDecimalO).s : Option[BigDecimal]
    max(aBigDecimalO add aBigDecimalO).s : Option[BigDecimal]    
  }
}
