
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

abstract class FloatOptionBasketFactory[F,G1,G2](_related: BasketFactory[F,G1]) 
extends OptionBasketFactory[F,G1,G2](_related)
with Floatifier[G2,Option[F],G2]
