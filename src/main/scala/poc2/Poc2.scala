package poc2

object Poc2 {

  trait TARInt
  
  trait Basket[A,B] {
    def iAmABasket = {}
  }

  trait BasketFactory[A,B] {
    def create(v: A): Basket[A,B]
  }
    
  implicit val bf = new BasketFactory[Int,TARInt] {
    def create(v: Int): Basket[Int,TARInt] = sys.error("!")
  }
  
  //implicit def i1(l: Int) = bf.create(l)

  implicit def i2[A,B](a: A)(implicit bf: BasketFactory[A,B]): Basket[A,B] = bf.create(a)

  //1.iAmABasket
  
}