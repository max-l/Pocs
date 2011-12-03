package poc

object Poc1Tests {

  import Impls._

  // Generator :

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
  
  def generateSystematicTypeLevelTests = {
    
    for(p1 <- p; p2 <- p; bo <- binOps) {
      print("(")
      print(p1.func)
      print(" ")
      print(bo)
      print(" ")
      print(p2.func)
      print(").value : ")
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
      print(").value : ")
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
      print(").value : ")
      print(absorber(p1,p2,true,p1.isFloat || p2.isFloat || bo == "div").name)      
      
      print("\n")
    }    
  }
  
  def typeLevelTests = {
  
    1 === 1  
    "" === ""      
    Option("") === ""
    "" === Option("")    
    1 === Option(1)
    
    1 between (0, Option(2))    
  }
  
  def main(args: Array[String]): Unit = {
    generateSystematicTypeLevelTests
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
  
  def adHocTypeLevelTests {

    1L.value
    1.value
    1.0F.value
    1.0D.value

    val bLong = s4.sampleB

    (bLong add 3).value : Long
    (1L add 3).value : Long
    (1L add 2L).value : Long

    (bLong add 3.5F).value : Double
    (1L add 3.5D).value : Double
    (1 add 3).value : Int

    (bLong add Some(3)).value : Option[Long]
    (1L add Some(3.4F)).value : Option[Double]
    (Some(1L) add 2L).value : Option[Long]

    (Some(1L) add 3.5F).value : Option[Double]
    (Some(1L) add 3.5D).value : Option[Double]
    (Some(1) add 3).value : Option[Int]


    (1 div 2L ).value : Double
    ((1:Int) div(2:Int)).value : Float
    (1 div(2 :Byte)).value : Float
    (1 div(2 :Long)).value : Double

    (Some(1L) div(2 :Int)).value : Option[Double]
    (1L div(2 :Int)).value : Double
    (Some(1L) div(Some(1 :Int))).value : Option[Double]

    max(bLong).value : Option[Long]
    max(1).value : Option[Int]
    max(2L).value : Option[Long]
    max(3.5D).value : Option[Double]
    max(3.0F).value : Option[Float]

    (bLong add 3).value : Long
    (1L add 3).value : Long
    (1L add 2L).value : Long

    max(bLong add 3.5F).value : Option[Double]
    max(1L add 3.5D).value : Option[Double]
    max(1 add 3).value : Option[Int]

    max(1 div 2L ).value : Option[Double]
    max((1:Int) div(2:Int)).value : Option[Float]
    max(1 div(2 :Byte)).value : Option[Float]
    max(1 div(2 :Long)).value : Option[Double]

    aByte.div(aFloat).value : Float
    
    max(aByte.div(aFloat)).value : Option[Float]
    
    max((1:Int) div(2:Int)).value : Option[Float]
    max(1 div(2 :Byte)).value : Option[Float]
    max(1 div(2 :Long)).value : Option[Double]

    
    avg(bLong add 3.5F).value : Option[Double]
    avg(1L add 3.5D).value : Option[Double]
    avg(1 add 3).value : Option[Float]

    avg(1 add 2L ).value : Option[Double]
    avg((1:Int) add(2:Int)).value : Option[Float]
    avg(1 add(2 :Byte)).value : Option[Float]
    avg(1 add(2 :Long)).value : Option[Double]

    avg(BigDecimal(1) add(2 :Long)).value : Option[BigDecimal]

    avg(3.0D).value : Option[Double]
    avg(Some(3.2D)).value : Option[Double]
    avg(3L).value : Option[Double]
    avg(Some(3L)).value : Option[Double]
    avg(1).value : Option[Float]
    avg(Some(1)).value : Option[Float]
    avg(1F).value : Option[Float]
    avg(Some(1F)).value : Option[Float]
    avg(1: Byte).value : Option[Float]
    avg(Some(1: Byte)).value : Option[Float]

    avg(BigDecimal(1)).value : Option[BigDecimal]
    avg(Some(BigDecimal(1))).value : Option[BigDecimal]

    max(BigDecimal(1) add(2 :Long)).value : Option[BigDecimal]

    (BigDecimal(1) add(2 :Long)).value : BigDecimal
    (BigDecimal(1) add(2 :Byte)).value : BigDecimal

    nvl(Option(1.0F), 0).value : Float
    nvl(Option(1.0F), 0 : Long).value : Double
    
    nvl(Option(1), 1).value : Int
    nvl(Option(1), 0.7F).value  : Float    
    nvl(Option(BigDecimal(1)), 0.7F).value  : BigDecimal
    
    nvl(max(BigDecimal(1)), 0.7F).value  : BigDecimal
    nvl(avg(1: Byte), 7L).value  : Double
  }
  
  
  def systematicTypeLevelTests = {
    
    (aByte div aByte).value : Float
    (aByte add aByte).value : Byte
    (aByte div aInt).value : Float
    (aByte add aInt).value : Int
    (aByte div aLong).value : Double
    (aByte add aLong).value : Long
    (aByte div aFloat).value : Float
    (aByte add aFloat).value : Float
    (aByte div aDouble).value : Double
    (aByte add aDouble).value : Double
    (aByte div aBigDecimal).value : BigDecimal
    (aByte add aBigDecimal).value : BigDecimal
    (aByte div aByteO).value : Option[Float]
    (aByte add aByteO).value : Option[Byte]
    (aByte div aIntO).value : Option[Float]
    (aByte add aIntO).value : Option[Int]
    (aByte div aLongO).value : Option[Double]
    (aByte add aLongO).value : Option[Long]
    (aByte div aFloatO).value : Option[Float]
    (aByte add aFloatO).value : Option[Float]
    (aByte div aDoubleO).value : Option[Double]
    (aByte add aDoubleO).value : Option[Double]
    (aByte div aBigDecimalO).value : Option[BigDecimal]
    (aByte add aBigDecimalO).value : Option[BigDecimal]
    (aInt div aByte).value : Float
    (aInt add aByte).value : Int
    (aInt div aInt).value : Float
    (aInt add aInt).value : Int
    (aInt div aLong).value : Double
    (aInt add aLong).value : Long
    (aInt div aFloat).value : Float
    (aInt add aFloat).value : Float
    (aInt div aDouble).value : Double
    (aInt add aDouble).value : Double
    (aInt div aBigDecimal).value : BigDecimal
    (aInt add aBigDecimal).value : BigDecimal
    (aInt div aByteO).value : Option[Float]
    (aInt add aByteO).value : Option[Int]
    (aInt div aIntO).value : Option[Float]
    (aInt add aIntO).value : Option[Int]
    (aInt div aLongO).value : Option[Double]
    (aInt add aLongO).value : Option[Long]
    (aInt div aFloatO).value : Option[Float]
    (aInt add aFloatO).value : Option[Float]
    (aInt div aDoubleO).value : Option[Double]
    (aInt add aDoubleO).value : Option[Double]
    (aInt div aBigDecimalO).value : Option[BigDecimal]
    (aInt add aBigDecimalO).value : Option[BigDecimal]
    (aLong div aByte).value : Double
    (aLong add aByte).value : Long
    (aLong div aInt).value : Double
    (aLong add aInt).value : Long
    (aLong div aLong).value : Double
    (aLong add aLong).value : Long
    (aLong div aFloat).value : Double
    (aLong add aFloat).value : Double
    (aLong div aDouble).value : Double
    (aLong add aDouble).value : Double
    (aLong div aBigDecimal).value : BigDecimal
    (aLong add aBigDecimal).value : BigDecimal
    (aLong div aByteO).value : Option[Double]
    (aLong add aByteO).value : Option[Long]
    (aLong div aIntO).value : Option[Double]
    (aLong add aIntO).value : Option[Long]
    (aLong div aLongO).value : Option[Double]
    (aLong add aLongO).value : Option[Long]
    (aLong div aFloatO).value : Option[Double]
    (aLong add aFloatO).value : Option[Double]
    (aLong div aDoubleO).value : Option[Double]
    (aLong add aDoubleO).value : Option[Double]
    (aLong div aBigDecimalO).value : Option[BigDecimal]
    (aLong add aBigDecimalO).value : Option[BigDecimal]
    (aFloat div aByte).value : Float
    (aFloat add aByte).value : Float
    (aFloat div aInt).value : Float
    (aFloat add aInt).value : Float
    (aFloat div aLong).value : Double
    (aFloat add aLong).value : Double
    (aFloat div aFloat).value : Float
    (aFloat add aFloat).value : Float
    (aFloat div aDouble).value : Double
    (aFloat add aDouble).value : Double
    (aFloat div aBigDecimal).value : BigDecimal
    (aFloat add aBigDecimal).value : BigDecimal
    (aFloat div aByteO).value : Option[Float]
    (aFloat add aByteO).value : Option[Float]
    (aFloat div aIntO).value : Option[Float]
    (aFloat add aIntO).value : Option[Float]
    (aFloat div aLongO).value : Option[Double]
    (aFloat add aLongO).value : Option[Double]
    (aFloat div aFloatO).value : Option[Float]
    (aFloat add aFloatO).value : Option[Float]
    (aFloat div aDoubleO).value : Option[Double]
    (aFloat add aDoubleO).value : Option[Double]
    (aFloat div aBigDecimalO).value : Option[BigDecimal]
    (aFloat add aBigDecimalO).value : Option[BigDecimal]
    (aDouble div aByte).value : Double
    (aDouble add aByte).value : Double
    (aDouble div aInt).value : Double
    (aDouble add aInt).value : Double
    (aDouble div aLong).value : Double
    (aDouble add aLong).value : Double
    (aDouble div aFloat).value : Double
    (aDouble add aFloat).value : Double
    (aDouble div aDouble).value : Double
    (aDouble add aDouble).value : Double
    (aDouble div aBigDecimal).value : BigDecimal
    (aDouble add aBigDecimal).value : BigDecimal
    (aDouble div aByteO).value : Option[Double]
    (aDouble add aByteO).value : Option[Double]
    (aDouble div aIntO).value : Option[Double]
    (aDouble add aIntO).value : Option[Double]
    (aDouble div aLongO).value : Option[Double]
    (aDouble add aLongO).value : Option[Double]
    (aDouble div aFloatO).value : Option[Double]
    (aDouble add aFloatO).value : Option[Double]
    (aDouble div aDoubleO).value : Option[Double]
    (aDouble add aDoubleO).value : Option[Double]
    (aDouble div aBigDecimalO).value : Option[BigDecimal]
    (aDouble add aBigDecimalO).value : Option[BigDecimal]
    (aBigDecimal div aByte).value : BigDecimal
    (aBigDecimal add aByte).value : BigDecimal
    (aBigDecimal div aInt).value : BigDecimal
    (aBigDecimal add aInt).value : BigDecimal
    (aBigDecimal div aLong).value : BigDecimal
    (aBigDecimal add aLong).value : BigDecimal
    (aBigDecimal div aFloat).value : BigDecimal
    (aBigDecimal add aFloat).value : BigDecimal
    (aBigDecimal div aDouble).value : BigDecimal
    (aBigDecimal add aDouble).value : BigDecimal
    (aBigDecimal div aBigDecimal).value : BigDecimal
    (aBigDecimal add aBigDecimal).value : BigDecimal
    (aBigDecimal div aByteO).value : Option[BigDecimal]
    (aBigDecimal add aByteO).value : Option[BigDecimal]
    (aBigDecimal div aIntO).value : Option[BigDecimal]
    (aBigDecimal add aIntO).value : Option[BigDecimal]
    (aBigDecimal div aLongO).value : Option[BigDecimal]
    (aBigDecimal add aLongO).value : Option[BigDecimal]
    (aBigDecimal div aFloatO).value : Option[BigDecimal]
    (aBigDecimal add aFloatO).value : Option[BigDecimal]
    (aBigDecimal div aDoubleO).value : Option[BigDecimal]
    (aBigDecimal add aDoubleO).value : Option[BigDecimal]
    (aBigDecimal div aBigDecimalO).value : Option[BigDecimal]
    (aBigDecimal add aBigDecimalO).value : Option[BigDecimal]
    (aByteO div aByte).value : Option[Float]
    (aByteO add aByte).value : Option[Byte]
    (aByteO div aInt).value : Option[Float]
    (aByteO add aInt).value : Option[Int]
    (aByteO div aLong).value : Option[Double]
    (aByteO add aLong).value : Option[Long]
    (aByteO div aFloat).value : Option[Float]
    (aByteO add aFloat).value : Option[Float]
    (aByteO div aDouble).value : Option[Double]
    (aByteO add aDouble).value : Option[Double]
    (aByteO div aBigDecimal).value : Option[BigDecimal]
    (aByteO add aBigDecimal).value : Option[BigDecimal]
    (aByteO div aByteO).value : Option[Float]
    (aByteO add aByteO).value : Option[Byte]
    (aByteO div aIntO).value : Option[Float]
    (aByteO add aIntO).value : Option[Int]
    (aByteO div aLongO).value : Option[Double]
    (aByteO add aLongO).value : Option[Long]
    (aByteO div aFloatO).value : Option[Float]
    (aByteO add aFloatO).value : Option[Float]
    (aByteO div aDoubleO).value : Option[Double]
    (aByteO add aDoubleO).value : Option[Double]
    (aByteO div aBigDecimalO).value : Option[BigDecimal]
    (aByteO add aBigDecimalO).value : Option[BigDecimal]
    (aIntO div aByte).value : Option[Float]
    (aIntO add aByte).value : Option[Int]
    (aIntO div aInt).value : Option[Float]
    (aIntO add aInt).value : Option[Int]
    (aIntO div aLong).value : Option[Double]
    (aIntO add aLong).value : Option[Long]
    (aIntO div aFloat).value : Option[Float]
    (aIntO add aFloat).value : Option[Float]
    (aIntO div aDouble).value : Option[Double]
    (aIntO add aDouble).value : Option[Double]
    (aIntO div aBigDecimal).value : Option[BigDecimal]
    (aIntO add aBigDecimal).value : Option[BigDecimal]
    (aIntO div aByteO).value : Option[Float]
    (aIntO add aByteO).value : Option[Int]
    (aIntO div aIntO).value : Option[Float]
    (aIntO add aIntO).value : Option[Int]
    (aIntO div aLongO).value : Option[Double]
    (aIntO add aLongO).value : Option[Long]
    (aIntO div aFloatO).value : Option[Float]
    (aIntO add aFloatO).value : Option[Float]
    (aIntO div aDoubleO).value : Option[Double]
    (aIntO add aDoubleO).value : Option[Double]
    (aIntO div aBigDecimalO).value : Option[BigDecimal]
    (aIntO add aBigDecimalO).value : Option[BigDecimal]
    (aLongO div aByte).value : Option[Double]
    (aLongO add aByte).value : Option[Long]
    (aLongO div aInt).value : Option[Double]
    (aLongO add aInt).value : Option[Long]
    (aLongO div aLong).value : Option[Double]
    (aLongO add aLong).value : Option[Long]
    (aLongO div aFloat).value : Option[Double]
    (aLongO add aFloat).value : Option[Double]
    (aLongO div aDouble).value : Option[Double]
    (aLongO add aDouble).value : Option[Double]
    (aLongO div aBigDecimal).value : Option[BigDecimal]
    (aLongO add aBigDecimal).value : Option[BigDecimal]
    (aLongO div aByteO).value : Option[Double]
    (aLongO add aByteO).value : Option[Long]
    (aLongO div aIntO).value : Option[Double]
    (aLongO add aIntO).value : Option[Long]
    (aLongO div aLongO).value : Option[Double]
    (aLongO add aLongO).value : Option[Long]
    (aLongO div aFloatO).value : Option[Double]
    (aLongO add aFloatO).value : Option[Double]
    (aLongO div aDoubleO).value : Option[Double]
    (aLongO add aDoubleO).value : Option[Double]
    (aLongO div aBigDecimalO).value : Option[BigDecimal]
    (aLongO add aBigDecimalO).value : Option[BigDecimal]
    (aFloatO div aByte).value : Option[Float]
    (aFloatO add aByte).value : Option[Float]
    (aFloatO div aInt).value : Option[Float]
    (aFloatO add aInt).value : Option[Float]
    (aFloatO div aLong).value : Option[Double]
    (aFloatO add aLong).value : Option[Double]
    (aFloatO div aFloat).value : Option[Float]
    (aFloatO add aFloat).value : Option[Float]
    (aFloatO div aDouble).value : Option[Double]
    (aFloatO add aDouble).value : Option[Double]
    (aFloatO div aBigDecimal).value : Option[BigDecimal]
    (aFloatO add aBigDecimal).value : Option[BigDecimal]
    (aFloatO div aByteO).value : Option[Float]
    (aFloatO add aByteO).value : Option[Float]
    (aFloatO div aIntO).value : Option[Float]
    (aFloatO add aIntO).value : Option[Float]
    (aFloatO div aLongO).value : Option[Double]
    (aFloatO add aLongO).value : Option[Double]
    (aFloatO div aFloatO).value : Option[Float]
    (aFloatO add aFloatO).value : Option[Float]
    (aFloatO div aDoubleO).value : Option[Double]
    (aFloatO add aDoubleO).value : Option[Double]
    (aFloatO div aBigDecimalO).value : Option[BigDecimal]
    (aFloatO add aBigDecimalO).value : Option[BigDecimal]
    (aDoubleO div aByte).value : Option[Double]
    (aDoubleO add aByte).value : Option[Double]
    (aDoubleO div aInt).value : Option[Double]
    (aDoubleO add aInt).value : Option[Double]
    (aDoubleO div aLong).value : Option[Double]
    (aDoubleO add aLong).value : Option[Double]
    (aDoubleO div aFloat).value : Option[Double]
    (aDoubleO add aFloat).value : Option[Double]
    (aDoubleO div aDouble).value : Option[Double]
    (aDoubleO add aDouble).value : Option[Double]
    (aDoubleO div aBigDecimal).value : Option[BigDecimal]
    (aDoubleO add aBigDecimal).value : Option[BigDecimal]
    (aDoubleO div aByteO).value : Option[Double]
    (aDoubleO add aByteO).value : Option[Double]
    (aDoubleO div aIntO).value : Option[Double]
    (aDoubleO add aIntO).value : Option[Double]
    (aDoubleO div aLongO).value : Option[Double]
    (aDoubleO add aLongO).value : Option[Double]
    (aDoubleO div aFloatO).value : Option[Double]
    (aDoubleO add aFloatO).value : Option[Double]
    (aDoubleO div aDoubleO).value : Option[Double]
    (aDoubleO add aDoubleO).value : Option[Double]
    (aDoubleO div aBigDecimalO).value : Option[BigDecimal]
    (aDoubleO add aBigDecimalO).value : Option[BigDecimal]
    (aBigDecimalO div aByte).value : Option[BigDecimal]
    (aBigDecimalO add aByte).value : Option[BigDecimal]
    (aBigDecimalO div aInt).value : Option[BigDecimal]
    (aBigDecimalO add aInt).value : Option[BigDecimal]
    (aBigDecimalO div aLong).value : Option[BigDecimal]
    (aBigDecimalO add aLong).value : Option[BigDecimal]
    (aBigDecimalO div aFloat).value : Option[BigDecimal]
    (aBigDecimalO add aFloat).value : Option[BigDecimal]
    (aBigDecimalO div aDouble).value : Option[BigDecimal]
    (aBigDecimalO add aDouble).value : Option[BigDecimal]
    (aBigDecimalO div aBigDecimal).value : Option[BigDecimal]
    (aBigDecimalO add aBigDecimal).value : Option[BigDecimal]
    (aBigDecimalO div aByteO).value : Option[BigDecimal]
    (aBigDecimalO add aByteO).value : Option[BigDecimal]
    (aBigDecimalO div aIntO).value : Option[BigDecimal]
    (aBigDecimalO add aIntO).value : Option[BigDecimal]
    (aBigDecimalO div aLongO).value : Option[BigDecimal]
    (aBigDecimalO add aLongO).value : Option[BigDecimal]
    (aBigDecimalO div aFloatO).value : Option[BigDecimal]
    (aBigDecimalO add aFloatO).value : Option[BigDecimal]
    (aBigDecimalO div aDoubleO).value : Option[BigDecimal]
    (aBigDecimalO add aDoubleO).value : Option[BigDecimal]
    (aBigDecimalO div aBigDecimalO).value : Option[BigDecimal]
    (aBigDecimalO add aBigDecimalO).value : Option[BigDecimal]
    avg(aByte div aByte).value : Option[Float]
    avg(aByte add aByte).value : Option[Float]
    avg(aByte div aInt).value : Option[Float]
    avg(aByte add aInt).value : Option[Float]
    avg(aByte div aLong).value : Option[Double]
    avg(aByte add aLong).value : Option[Double]
    avg(aByte div aFloat).value : Option[Float]
    avg(aByte add aFloat).value : Option[Float]
    avg(aByte div aDouble).value : Option[Double]
    avg(aByte add aDouble).value : Option[Double]
    avg(aByte div aBigDecimal).value : Option[BigDecimal]
    avg(aByte add aBigDecimal).value : Option[BigDecimal]
    avg(aByte div aByteO).value : Option[Float]
    avg(aByte add aByteO).value : Option[Float]
    avg(aByte div aIntO).value : Option[Float]
    avg(aByte add aIntO).value : Option[Float]
    avg(aByte div aLongO).value : Option[Double]
    avg(aByte add aLongO).value : Option[Double]
    avg(aByte div aFloatO).value : Option[Float]
    avg(aByte add aFloatO).value : Option[Float]
    avg(aByte div aDoubleO).value : Option[Double]
    avg(aByte add aDoubleO).value : Option[Double]
    avg(aByte div aBigDecimalO).value : Option[BigDecimal]
    avg(aByte add aBigDecimalO).value : Option[BigDecimal]
    avg(aInt div aByte).value : Option[Float]
    avg(aInt add aByte).value : Option[Float]
    avg(aInt div aInt).value : Option[Float]
    avg(aInt add aInt).value : Option[Float]
    avg(aInt div aLong).value : Option[Double]
    avg(aInt add aLong).value : Option[Double]
    avg(aInt div aFloat).value : Option[Float]
    avg(aInt add aFloat).value : Option[Float]
    avg(aInt div aDouble).value : Option[Double]
    avg(aInt add aDouble).value : Option[Double]
    avg(aInt div aBigDecimal).value : Option[BigDecimal]
    avg(aInt add aBigDecimal).value : Option[BigDecimal]
    avg(aInt div aByteO).value : Option[Float]
    avg(aInt add aByteO).value : Option[Float]
    avg(aInt div aIntO).value : Option[Float]
    avg(aInt add aIntO).value : Option[Float]
    avg(aInt div aLongO).value : Option[Double]
    avg(aInt add aLongO).value : Option[Double]
    avg(aInt div aFloatO).value : Option[Float]
    avg(aInt add aFloatO).value : Option[Float]
    avg(aInt div aDoubleO).value : Option[Double]
    avg(aInt add aDoubleO).value : Option[Double]
    avg(aInt div aBigDecimalO).value : Option[BigDecimal]
    avg(aInt add aBigDecimalO).value : Option[BigDecimal]
    avg(aLong div aByte).value : Option[Double]
    avg(aLong add aByte).value : Option[Double]
    avg(aLong div aInt).value : Option[Double]
    avg(aLong add aInt).value : Option[Double]
    avg(aLong div aLong).value : Option[Double]
    avg(aLong add aLong).value : Option[Double]
    avg(aLong div aFloat).value : Option[Double]
    avg(aLong add aFloat).value : Option[Double]
    avg(aLong div aDouble).value : Option[Double]
    avg(aLong add aDouble).value : Option[Double]
    avg(aLong div aBigDecimal).value : Option[BigDecimal]
    avg(aLong add aBigDecimal).value : Option[BigDecimal]
    avg(aLong div aByteO).value : Option[Double]
    avg(aLong add aByteO).value : Option[Double]
    avg(aLong div aIntO).value : Option[Double]
    avg(aLong add aIntO).value : Option[Double]
    avg(aLong div aLongO).value : Option[Double]
    avg(aLong add aLongO).value : Option[Double]
    avg(aLong div aFloatO).value : Option[Double]
    avg(aLong add aFloatO).value : Option[Double]
    avg(aLong div aDoubleO).value : Option[Double]
    avg(aLong add aDoubleO).value : Option[Double]
    avg(aLong div aBigDecimalO).value : Option[BigDecimal]
    avg(aLong add aBigDecimalO).value : Option[BigDecimal]
    avg(aFloat div aByte).value : Option[Float]
    avg(aFloat add aByte).value : Option[Float]
    avg(aFloat div aInt).value : Option[Float]
    avg(aFloat add aInt).value : Option[Float]
    avg(aFloat div aLong).value : Option[Double]
    avg(aFloat add aLong).value : Option[Double]
    avg(aFloat div aFloat).value : Option[Float]
    avg(aFloat add aFloat).value : Option[Float]
    avg(aFloat div aDouble).value : Option[Double]
    avg(aFloat add aDouble).value : Option[Double]
    avg(aFloat div aBigDecimal).value : Option[BigDecimal]
    avg(aFloat add aBigDecimal).value : Option[BigDecimal]
    avg(aFloat div aByteO).value : Option[Float]
    avg(aFloat add aByteO).value : Option[Float]
    avg(aFloat div aIntO).value : Option[Float]
    avg(aFloat add aIntO).value : Option[Float]
    avg(aFloat div aLongO).value : Option[Double]
    avg(aFloat add aLongO).value : Option[Double]
    avg(aFloat div aFloatO).value : Option[Float]
    avg(aFloat add aFloatO).value : Option[Float]
    avg(aFloat div aDoubleO).value : Option[Double]
    avg(aFloat add aDoubleO).value : Option[Double]
    avg(aFloat div aBigDecimalO).value : Option[BigDecimal]
    avg(aFloat add aBigDecimalO).value : Option[BigDecimal]
    avg(aDouble div aByte).value : Option[Double]
    avg(aDouble add aByte).value : Option[Double]
    avg(aDouble div aInt).value : Option[Double]
    avg(aDouble add aInt).value : Option[Double]
    avg(aDouble div aLong).value : Option[Double]
    avg(aDouble add aLong).value : Option[Double]
    avg(aDouble div aFloat).value : Option[Double]
    avg(aDouble add aFloat).value : Option[Double]
    avg(aDouble div aDouble).value : Option[Double]
    avg(aDouble add aDouble).value : Option[Double]
    avg(aDouble div aBigDecimal).value : Option[BigDecimal]
    avg(aDouble add aBigDecimal).value : Option[BigDecimal]
    avg(aDouble div aByteO).value : Option[Double]
    avg(aDouble add aByteO).value : Option[Double]
    avg(aDouble div aIntO).value : Option[Double]
    avg(aDouble add aIntO).value : Option[Double]
    avg(aDouble div aLongO).value : Option[Double]
    avg(aDouble add aLongO).value : Option[Double]
    avg(aDouble div aFloatO).value : Option[Double]
    avg(aDouble add aFloatO).value : Option[Double]
    avg(aDouble div aDoubleO).value : Option[Double]
    avg(aDouble add aDoubleO).value : Option[Double]
    avg(aDouble div aBigDecimalO).value : Option[BigDecimal]
    avg(aDouble add aBigDecimalO).value : Option[BigDecimal]
    avg(aBigDecimal div aByte).value : Option[BigDecimal]
    avg(aBigDecimal add aByte).value : Option[BigDecimal]
    avg(aBigDecimal div aInt).value : Option[BigDecimal]
    avg(aBigDecimal add aInt).value : Option[BigDecimal]
    avg(aBigDecimal div aLong).value : Option[BigDecimal]
    avg(aBigDecimal add aLong).value : Option[BigDecimal]
    avg(aBigDecimal div aFloat).value : Option[BigDecimal]
    avg(aBigDecimal add aFloat).value : Option[BigDecimal]
    avg(aBigDecimal div aDouble).value : Option[BigDecimal]
    avg(aBigDecimal add aDouble).value : Option[BigDecimal]
    avg(aBigDecimal div aBigDecimal).value : Option[BigDecimal]
    avg(aBigDecimal add aBigDecimal).value : Option[BigDecimal]
    avg(aBigDecimal div aByteO).value : Option[BigDecimal]
    avg(aBigDecimal add aByteO).value : Option[BigDecimal]
    avg(aBigDecimal div aIntO).value : Option[BigDecimal]
    avg(aBigDecimal add aIntO).value : Option[BigDecimal]
    avg(aBigDecimal div aLongO).value : Option[BigDecimal]
    avg(aBigDecimal add aLongO).value : Option[BigDecimal]
    avg(aBigDecimal div aFloatO).value : Option[BigDecimal]
    avg(aBigDecimal add aFloatO).value : Option[BigDecimal]
    avg(aBigDecimal div aDoubleO).value : Option[BigDecimal]
    avg(aBigDecimal add aDoubleO).value : Option[BigDecimal]
    avg(aBigDecimal div aBigDecimalO).value : Option[BigDecimal]
    avg(aBigDecimal add aBigDecimalO).value : Option[BigDecimal]
    avg(aByteO div aByte).value : Option[Float]
    avg(aByteO add aByte).value : Option[Float]
    avg(aByteO div aInt).value : Option[Float]
    avg(aByteO add aInt).value : Option[Float]
    avg(aByteO div aLong).value : Option[Double]
    avg(aByteO add aLong).value : Option[Double]
    avg(aByteO div aFloat).value : Option[Float]
    avg(aByteO add aFloat).value : Option[Float]
    avg(aByteO div aDouble).value : Option[Double]
    avg(aByteO add aDouble).value : Option[Double]
    avg(aByteO div aBigDecimal).value : Option[BigDecimal]
    avg(aByteO add aBigDecimal).value : Option[BigDecimal]
    avg(aByteO div aByteO).value : Option[Float]
    avg(aByteO add aByteO).value : Option[Float]
    avg(aByteO div aIntO).value : Option[Float]
    avg(aByteO add aIntO).value : Option[Float]
    avg(aByteO div aLongO).value : Option[Double]
    avg(aByteO add aLongO).value : Option[Double]
    avg(aByteO div aFloatO).value : Option[Float]
    avg(aByteO add aFloatO).value : Option[Float]
    avg(aByteO div aDoubleO).value : Option[Double]
    avg(aByteO add aDoubleO).value : Option[Double]
    avg(aByteO div aBigDecimalO).value : Option[BigDecimal]
    avg(aByteO add aBigDecimalO).value : Option[BigDecimal]
    avg(aIntO div aByte).value : Option[Float]
    avg(aIntO add aByte).value : Option[Float]
    avg(aIntO div aInt).value : Option[Float]
    avg(aIntO add aInt).value : Option[Float]
    avg(aIntO div aLong).value : Option[Double]
    avg(aIntO add aLong).value : Option[Double]
    avg(aIntO div aFloat).value : Option[Float]
    avg(aIntO add aFloat).value : Option[Float]
    avg(aIntO div aDouble).value : Option[Double]
    avg(aIntO add aDouble).value : Option[Double]
    avg(aIntO div aBigDecimal).value : Option[BigDecimal]
    avg(aIntO add aBigDecimal).value : Option[BigDecimal]
    avg(aIntO div aByteO).value : Option[Float]
    avg(aIntO add aByteO).value : Option[Float]
    avg(aIntO div aIntO).value : Option[Float]
    avg(aIntO add aIntO).value : Option[Float]
    avg(aIntO div aLongO).value : Option[Double]
    avg(aIntO add aLongO).value : Option[Double]
    avg(aIntO div aFloatO).value : Option[Float]
    avg(aIntO add aFloatO).value : Option[Float]
    avg(aIntO div aDoubleO).value : Option[Double]
    avg(aIntO add aDoubleO).value : Option[Double]
    avg(aIntO div aBigDecimalO).value : Option[BigDecimal]
    avg(aIntO add aBigDecimalO).value : Option[BigDecimal]
    avg(aLongO div aByte).value : Option[Double]
    avg(aLongO add aByte).value : Option[Double]
    avg(aLongO div aInt).value : Option[Double]
    avg(aLongO add aInt).value : Option[Double]
    avg(aLongO div aLong).value : Option[Double]
    avg(aLongO add aLong).value : Option[Double]
    avg(aLongO div aFloat).value : Option[Double]
    avg(aLongO add aFloat).value : Option[Double]
    avg(aLongO div aDouble).value : Option[Double]
    avg(aLongO add aDouble).value : Option[Double]
    avg(aLongO div aBigDecimal).value : Option[BigDecimal]
    avg(aLongO add aBigDecimal).value : Option[BigDecimal]
    avg(aLongO div aByteO).value : Option[Double]
    avg(aLongO add aByteO).value : Option[Double]
    avg(aLongO div aIntO).value : Option[Double]
    avg(aLongO add aIntO).value : Option[Double]
    avg(aLongO div aLongO).value : Option[Double]
    avg(aLongO add aLongO).value : Option[Double]
    avg(aLongO div aFloatO).value : Option[Double]
    avg(aLongO add aFloatO).value : Option[Double]
    avg(aLongO div aDoubleO).value : Option[Double]
    avg(aLongO add aDoubleO).value : Option[Double]
    avg(aLongO div aBigDecimalO).value : Option[BigDecimal]
    avg(aLongO add aBigDecimalO).value : Option[BigDecimal]
    avg(aFloatO div aByte).value : Option[Float]
    avg(aFloatO add aByte).value : Option[Float]
    avg(aFloatO div aInt).value : Option[Float]
    avg(aFloatO add aInt).value : Option[Float]
    avg(aFloatO div aLong).value : Option[Double]
    avg(aFloatO add aLong).value : Option[Double]
    avg(aFloatO div aFloat).value : Option[Float]
    avg(aFloatO add aFloat).value : Option[Float]
    avg(aFloatO div aDouble).value : Option[Double]
    avg(aFloatO add aDouble).value : Option[Double]
    avg(aFloatO div aBigDecimal).value : Option[BigDecimal]
    avg(aFloatO add aBigDecimal).value : Option[BigDecimal]
    avg(aFloatO div aByteO).value : Option[Float]
    avg(aFloatO add aByteO).value : Option[Float]
    avg(aFloatO div aIntO).value : Option[Float]
    avg(aFloatO add aIntO).value : Option[Float]
    avg(aFloatO div aLongO).value : Option[Double]
    avg(aFloatO add aLongO).value : Option[Double]
    avg(aFloatO div aFloatO).value : Option[Float]
    avg(aFloatO add aFloatO).value : Option[Float]
    avg(aFloatO div aDoubleO).value : Option[Double]
    avg(aFloatO add aDoubleO).value : Option[Double]
    avg(aFloatO div aBigDecimalO).value : Option[BigDecimal]
    avg(aFloatO add aBigDecimalO).value : Option[BigDecimal]
    avg(aDoubleO div aByte).value : Option[Double]
    avg(aDoubleO add aByte).value : Option[Double]
    avg(aDoubleO div aInt).value : Option[Double]
    avg(aDoubleO add aInt).value : Option[Double]
    avg(aDoubleO div aLong).value : Option[Double]
    avg(aDoubleO add aLong).value : Option[Double]
    avg(aDoubleO div aFloat).value : Option[Double]
    avg(aDoubleO add aFloat).value : Option[Double]
    avg(aDoubleO div aDouble).value : Option[Double]
    avg(aDoubleO add aDouble).value : Option[Double]
    avg(aDoubleO div aBigDecimal).value : Option[BigDecimal]
    avg(aDoubleO add aBigDecimal).value : Option[BigDecimal]
    avg(aDoubleO div aByteO).value : Option[Double]
    avg(aDoubleO add aByteO).value : Option[Double]
    avg(aDoubleO div aIntO).value : Option[Double]
    avg(aDoubleO add aIntO).value : Option[Double]
    avg(aDoubleO div aLongO).value : Option[Double]
    avg(aDoubleO add aLongO).value : Option[Double]
    avg(aDoubleO div aFloatO).value : Option[Double]
    avg(aDoubleO add aFloatO).value : Option[Double]
    avg(aDoubleO div aDoubleO).value : Option[Double]
    avg(aDoubleO add aDoubleO).value : Option[Double]
    avg(aDoubleO div aBigDecimalO).value : Option[BigDecimal]
    avg(aDoubleO add aBigDecimalO).value : Option[BigDecimal]
    avg(aBigDecimalO div aByte).value : Option[BigDecimal]
    avg(aBigDecimalO add aByte).value : Option[BigDecimal]
    avg(aBigDecimalO div aInt).value : Option[BigDecimal]
    avg(aBigDecimalO add aInt).value : Option[BigDecimal]
    avg(aBigDecimalO div aLong).value : Option[BigDecimal]
    avg(aBigDecimalO add aLong).value : Option[BigDecimal]
    avg(aBigDecimalO div aFloat).value : Option[BigDecimal]
    avg(aBigDecimalO add aFloat).value : Option[BigDecimal]
    avg(aBigDecimalO div aDouble).value : Option[BigDecimal]
    avg(aBigDecimalO add aDouble).value : Option[BigDecimal]
    avg(aBigDecimalO div aBigDecimal).value : Option[BigDecimal]
    avg(aBigDecimalO add aBigDecimal).value : Option[BigDecimal]
    avg(aBigDecimalO div aByteO).value : Option[BigDecimal]
    avg(aBigDecimalO add aByteO).value : Option[BigDecimal]
    avg(aBigDecimalO div aIntO).value : Option[BigDecimal]
    avg(aBigDecimalO add aIntO).value : Option[BigDecimal]
    avg(aBigDecimalO div aLongO).value : Option[BigDecimal]
    avg(aBigDecimalO add aLongO).value : Option[BigDecimal]
    avg(aBigDecimalO div aFloatO).value : Option[BigDecimal]
    avg(aBigDecimalO add aFloatO).value : Option[BigDecimal]
    avg(aBigDecimalO div aDoubleO).value : Option[BigDecimal]
    avg(aBigDecimalO add aDoubleO).value : Option[BigDecimal]
    avg(aBigDecimalO div aBigDecimalO).value : Option[BigDecimal]
    avg(aBigDecimalO add aBigDecimalO).value : Option[BigDecimal]
    max(aByte div aByte).value : Option[Float]
    max(aByte add aByte).value : Option[Byte]
    max(aByte div aInt).value : Option[Float]
    max(aByte add aInt).value : Option[Int]
    max(aByte div aLong).value : Option[Double]
    max(aByte add aLong).value : Option[Long]
    max(aByte div aFloat).value : Option[Float]
    max(aByte add aFloat).value : Option[Float]
    max(aByte div aDouble).value : Option[Double]
    max(aByte add aDouble).value : Option[Double]
    max(aByte div aBigDecimal).value : Option[BigDecimal]
    max(aByte add aBigDecimal).value : Option[BigDecimal]
    max(aByte div aByteO).value : Option[Float]
    max(aByte add aByteO).value : Option[Byte]
    max(aByte div aIntO).value : Option[Float]
    max(aByte add aIntO).value : Option[Int]
    max(aByte div aLongO).value : Option[Double]
    max(aByte add aLongO).value : Option[Long]
    max(aByte div aFloatO).value : Option[Float]
    max(aByte add aFloatO).value : Option[Float]
    max(aByte div aDoubleO).value : Option[Double]
    max(aByte add aDoubleO).value : Option[Double]
    max(aByte div aBigDecimalO).value : Option[BigDecimal]
    max(aByte add aBigDecimalO).value : Option[BigDecimal]
    max(aInt div aByte).value : Option[Float]
    max(aInt add aByte).value : Option[Int]
    max(aInt div aInt).value : Option[Float]
    max(aInt add aInt).value : Option[Int]
    max(aInt div aLong).value : Option[Double]
    max(aInt add aLong).value : Option[Long]
    max(aInt div aFloat).value : Option[Float]
    max(aInt add aFloat).value : Option[Float]
    max(aInt div aDouble).value : Option[Double]
    max(aInt add aDouble).value : Option[Double]
    max(aInt div aBigDecimal).value : Option[BigDecimal]
    max(aInt add aBigDecimal).value : Option[BigDecimal]
    max(aInt div aByteO).value : Option[Float]
    max(aInt add aByteO).value : Option[Int]
    max(aInt div aIntO).value : Option[Float]
    max(aInt add aIntO).value : Option[Int]
    max(aInt div aLongO).value : Option[Double]
    max(aInt add aLongO).value : Option[Long]
    max(aInt div aFloatO).value : Option[Float]
    max(aInt add aFloatO).value : Option[Float]
    max(aInt div aDoubleO).value : Option[Double]
    max(aInt add aDoubleO).value : Option[Double]
    max(aInt div aBigDecimalO).value : Option[BigDecimal]
    max(aInt add aBigDecimalO).value : Option[BigDecimal]
    max(aLong div aByte).value : Option[Double]
    max(aLong add aByte).value : Option[Long]
    max(aLong div aInt).value : Option[Double]
    max(aLong add aInt).value : Option[Long]
    max(aLong div aLong).value : Option[Double]
    max(aLong add aLong).value : Option[Long]
    max(aLong div aFloat).value : Option[Double]
    max(aLong add aFloat).value : Option[Double]
    max(aLong div aDouble).value : Option[Double]
    max(aLong add aDouble).value : Option[Double]
    max(aLong div aBigDecimal).value : Option[BigDecimal]
    max(aLong add aBigDecimal).value : Option[BigDecimal]
    max(aLong div aByteO).value : Option[Double]
    max(aLong add aByteO).value : Option[Long]
    max(aLong div aIntO).value : Option[Double]
    max(aLong add aIntO).value : Option[Long]
    max(aLong div aLongO).value : Option[Double]
    max(aLong add aLongO).value : Option[Long]
    max(aLong div aFloatO).value : Option[Double]
    max(aLong add aFloatO).value : Option[Double]
    max(aLong div aDoubleO).value : Option[Double]
    max(aLong add aDoubleO).value : Option[Double]
    max(aLong div aBigDecimalO).value : Option[BigDecimal]
    max(aLong add aBigDecimalO).value : Option[BigDecimal]
    max(aFloat div aByte).value : Option[Float]
    max(aFloat add aByte).value : Option[Float]
    max(aFloat div aInt).value : Option[Float]
    max(aFloat add aInt).value : Option[Float]
    max(aFloat div aLong).value : Option[Double]
    max(aFloat add aLong).value : Option[Double]
    max(aFloat div aFloat).value : Option[Float]
    max(aFloat add aFloat).value : Option[Float]
    max(aFloat div aDouble).value : Option[Double]
    max(aFloat add aDouble).value : Option[Double]
    max(aFloat div aBigDecimal).value : Option[BigDecimal]
    max(aFloat add aBigDecimal).value : Option[BigDecimal]
    max(aFloat div aByteO).value : Option[Float]
    max(aFloat add aByteO).value : Option[Float]
    max(aFloat div aIntO).value : Option[Float]
    max(aFloat add aIntO).value : Option[Float]
    max(aFloat div aLongO).value : Option[Double]
    max(aFloat add aLongO).value : Option[Double]
    max(aFloat div aFloatO).value : Option[Float]
    max(aFloat add aFloatO).value : Option[Float]
    max(aFloat div aDoubleO).value : Option[Double]
    max(aFloat add aDoubleO).value : Option[Double]
    max(aFloat div aBigDecimalO).value : Option[BigDecimal]
    max(aFloat add aBigDecimalO).value : Option[BigDecimal]
    max(aDouble div aByte).value : Option[Double]
    max(aDouble add aByte).value : Option[Double]
    max(aDouble div aInt).value : Option[Double]
    max(aDouble add aInt).value : Option[Double]
    max(aDouble div aLong).value : Option[Double]
    max(aDouble add aLong).value : Option[Double]
    max(aDouble div aFloat).value : Option[Double]
    max(aDouble add aFloat).value : Option[Double]
    max(aDouble div aDouble).value : Option[Double]
    max(aDouble add aDouble).value : Option[Double]
    max(aDouble div aBigDecimal).value : Option[BigDecimal]
    max(aDouble add aBigDecimal).value : Option[BigDecimal]
    max(aDouble div aByteO).value : Option[Double]
    max(aDouble add aByteO).value : Option[Double]
    max(aDouble div aIntO).value : Option[Double]
    max(aDouble add aIntO).value : Option[Double]
    max(aDouble div aLongO).value : Option[Double]
    max(aDouble add aLongO).value : Option[Double]
    max(aDouble div aFloatO).value : Option[Double]
    max(aDouble add aFloatO).value : Option[Double]
    max(aDouble div aDoubleO).value : Option[Double]
    max(aDouble add aDoubleO).value : Option[Double]
    max(aDouble div aBigDecimalO).value : Option[BigDecimal]
    max(aDouble add aBigDecimalO).value : Option[BigDecimal]
    max(aBigDecimal div aByte).value : Option[BigDecimal]
    max(aBigDecimal add aByte).value : Option[BigDecimal]
    max(aBigDecimal div aInt).value : Option[BigDecimal]
    max(aBigDecimal add aInt).value : Option[BigDecimal]
    max(aBigDecimal div aLong).value : Option[BigDecimal]
    max(aBigDecimal add aLong).value : Option[BigDecimal]
    max(aBigDecimal div aFloat).value : Option[BigDecimal]
    max(aBigDecimal add aFloat).value : Option[BigDecimal]
    max(aBigDecimal div aDouble).value : Option[BigDecimal]
    max(aBigDecimal add aDouble).value : Option[BigDecimal]
    max(aBigDecimal div aBigDecimal).value : Option[BigDecimal]
    max(aBigDecimal add aBigDecimal).value : Option[BigDecimal]
    max(aBigDecimal div aByteO).value : Option[BigDecimal]
    max(aBigDecimal add aByteO).value : Option[BigDecimal]
    max(aBigDecimal div aIntO).value : Option[BigDecimal]
    max(aBigDecimal add aIntO).value : Option[BigDecimal]
    max(aBigDecimal div aLongO).value : Option[BigDecimal]
    max(aBigDecimal add aLongO).value : Option[BigDecimal]
    max(aBigDecimal div aFloatO).value : Option[BigDecimal]
    max(aBigDecimal add aFloatO).value : Option[BigDecimal]
    max(aBigDecimal div aDoubleO).value : Option[BigDecimal]
    max(aBigDecimal add aDoubleO).value : Option[BigDecimal]
    max(aBigDecimal div aBigDecimalO).value : Option[BigDecimal]
    max(aBigDecimal add aBigDecimalO).value : Option[BigDecimal]
    max(aByteO div aByte).value : Option[Float]
    max(aByteO add aByte).value : Option[Byte]
    max(aByteO div aInt).value : Option[Float]
    max(aByteO add aInt).value : Option[Int]
    max(aByteO div aLong).value : Option[Double]
    max(aByteO add aLong).value : Option[Long]
    max(aByteO div aFloat).value : Option[Float]
    max(aByteO add aFloat).value : Option[Float]
    max(aByteO div aDouble).value : Option[Double]
    max(aByteO add aDouble).value : Option[Double]
    max(aByteO div aBigDecimal).value : Option[BigDecimal]
    max(aByteO add aBigDecimal).value : Option[BigDecimal]
    max(aByteO div aByteO).value : Option[Float]
    max(aByteO add aByteO).value : Option[Byte]
    max(aByteO div aIntO).value : Option[Float]
    max(aByteO add aIntO).value : Option[Int]
    max(aByteO div aLongO).value : Option[Double]
    max(aByteO add aLongO).value : Option[Long]
    max(aByteO div aFloatO).value : Option[Float]
    max(aByteO add aFloatO).value : Option[Float]
    max(aByteO div aDoubleO).value : Option[Double]
    max(aByteO add aDoubleO).value : Option[Double]
    max(aByteO div aBigDecimalO).value : Option[BigDecimal]
    max(aByteO add aBigDecimalO).value : Option[BigDecimal]
    max(aIntO div aByte).value : Option[Float]
    max(aIntO add aByte).value : Option[Int]
    max(aIntO div aInt).value : Option[Float]
    max(aIntO add aInt).value : Option[Int]
    max(aIntO div aLong).value : Option[Double]
    max(aIntO add aLong).value : Option[Long]
    max(aIntO div aFloat).value : Option[Float]
    max(aIntO add aFloat).value : Option[Float]
    max(aIntO div aDouble).value : Option[Double]
    max(aIntO add aDouble).value : Option[Double]
    max(aIntO div aBigDecimal).value : Option[BigDecimal]
    max(aIntO add aBigDecimal).value : Option[BigDecimal]
    max(aIntO div aByteO).value : Option[Float]
    max(aIntO add aByteO).value : Option[Int]
    max(aIntO div aIntO).value : Option[Float]
    max(aIntO add aIntO).value : Option[Int]
    max(aIntO div aLongO).value : Option[Double]
    max(aIntO add aLongO).value : Option[Long]
    max(aIntO div aFloatO).value : Option[Float]
    max(aIntO add aFloatO).value : Option[Float]
    max(aIntO div aDoubleO).value : Option[Double]
    max(aIntO add aDoubleO).value : Option[Double]
    max(aIntO div aBigDecimalO).value : Option[BigDecimal]
    max(aIntO add aBigDecimalO).value : Option[BigDecimal]
    max(aLongO div aByte).value : Option[Double]
    max(aLongO add aByte).value : Option[Long]
    max(aLongO div aInt).value : Option[Double]
    max(aLongO add aInt).value : Option[Long]
    max(aLongO div aLong).value : Option[Double]
    max(aLongO add aLong).value : Option[Long]
    max(aLongO div aFloat).value : Option[Double]
    max(aLongO add aFloat).value : Option[Double]
    max(aLongO div aDouble).value : Option[Double]
    max(aLongO add aDouble).value : Option[Double]
    max(aLongO div aBigDecimal).value : Option[BigDecimal]
    max(aLongO add aBigDecimal).value : Option[BigDecimal]
    max(aLongO div aByteO).value : Option[Double]
    max(aLongO add aByteO).value : Option[Long]
    max(aLongO div aIntO).value : Option[Double]
    max(aLongO add aIntO).value : Option[Long]
    max(aLongO div aLongO).value : Option[Double]
    max(aLongO add aLongO).value : Option[Long]
    max(aLongO div aFloatO).value : Option[Double]
    max(aLongO add aFloatO).value : Option[Double]
    max(aLongO div aDoubleO).value : Option[Double]
    max(aLongO add aDoubleO).value : Option[Double]
    max(aLongO div aBigDecimalO).value : Option[BigDecimal]
    max(aLongO add aBigDecimalO).value : Option[BigDecimal]
    max(aFloatO div aByte).value : Option[Float]
    max(aFloatO add aByte).value : Option[Float]
    max(aFloatO div aInt).value : Option[Float]
    max(aFloatO add aInt).value : Option[Float]
    max(aFloatO div aLong).value : Option[Double]
    max(aFloatO add aLong).value : Option[Double]
    max(aFloatO div aFloat).value : Option[Float]
    max(aFloatO add aFloat).value : Option[Float]
    max(aFloatO div aDouble).value : Option[Double]
    max(aFloatO add aDouble).value : Option[Double]
    max(aFloatO div aBigDecimal).value : Option[BigDecimal]
    max(aFloatO add aBigDecimal).value : Option[BigDecimal]
    max(aFloatO div aByteO).value : Option[Float]
    max(aFloatO add aByteO).value : Option[Float]
    max(aFloatO div aIntO).value : Option[Float]
    max(aFloatO add aIntO).value : Option[Float]
    max(aFloatO div aLongO).value : Option[Double]
    max(aFloatO add aLongO).value : Option[Double]
    max(aFloatO div aFloatO).value : Option[Float]
    max(aFloatO add aFloatO).value : Option[Float]
    max(aFloatO div aDoubleO).value : Option[Double]
    max(aFloatO add aDoubleO).value : Option[Double]
    max(aFloatO div aBigDecimalO).value : Option[BigDecimal]
    max(aFloatO add aBigDecimalO).value : Option[BigDecimal]
    max(aDoubleO div aByte).value : Option[Double]
    max(aDoubleO add aByte).value : Option[Double]
    max(aDoubleO div aInt).value : Option[Double]
    max(aDoubleO add aInt).value : Option[Double]
    max(aDoubleO div aLong).value : Option[Double]
    max(aDoubleO add aLong).value : Option[Double]
    max(aDoubleO div aFloat).value : Option[Double]
    max(aDoubleO add aFloat).value : Option[Double]
    max(aDoubleO div aDouble).value : Option[Double]
    max(aDoubleO add aDouble).value : Option[Double]
    max(aDoubleO div aBigDecimal).value : Option[BigDecimal]
    max(aDoubleO add aBigDecimal).value : Option[BigDecimal]
    max(aDoubleO div aByteO).value : Option[Double]
    max(aDoubleO add aByteO).value : Option[Double]
    max(aDoubleO div aIntO).value : Option[Double]
    max(aDoubleO add aIntO).value : Option[Double]
    max(aDoubleO div aLongO).value : Option[Double]
    max(aDoubleO add aLongO).value : Option[Double]
    max(aDoubleO div aFloatO).value : Option[Double]
    max(aDoubleO add aFloatO).value : Option[Double]
    max(aDoubleO div aDoubleO).value : Option[Double]
    max(aDoubleO add aDoubleO).value : Option[Double]
    max(aDoubleO div aBigDecimalO).value : Option[BigDecimal]
    max(aDoubleO add aBigDecimalO).value : Option[BigDecimal]
    max(aBigDecimalO div aByte).value : Option[BigDecimal]
    max(aBigDecimalO add aByte).value : Option[BigDecimal]
    max(aBigDecimalO div aInt).value : Option[BigDecimal]
    max(aBigDecimalO add aInt).value : Option[BigDecimal]
    max(aBigDecimalO div aLong).value : Option[BigDecimal]
    max(aBigDecimalO add aLong).value : Option[BigDecimal]
    max(aBigDecimalO div aFloat).value : Option[BigDecimal]
    max(aBigDecimalO add aFloat).value : Option[BigDecimal]
    max(aBigDecimalO div aDouble).value : Option[BigDecimal]
    max(aBigDecimalO add aDouble).value : Option[BigDecimal]
    max(aBigDecimalO div aBigDecimal).value : Option[BigDecimal]
    max(aBigDecimalO add aBigDecimal).value : Option[BigDecimal]
    max(aBigDecimalO div aByteO).value : Option[BigDecimal]
    max(aBigDecimalO add aByteO).value : Option[BigDecimal]
    max(aBigDecimalO div aIntO).value : Option[BigDecimal]
    max(aBigDecimalO add aIntO).value : Option[BigDecimal]
    max(aBigDecimalO div aLongO).value : Option[BigDecimal]
    max(aBigDecimalO add aLongO).value : Option[BigDecimal]
    max(aBigDecimalO div aFloatO).value : Option[BigDecimal]
    max(aBigDecimalO add aFloatO).value : Option[BigDecimal]
    max(aBigDecimalO div aDoubleO).value : Option[BigDecimal]
    max(aBigDecimalO add aDoubleO).value : Option[BigDecimal]
    max(aBigDecimalO div aBigDecimalO).value : Option[BigDecimal]
    max(aBigDecimalO add aBigDecimalO).value : Option[BigDecimal]    
  }
}