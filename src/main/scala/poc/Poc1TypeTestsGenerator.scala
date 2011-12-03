package poc

object Poc1TypeTestsGenerator {

  def main(args: Array[String]): Unit = {
    generateSystematicTypeLevelTests
  }
  
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
  
}