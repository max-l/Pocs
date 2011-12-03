package poc


object Genre extends Enumeration {
  type Genre = Value
  
  val Bluegrass, Funk, Folk = Value 
}

object WeekDay extends Enumeration {
  type WeekDay = Value
  
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value 
}

object Poc1Tests {

  import Impls._
  
  def enumTests = {
    import Genre._
    import WeekDay._
    
    Bluegrass === Funk
    Option(Bluegrass) === Funk
    Folk === Option(Bluegrass)
    
    Mon === Option(Fri)
    
    //Will not (and should not) compile :
    //Folk === Mon
    //Folk === Option(Mon)
  }  
  
  def typeLevelTests = {
    //avg("")
    avg(1: Byte).value : Option[Float]
    min("").value : Option[String]
    sum(0F).value : Option[Float]
  }
  
  
  def typeLevelTestsComparison = {
  
    import Impls._
    
    1 === 1  
    "" === ""      
    Option("") === ""
    "" === Option("")    
    1 === Option(1)    
    1 between (0, Option(2))
    BigDecimal(1) between (Option(2), 1000F)
  }
  
}