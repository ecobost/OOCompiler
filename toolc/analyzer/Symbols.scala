package toolc
package analyzer

import utils._
import Types._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String,ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = List[(String,MethodSymbol)]()
    var members = Map[String,VariableSymbol]()

    def lookupMethods(n: String): List[MethodSymbol] = {
      lookupMethodsExceptFor(n, Nil)
    }
    
    // Give a list with all methods in this class that have the given name, except for the methods in the excluded list.
    def lookupMethodsExceptFor(n: String, excluded: List[MethodSymbol]) : List[MethodSymbol] = {
      
      // Assign initially to all methods in this class that have name n
      var meths: List[MethodSymbol] = methods.filter(x => (x._1 == n)).map(x => x._2)
      
      // Look up for methods that should be excluded.
      meths = meths.filter(x => !excluded.contains(x))
      
      // If a method 1 is overriding method 2, include method 2 in the excluded list.
      val newExcludedList: List[MethodSymbol] = meths.map(x => x.overridden).filter (x => x.isDefined).map( x => x.get)
      
      // If this class inherits from other class, get the methods from this class that should be added to the list
      if(parent.isDefined) {
        meths ++ parent.get.lookupMethodsExceptFor(n, excluded ++ newExcludedList)
      }else{
        meths
      }
    }
    
    
    def lookupVar(n: String): Option[VariableSymbol] = {
      if(members.contains(n)) {
        members.get(n)
      }else{
        if(parent.isDefined) {
          parent.get.lookupVar(n)
        }else{
          None
        }
      }
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String,VariableSymbol]()
    var members = Map[String,VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden : Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      if(params.contains(n)) {
        params.get(n)
      }else{
        if(members.contains(n)) {
         members.get(n)
        }else{
           classSymbol.lookupVar(n)     
        }
      }
    }
  }

  class VariableSymbol(val name: String) extends Symbol {
    var dynamicType: Type = TUntyped
    
    def setDynamicType(dynamicT: Type) {
      dynamicType = dynamicT
    }
    
    def getDynamicType: Type = {
      if(dynamicType == TUntyped){
        getType
      }
      else{
        dynamicType
      }
    }
  }
  
}