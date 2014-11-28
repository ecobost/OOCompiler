package toolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {
  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._
    var dynamicalVars: List[VariableSymbol] = Nil

    def tcExpr(expression: ExprTree, expected: Type*): Type = {
      val tpe: Type = expression match {
        case expr: And => {
          tcExpr(expr.lhs, TBoolean)
          tcExpr(expr.rhs, TBoolean)
          TBoolean
        }
        case expr: Or => {
          tcExpr(expr.lhs, TBoolean)
          tcExpr(expr.rhs, TBoolean)
          TBoolean
        }
        case expr: Equals => {
          val T1: Type = tcExpr(expr.lhs)
          val T2: Type = tcExpr(expr.rhs)
          T1 match {
            case TInt | TString | TBoolean | TIntArray => if(T1 != T2) {
                error("Incompatible types in equality: " + T1.toString + ", " + T2.toString, expr)
            }
            case TObject(_) => if(!T2.isSubTypeOf(anyObject)){
              error("Incompatible types in equality: " + T1.toString + ", " + T2.toString, expr)
            }
            case _ => // TUntyped, TError: No action
          }
          TBoolean
        }
        case expr: LessThan => {
          tcExpr(expr.lhs, TInt)
          tcExpr(expr.rhs, TInt)
          TBoolean
        }
        case expr: Plus => {
          val T1: Type = tcExpr(expr.lhs, TInt, TString)
          val T2: Type = tcExpr(expr.rhs, TInt, TString)
          if(T1 == TInt && T2 == TInt) {
            TInt
          }else{
            TString
          }
        }
        case expr: Minus => {
          tcExpr(expr.lhs, TInt)
          tcExpr(expr.rhs, TInt)
          TInt
        }
        case expr: Times => {
          tcExpr(expr.lhs, TInt)
          tcExpr(expr.rhs, TInt)
          TInt
        }
        case expr: Div => {
          tcExpr(expr.lhs, TInt)
          tcExpr(expr.rhs, TInt)
          TInt
        }
        case expr: ArrayRead => {
          tcExpr(expr.arr, TIntArray)
          tcExpr(expr.index, TInt)
          TInt
        }
        case expr: ArrayLength => {
          tcExpr(expr.arr, TIntArray)
          TInt
        }
        case expr: MethodCall => {
          val classType: Type = tcExpr(expr.obj, anyObject)
          var methodType: Type = TUntyped
          classType match {
            case TObject(classSymbol) =>
              val methodList: List[MethodSymbol] = classSymbol.lookupMethods(expr.meth.value)
              
              if(methodList.isEmpty){
                error("Method " + expr.meth.value + " does not exist in class " + classSymbol.name + "(" + classSymbol.position + ")", expr);
              }
              val methodSymbol: Option[MethodSymbol] = selectMethod(methodList, expr.args, expr)
              if(methodSymbol.isDefined){
                // Assign symbol to method identifier
                expr.meth.setSymbol(methodSymbol.get)
                
                methodType= methodSymbol.get.getType
              }
            case _ => error("Type error: Trying to call method " + expr.meth.value + " in an object that is not a class", expr)
          }
          methodType
        }
        case expr: IntLit => TInt
        case expr: StringLit => TString
        case expr: True => TBoolean
        case expr: False => TBoolean
        case expr: Identifier => expr.getType
        case expr: This => expr.getSymbol.getType
        case expr: NewIntArray => {
          tcExpr(expr.size, TInt)
          TIntArray
        }
        case expr: New => expr.tpe.getType
        case expr: Not => {
          tcExpr(expr.expr, TBoolean)
          TBoolean
        }
        case _ => TError //Not possible
      }
      
      expression.setType(tpe);
      if(expected.isEmpty) {
          tpe
      } else {
        if(!expected.exists(e => tpe.isSubTypeOf(e))) {
            error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expression)
            expected.head
        } else {
            tpe
        }
      }
    }
    
    // Select of a list of methods, the one that matches better the list of arguments
    def selectMethod(methList: List[MethodSymbol], argList: List[ExprTree], expr : MethodCall): Option[MethodSymbol] = {
      // Select the methods that match the arguments.
      var methodList = methList.filter( x => doArgTypesMatch(x.argList,argList))
      
      // Rank the methods to see which one is a better fit
      val anotatedMethodList: List[(MethodSymbol, Int)] = methodList.map(rankMethod(_,argList))
      
      // Select method with best ranking
      if(anotatedMethodList.isEmpty){
        error("Type error: Arguments in method call " + expr.meth.value +"("+ expr.args.map(x => tcExprFMD(x)).mkString(", ") +")" + " do not match any method definition", expr)
        None
      }
      else{
        selectBestRanking(anotatedMethodList, expr)
      }
    }
    
    def doArgTypesMatch(list1: List[VariableSymbol], list2: List[ExprTree]): Boolean  = {
        if (list1.isEmpty && list2.isEmpty) true
        else if (list1.isEmpty || list2.isEmpty) false
        else if (! (tcExprFMD(list2.head).isSubTypeOf(list1.head.getType))) false
        else doArgTypesMatch(list1.tail, list2.tail)
    }
    
    // tcExprFMM stands for For Multiple Dispatching, this will return the dynamic type of the expression tree
    // if the tree is an identifier we get its dynamic type, any other exprtrees are treated normally.
    def tcExprFMD(expr: ExprTree): Type = expr match {
      case id: Identifier => id.getSymbol match {
        case variableSymbol: VariableSymbol => variableSymbol.getDynamicType
        case _ => TError //Impossible
      }
      case _ => tcExpr(expr)
    }
    
    //Give ranking to method based on how far in the inheritance graph a method is of another
    def rankMethod(method: MethodSymbol, list2: List[ExprTree]): (MethodSymbol, Int) = {
      val ranking = method.argList.zip(list2). map(x => distanceInInheritance(x._1.getType ,tcExprFMD(x._2))).sum
      (method, ranking)
    }
    
    def distanceInInheritance(t1: Type, t2: Type): Int = {
      t1 match {
        case TObject(class1) => t2 match{
          case TObject(class2) => {
            if(class1 == class2) 0
            else 1 + distanceInInheritance(t1, class2.parent.get.getType)
          }
          case _ => 100 // Impossible
        }
        case _ => 0
      }
    }
    
    // Select the method with the best ranking(lower distance), gives an error if both methods have the same ranking
    def selectBestRanking(methodList: List[(MethodSymbol, Int)], expr: MethodCall): Option[MethodSymbol] = {
      // Find the best ranking
      val bestRanking = methodList.map(x => x._2).min
     
      // Select all the methods that have the best ranking
      val selectedMethods = methodList.filter(x => x._2 == bestRanking)
      
      // If there is more than one method with best ranking, error. If not, return the selected method.
      var errorMsg: String = ""
      val functionName = expr.meth.value
      if(selectedMethods.size > 1){
        errorMsg = "Ambigous call to " + functionName +"("+ expr.args.map(x => tcExprFMD(x)).mkString(", ") +")."
        errorMsg = errorMsg + "Possible candidates:\n" + selectedMethods.map(tuple => {
          "\t"+ functionName +"(" + tuple._1.argList.map(x => x.getType).mkString(", ") + ") in class " + tuple._1.classSymbol.name 
        }).mkString("\n")+ "."
        error("Type error: " + errorMsg, expr)
        None
      }
      else{
        Some(selectedMethods.head._1)
      }
      
    }
    

    def tcStatTree(statement: StatTree): Unit = {
      statement match {
        case stat: Block => {
          stat.stats.foreach(tcStatTree)
        }
        case stat: If => {
          tcExpr(stat.expr, TBoolean)
          tcStatTree(stat.thn)
          if(stat.els.isDefined) {
            tcStatTree(stat.els.get)
          }
        }
        case stat: While => {
          tcExpr(stat.expr, TBoolean)
          tcStatTree(stat.stat)
        }
        case stat: Println => {
          tcExpr(stat.expr, TBoolean, TInt, TString)
        }
        case stat: Assign => {
          tcExpr(stat.expr, stat.id.getType)
          
          // For multiple dispatch, bookkeeping of the dynamic type 
          stat.expr match {
            case expr: New => {
              val newType:Type = tcExpr(stat.expr)
              if(newType.isSubTypeOf(stat.id.getType)){ // Correct assignment of new
                stat.id.getSymbol match {
                  case variable:VariableSymbol => {
                    variable.setDynamicType(newType)
                    dynamicalVars = variable :: dynamicalVars
                  }
                  case _ => //Impossible
                }
              }
            }
            case _ => //When expr is not New, we don't care
          }
        }
        case stat: ArrayAssign => {
          tcExpr(stat.id, TIntArray)
          tcExpr(stat.index, TInt)
          tcExpr(stat.expr, TInt)
        }
        case _ => // Not possible
      }
    }
    
    def tcProgram(program: Program){
      tcMainObject(program.main)
      program.classes.foreach(tcClass)
    }
    
    def tcMainObject(main: MainObject){
      main.stats.foreach(tcStatTree)
    }
    
    def tcClass(_class: ClassDecl){
      _class.methods.foreach( x => {
        tcMethods(x)
        // Refresh class variables so that old dynamic assignments are not reachable from other methods
        dynamicalVars.foreach(x => x.setDynamicType(TUntyped))
      })
    }
    
    def tcMethods(method: MethodDecl){
      method.stats.foreach(tcStatTree)
      // This works, because the method symbols are assigned the return type as their own type
      tcExpr(method.retExpr, method.getSymbol.getType)
    }

    tcProgram(prog)
    terminateIfErrors
    prog
  }
}
