package toolc
package ast

import Trees._

object Printer {
  var indexation: Int = 0; // Amount of tabs before the lines

  def apply(t: Tree): String = {
    t match {
      case t: Program => printProgram(t)
      case _ => "ERROR: At printing, bad formed Program. Expected: Program, found " + t
    }
  }

  def printProgram(t: Program): String = {
    var s: String = ""
    s += printMainObject(t.main)
    s += printEnter
    s += printEnter
    s += t.classes.map(printClassDecl).mkString(printEnter + printEnter)
    s
  }

  def printMainObject(t: MainObject): String = {
    var s: String = ""
    s += "object " + printIdentifier(t.id) + " {"
    indexation += 1
    s += printEnter
    s += "def main(): Unit = {"
    indexation += 1
    s += printEnter
    s += t.stats.map(printStatTree).mkString(printEnter)
    indexation -= 1
    s += printEnter
    s += "}"
    indexation -= 1
    s += printEnter
    s += "}"
    s
  }

  def printIdentifier(t: Identifier): String = {
    t.value + "#" + t.getSymbol.id
  }

  def printStatTree(t: StatTree): String = {
    t match {
      case stat: Block => printBlock(stat)
      case stat: If => printIf(stat)
      case stat: While => printWhile(stat)
      case stat: Println => printPrintln(stat)
      case stat: Assign => printAssign(stat)
      case stat: ArrayAssign => printArrayAssign(stat)
      case _ => "ERROR: Invalid Statement" //Impossible
    }
  }

  def printBlock(t: Block): String = {
    var s: String = "{"
    indexation += 1
    s += printEnter
    s += t.stats.map(printStatTree).mkString(printEnter)
    indexation -= 1
    s += printEnter
    s += "}"
    s
  }

  def printIf(t: If): String = {
    var s: String = ""
    s += "if(" + printExprTree(t.expr) + ") "
    s += printBlockOrStatTreeAsBlock(t.thn)
    if (t.els.isDefined) {
      s += printEnter
      s += "else "
      s += printBlockOrStatTreeAsBlock(t.els.get)
    }
    s
  }

  def printBlockOrStatTreeAsBlock(t: StatTree): String = {
    var s = ""
    t match {
      case stat: Block => s += printBlock(stat)
      case _ =>
        s += "{"
        indexation += 1
        s += printEnter
        s += printStatTree(t)
        indexation -= 1
        s += printEnter
        s += "}"
    }
    s
  }

  def printWhile(t: While): String = {
    var s: String = ""
    s += "while(" + printExprTree(t.expr) + ") "
    s += printBlockOrStatTreeAsBlock(t.stat)
    s
  }

  def printPrintln(t: Println): String = {
    var s: String = ""
    s += "println(" + printExprTree(t.expr) + ");"
    s
  }

  def printAssign(t: Assign): String = {
    var s: String = ""
    s += printIdentifier(t.id) + " = " + printExprTree(t.expr) + ";"
    s
  }

  def printArrayAssign(t: ArrayAssign): String = {
    var s: String = ""
    s += printIdentifier(t.id)
    s += "[" + printExprTree(t.index) + "] = "
    s += printExprTree(t.expr) + ";"
    s
  }

  def printExprTree(t: ExprTree): String = {
    t match {
      case expr: And =>
        "(" + printExprTree(expr.lhs) + " && " + printExprTree(expr.rhs) + ")"
      case expr: Or =>
        "(" + printExprTree(expr.lhs) + " || " + printExprTree(expr.rhs) + ")"
      case expr: Equals =>
        "(" + printExprTree(expr.lhs) + " == " + printExprTree(expr.rhs) + ")"
      case expr: LessThan =>
        "(" + printExprTree(expr.lhs) + " < " + printExprTree(expr.rhs) + ")"
      case expr: Plus =>
        "(" + printExprTree(expr.lhs) + " + " + printExprTree(expr.rhs) + ")"
      case expr: Minus =>
        "(" + printExprTree(expr.lhs) + " - " + printExprTree(expr.rhs) + ")"
      case expr: Times =>
        "(" + printExprTree(expr.lhs) + " * " + printExprTree(expr.rhs) + ")"
      case expr: Div =>
        "(" + printExprTree(expr.lhs) + " / " + printExprTree(expr.rhs) + ")"
      case expr: ArrayRead =>
        printExprTree(expr.arr) + "[" + printExprTree(expr.index) + "]"
      case expr: ArrayLength =>
        printExprTree(expr.arr) + ".length"
      case expr: MethodCall =>
        printExprTree(expr.obj) + "." + printIdentifier(expr.meth) + "(" + expr.args.map(printExprTree).mkString(", ") + ")"
      case expr: IntLit =>
        expr.value.toString
      case expr: StringLit =>
        "\"" + expr.value + "\""
      case expr: True =>
        "true"
      case expr: False =>
        "false"
      case expr: Identifier =>
        printIdentifier(expr)
      case expr: This =>
        "this" + "#" + expr.getSymbol.id 
      case expr: NewIntArray =>
        "new Int [" + printExprTree(expr.size) + "]"
      case expr: New =>
        "new " + printIdentifier(expr.tpe) + "()"
      case expr: Not =>
        "!" + printExprTree(expr.expr)
      case _ => "ERROR: Invalid expression" //Impossible
    }
  }

  def printClassDecl(t: ClassDecl): String = {
    var s: String = ""
    s += "class " + printIdentifier(t.id) + " "
    if (t.parent.isDefined) {
      s += "extends " + printIdentifier(t.parent.get) + " "
    }
    s += "{"
    indexation += 1
    s += printEnter
    s += t.vars.map(printVarDecl).mkString(printEnter)
    s += printEnter
    s += printEnter
    s += t.methods.map(printMethodDecl).mkString(printEnter + printEnter)
    s += printEnter //Optional
    indexation -= 1
    s += printEnter
    s += "}"
    s
  }

  def printVarDecl(t: VarDecl): String = {
    "var " + printIdentifier(t.id) + ": " + printType(t.tpe) + ";"
  }

  def printType(t: TypeTree): String = {
    t match {
      case typ: IntArrayType => "Int []"
      case typ: BooleanType => "Bool"
      case typ: IntType => "Int"
      case typ: StringType => "String"
      case typ: Identifier => printIdentifier(typ)  
      case _ => "ERROR: Invalid type" //Impossible
    }
  }

  def printMethodDecl(t: MethodDecl): String = {
    var s: String = ""
    s += "def " + printIdentifier(t.id) + "("
    s += t.args.map(printFormal).mkString(", ")
    s += "): "
    s += printType(t.retType)
    s += " = {"
    indexation += 1
    s += printEnter
    s += t.vars.map(printVarDecl).mkString(printEnter)
    s += printEnter
    s += printEnter
    s += t.stats.map(printStatTree).mkString(printEnter)
    s += printEnter
    s += printEnter
    s += "return " + printExprTree(t.retExpr) + ";"
    indexation -= 1
    s += printEnter
    s += "}"
    s
  }

  def printFormal(t: Formal): String = {
    printIdentifier(t.id) + ": " + printType(t.tpe)
  }

  def printEnter: String = {
    "\n" + printIndexation
  }

  def printIndexation: String = {
    var s: String = ""
    var i = 0
    for (i <- 1 to indexation) {
      s += "\t"
    }
    s
  }
}
