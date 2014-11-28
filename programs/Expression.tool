object Expression {
   def main() : Unit = {
      if(new Runner().run()) {}
   }
}

class Runner {
   def run() : Bool = {
      var evaluator : Visitor;
      var expression : Expr;
      evaluator = new Evaluator().init();
      expression = new Plus().init(new Const().init(1), new Const().init(1));
      if(expression.accept(evaluator) && evaluator.extract()) {}
      return false;
   }
}

class Expr {
   var const : Int;
   var left : Expr;
   var right : Expr;
   def accept(visitor : Visitor) : Bool = {return false;}
}

class Null extends Expr {}

class Const extends Expr {
   def init(value : Int) : Expr = {
      const = value;
      return this;
   }
   def accept(visitor : Visitor) : Bool = {
      if(visitor.visit(0, const, new Null(), new Null())) {}
      return true;
   }
}

class Plus extends Expr {
   def init(l : Expr, r : Expr) : Expr = {
      left = l;
      right = r;
      return this;
   }
   def accept(visitor : Visitor) : Bool = {
      if(visitor.visit(1, 0, left, right)) {}
      return true;
   }
}

class Visitor {
   def visit(op : Int, val : Int, left : Expr, right : Expr) : Bool = {return false;}
   def extract() : Bool = {return false;}
}

class Evaluator extends Visitor {
   var resultStack : IntStack;
   def init() : Visitor = {resultStack = new EmptyIntStack(); return this;}
   def visit(op : Int, val : Int, left : Expr, right : Expr) : Bool = {
      var res : Int;
      if(op == 0) {
         resultStack = new NonEmptyIntStack().init(val, resultStack);
      }
      if(op == 1) {
         if(left.accept(this) && right.accept(this)) {}
         res = resultStack.head() + resultStack.tail().head();
         resultStack = resultStack.tail().tail();
         resultStack = new NonEmptyIntStack().init(res, resultStack);
      }
      return true;
   }
   def extract() : Bool = {
      println("The value of the expression is " + resultStack.head());
      return true;
   }
}

class IntStack {
   var val : Int;
   var tail : IntStack;
   def isEmpty() : Bool = {return false;}
   def head() : Int = {return val;}
   def tail() : IntStack = {return tail;}
}

class EmptyIntStack extends IntStack {
   def isEmpty() : Bool = {return true;}
}

class NonEmptyIntStack extends IntStack {
   def init(v : Int, t : IntStack) : IntStack = {val = v; tail = t; return this;}
   def isEmpty() : Bool = {return false;}
}
