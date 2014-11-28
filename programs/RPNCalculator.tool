/* Weird object-oriented RPN Calculator: Implement linked-list Stack
   with Stackable Objects to test inheritance & method overriding */
   
object RPNCalculator {
	def main(): Unit = {
		if(new Calculator().test()) {
			println("true");
		}
	}
}

class Calculator {
	def test(): Bool = {
		var st: Stack;
		var dummy: Stackable;
		
		st = new Stack().init();
		st = st.show();
		println("Push 3 then push 5");
		st = st.push(new Integer().init(3));
		st = st.show();
		st = st.push(new Integer().init(5));
		st = st.show();
		
		println("Pop");
		dummy = st.pop();
		st = st.show();
		
		println("Pop");
		dummy = st.pop();
		st = st.show();
		
		println("Push 7 then push 4");
		st = st.push(new Integer().init(7));
		st = st.push(new Integer().init(4));
		st = st.show();
		
		println("Push +");
		st = st.push(new Operation().init(st.plusCode()));
		st = st.show();
		
		println("Computes 2 * (5+2) - 18/(2+1) = 8");
		println("In RPN => 5 2 + 2 * 18 2 1 + / -");
		
		st = st.push(new Integer().init(5));
		st = st.show();
		st = st.push(new Integer().init(2));
		st = st.show();
		st = st.push(new Operation().init(st.plusCode()));
		st = st.show();
		st = st.push(new Integer().init(2));
		st = st.show();
		st = st.push(new Operation().init(st.timesCode()));
		st = st.show();
		st = st.push(new Integer().init(18));
		st = st.show();
		st = st.push(new Integer().init(2));
		st = st.show();
		st = st.push(new Integer().init(1));
		st = st.show();
		st = st.push(new Operation().init(st.plusCode()));
		st = st.show();
		st = st.push(new Operation().init(st.dividesCode()));
		st = st.show();
		st = st.push(new Operation().init(st.minusCode()));
		st = st.show();
		
		println("Trying to pop an operation to test method inheritance (will output error):");
		dummy = new Operation().init(st.plusCode()).popYourselfOutOfStack(st);
		
		return true;
	}
}

class Stack {
	
	var top: Stackable;
	var isEmpty: Bool;

	def init(): Stack = {
		isEmpty = true;
		return this;
	}

	def push(elem: Stackable): Stack = {
		elem = elem.pushYourselfOnStack(this);
		return this;
	}
	
	def pop(): Stackable = {
		return top.popYourselfOutOfStack(this);
	}
	
	def getTop(): Stackable = {
		return top;
	}
	
	def setTop(newTop: Stackable): Stack = {
		top = newTop;
		return this;
	}
	
	def isEmpty(): Bool = {
		return isEmpty;
	}
	
	def setEmpty(empty: Bool): Stack = {
		isEmpty = empty;
		return this;
	}
	
	def show(): Stack = {
		if(isEmpty) {
			println("Stack Empty ");
		}
		else {
			println("Stack = " + top.toString());
		}
		return this;
	}
	
	def plusCode(): Int = {
		return 0;
	}
	
	def minusCode(): Int = {
		return 1;
	}
	
	def timesCode(): Int = {
		return 2;
	}
	
	def dividesCode(): Int = {
		return 3;
	}
}

class Stackable {

	def value(): Int = {
		println("Generic Stackable's value must not be used.");
		return 0;
	}
	
	def pushYourselfOnStack(st: Stack): Stackable = {
		println("Generic Stackable must not be used.");
		return this;
	}
	
	def popYourselfOutOfStack(st: Stack): Stackable = {
		println("Generic Stackable must not be used to pop.");
		return this;
	}
	
	def toString(): String = {
		return "Generic Stackable must not be used.";
	}
}

class Integer extends Stackable {
	var value: Int;
	var isBottom: Bool;
	var previous: Stackable;
	
	def init(initValue: Int): Integer = {
		value = initValue;
		return this;
	}
	
	def value(): Int =  {
		return value;
	}
	
	def pushYourselfOnStack(st: Stack): Stackable = {
		if(st.isEmpty()) {
			isBottom = true;
			st = st.setEmpty(false);
		}
		else {
			previous = st.getTop();
			isBottom = false;
		}
		st = st.setTop(this);
		return this;
	}
	
	def popYourselfOutOfStack(st: Stack): Stackable = {
		if(isBottom) {
			st = st.setEmpty(true);
		}
		else {
			st = st.setTop(previous);
		}
		return this;
	}
	
	def toString(): String = {
		var res: String;
		if(isBottom) {
			res = value + "";
		}
		else {
			res = previous.toString() + ", " + value ;
		}
		
		return res;
	}
}

class Operation extends Stackable {
	
	var opCode: Int;
	
	def init(operationCode: Int): Operation = {
		opCode = operationCode;
		return this;
	}
	
	def pushYourselfOnStack(st: Stack): Stackable = {
		var top: Stackable;
		var belowTop: Stackable;
		
		top = st.pop();
		belowTop = st.pop();
		
		if(opCode == st.plusCode()) {
			st = st.push(new Integer().init(top.value() + belowTop.value()));
		}
		
		if(opCode == st.minusCode()) {
			st = st.push(new Integer().init(belowTop.value() - top.value()));
		}
		
		if(opCode == st.timesCode()) {
			st = st.push(new Integer().init(top.value() * belowTop.value()));
		}
		
		if(opCode == st.dividesCode()) {
			st = st.push(new Integer().init(belowTop.value() / top.value()));
		}
		
		return this;
	}
}