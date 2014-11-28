object Main {
	def main(): Unit = {
		println(new A().foo());
	}
}
class A {
	def foo(): Int = { return 1;} 
	def foo(): String  = {return "foo 2";}
}

// Output: ERROR!