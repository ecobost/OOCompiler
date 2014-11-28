object Main {
	def main(): Unit = {
		println(new A().foo());
	}
}
class A {
	def foo(a: Int): Int = { return 1;} 
	def foo(): String  = {return "foo 2";}
}

// Output: foo 2