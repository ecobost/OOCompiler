object Main {
	def main(): Unit = {
		println(new A().foo());
	}
}
class A {
	def foo(): String = { return "foo 1";} 
	def foo(a: Int): String  = {return "foo 2";}
}

// Output: foo 1