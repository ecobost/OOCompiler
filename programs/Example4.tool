object Main {
	def main(): Unit = {
		println(new A().foo( new SubSub(), 2));
	}
}
class A {
	def foo(b: Base, a: Int): String = { return "foo 1";} 
	def foo(s: Sub, a: Int): String = { return "foo 2";}
}

class Base {}
class Sub extends Base {}
class SubSub extends Sub {}

// Output: foo 2