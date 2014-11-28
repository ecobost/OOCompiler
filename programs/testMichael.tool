object Test1 {
    def main(): Unit = {
        println(new Test2().test());
    }
}

class Test2 {
    def test(): String = {
        var a: A;
        
        a= new B();

        if (false) {
           a = new A();
        } else {
           a = new B(); 
        }

        return this.test1(a);
    }

    def test1(a1: A) : String = { return "test1 A"; }
    def test1(a1: B) : String = { return "test1 B"; }

}

class A {
	def foo():String = { return "A";}
}
class B extends A {
	def foo():String = { return "B";}
}