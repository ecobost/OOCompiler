object Fibonacci {
    def main() : Unit = {
        println( new FibonacciPrint().start(25) );
    }
}

class FibonacciPrint {

      def recursive( count : Int) : Int = {
           var i : Int;
           i = 0;
           if((count < 0) || (count == 0) ){
                 i = 1;
           }
           else{
                 i =  this.recursive(count - 1) + this.recursive(count - 2);
           }
           return i;
      }

      def printNum(i : Int) : Int ={
        println(i);
        return i;
      }

      def start(end : Int) : Int = {
        var titi : Int;
        titi = this.recursive(end);
        println(titi);
        return 0;
      }
}