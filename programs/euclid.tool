/* This program computes the PGDC of two integers using Euclid algorithm.
*/
/* Author : Thiébaud Modoux, 2013 */

/* Print the PGDC of 25 and 255 */
object Euclid {
    def main() : Unit = {
        println(new EuclidPGDC().compute(25,555));
    }
}

class EuclidPGDC {
    def compute(a : Int, b : Int) : Int = {

		var r : Int;
		
		println("PGDC of " +a+ " and " +b+ " : ");
		
		/* Successive Euclidian divisions */
		while(!(b==0)){
		r = this.modulo(a,b);
		a = b;
		b = r;
		}
		
        return a;
    }
	
	/* Modulo operator implementation */
	def modulo(n: Int, mod:Int) : Int = {
    	return (n - ((n / mod) * mod));
    }
}