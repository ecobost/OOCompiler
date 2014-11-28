/*
 * This program computes the value of the n-th hyperoperation function given
 * n and two integers a and b
 * Here called HypOp(n, a, b)
 *
 * For more details about hyperoperations: http://en.wikipedia.org/wiki/Hyperoperation
 *
 * Level 0 is simply the successor: f0(a, b) = b+1
 * Level 1 is simply addition:		f1(a, b) = a+b
 * Level 2 is simply multipli.:		f2(a, b) = a*b
 * Level 3 is simply exponenti.:		f3(a, b) = a^b
 * Level 4 is .. tetration
 * Level 5 is .. pentation .. and closer to stackoverflow
 */
 
 object hyperOperation {
 	def main(): Unit = {
 		{
 			println("(a,b) = (3, 2)");
	 		println("n = 0: "+new H().HypOp(0, 3, 2));
	 		println("n = 1: "+new H().HypOp(1, 3, 2));
	 		println("n = 2: "+new H().HypOp(2, 3, 2));
	 		println("n = 3: "+new H().HypOp(3, 3, 2));
	 		println("n = 4: "+new H().HypOp(4, 3, 2));
	 		println("n = 5: StackOverflow? :__D");
	 		
 			println("(a,b) = (3, 0)");
	 		println("n = 0: "+new H().HypOp(0, 3, 0));
	 		println("n = 1: "+new H().HypOp(1, 3, 0));
	 		println("n = 2: "+new H().HypOp(2, 3, 0));
	 		println("n = 3: "+new H().HypOp(3, 3, 0));
	 		
 			println("(a,b) = (0, 4)");
	 		println("n = 0: "+new H().HypOp(0, 0, 4));
	 		println("n = 1: "+new H().HypOp(1, 0, 4));
	 		println("n = 2: "+new H().HypOp(2, 0, 4));
	 		println("n = 3: "+new H().HypOp(3, 0, 4));
	 		
 			println("(a,b) = (5, 4)");
	 		println("n = 0: "+new H().HypOp(0, 5, 4));
	 		println("n = 1: "+new H().HypOp(1, 5, 4));
	 		println("n = 2: "+new H().HypOp(2, 5, 4));
	 		println("n = 3: "+new H().HypOp(3, 5, 4));
	 	}
 	}
 }
 
 class H {
 	/*
 	 * Compute (with recursion) the n-th HyperOperation function on (a, b)
 	 */
 	def HypOp(n: Int, a: Int, b: Int): Int = {
 		var nextNb : Int;
 		
 		// Base Cases & Recursion
 		if(n == 0) {
 			nextNb = b + 1; // Successor function of b
 		} else if(n == 1 && b == 0) {
 			nextNb = a; // Simply a+b == a+0 == a
 		} else if(n == 2 && b == 0) {
 			nextNb = 0; // Simply a*b == a*0 == 0
 		} else if(b == 0) {
 			nextNb = 1; // Ex with n == 3: a^b == a^0 == 1
 		} else {
 			/*
 			 * Ex with (n, a, b) = (2, a, b) == a*b = a + a + .. + a (and this b times)
 			 */
 			nextNb = this.HypOp(n-1, a, this.HypOp(n, a, b-1));
 		}
 		
 		return nextNb;
 	}
 }
 
 // End :)
