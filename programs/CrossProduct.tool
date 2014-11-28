object Cross {
	def main() : Unit = {
		println(new CrossProduct().computeProduct(1, 2, 3, 4, 5, 6));
	}
}

class CrossProduct {
	def computeProduct(x1: Int, x2: Int, x3: Int, y1: Int, y2: Int, y3: Int) : String = {
		var z1 : Int;
		var z2 : Int;
		var z3 : Int;
		z1 =  x2 * y3 - x3 * y2;
		z2 = x3 * y1 - x1 * y3;
		z3 = x1 * y2 - x2 * y1;
		return "(" + x1 + ", " + x2 + ", " + x3 + ") x (" + y1 + ", " + y2 + ", " + y3 + ") = (" + z1 + ", " + z2 + ", " + z3 + ")"; 
	}
}