object PolynomialResolver2 {
    def main() : Unit = {
        println(new PolyRes2().computeRoots(1, 4, 3)[0]);        
    }
}

class PolyRes2 {
    def computeRoots(a : Int, b : Int, c : Int) : Int[] = {
    	var delta : Int;
    	var sqrtDelta : Int;
    	var returnVal : Int[];
    	var x_1 : Int;
    	var x_2 : Int;
    	var i : Int;
    	
    	delta = b*b - 4*a*c;
    	sqrtDelta = 0;
    	
    	i = 1;
    	while (i < delta) {
    		if ((i * i) == delta) {
    			sqrtDelta = i;
    		}
    		i = i + 1;
    	}
    	
    	x_1 = 0;
    	x_2 = 0;
    	if (0 < delta) {
    		x_1 = (((0 - b) - sqrtDelta) / (2*a));
    		x_2 = (((0 - b) + sqrtDelta) / (2*a));
    	}
    	
    	returnVal = new Int[2];
    	returnVal[0] = x_1;
    	returnVal[1] = x_2;
        return returnVal;
    }
}
