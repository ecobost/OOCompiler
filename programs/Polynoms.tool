object Polynoms {
	def main() : Unit = {
		if(new Poly().start()){println("OK");}
	}
}

class Poly{
	def start() : Bool = {
		var len1 : Int;
		var len2 : Int;
		var poly1 : Int[];
		var poly2 : Int[];
		var polynom1 : Polynom;
		var polynom2 : Polynom;
		var i : Int;
		
		len1 = 4;
		len2 = 6;
		poly1 = new Int[len1];
		poly2 = new Int[len2];
		i = 0;
		
		while(i < len1){
			poly1[i] = i*3+6;
			i = i+1;
		}
		i = 0;
		while(i < len2){
			poly2[i] = i*2+2;	
			i = i+1;
		}
		
		polynom1 = new Polynom().init(poly1, len1);
		polynom2 = new Polynom().init(poly2, len2);
		
		println("polynom1: " + polynom1.toString());
		println("polynom2: " + polynom2.toString());
		println("polynom1 + polynom2 = " + polynom1.add(polynom2).toString());
		println("polynom1 * polynom2 = " + polynom1.multiply(polynom2).toString());
		return true;
	}
}

class Polynom {
	var len : Int;
	var numbers : Int[];
	def init(p : Int[], n : Int) : Polynom = {
		len = n;
		numbers = p;
		return this;
	}
	
	def toString() : String = {
		var i : Int;
		var text : String;
		text = "";
		i = 0;
		while(i < len) {
			if(i == 0) {
				text = text + numbers[i];
			} else {
				if(!(numbers[i]==0)) {
					text = text + " + " + numbers[i] + "x^" + i;
				}
			}
			i = i+1;
		}
		return text;
	}
	
	def add(p2 : Polynom) : Polynom = {
		var poly2 : Int[];
		var length2 : Int;	
		var j : Int;
		var resultLength : Int;
		var result : Int[];
		poly2 = p2.getNumbers();
		length2 = p2.getLength();
		j = 0;
		
		if(length2 < len) {
			resultLength = len;
		} else {
			resultLength = length2;
		}
		result = new Int[resultLength];
		while(j < resultLength) {
			if((j < len) && (j < length2)) {
				result[j] = numbers[j] + poly2[j];
			}
			if((j < len) && !(j < length2)) {
				result[j] = numbers[j];
			}
			if(!(j < len) && (j < length2)) {
				result[j] = poly2[j];
			}
			j = j+1;
		}
		return new Polynom().init(result, resultLength);
	}
	
	def multiply(p2 : Polynom) : Polynom = {
		var poly2 : Int[];
		var length2 : Int;	
		var k : Int;
		var l : Int;
		var resultLength : Int;
		var result : Int[];
		poly2 = p2.getNumbers();
		length2 = p2.getLength();
		
		resultLength = ((len-1)*(length2-1))+1;
		result = new Int[resultLength];
		k = 0;
		l = 0;
		while(k < resultLength) {
			result[k] = 0;
			k = k+1;
		}
		
		k = 0;
		
		while(k < len) {
			while(l < length2) {
				result[k+l] = result[k+l] + (poly2[l]*numbers[k]);
				l = l+1;
			}
			l = 0;
			k = k+1;
		}
		
		return new Polynom().init(result, resultLength);
	}
	
	def getNumbers() : Int[] = {
		return numbers;
	}
	
	def getLength() : Int = {
		return len;
	}
}