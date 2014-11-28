object LinearSearch {
    def main(): Unit = {
        println(new LS().start(20));
    }
}


class LS {
	var intArray : Int[];
	var sizeArray : Int;
	
	def start(size : Int) : Int = {
		var auxCall : Int;
		
		auxCall = this.init(size);
		
		println("Array of numbers where we search:");
		auxCall = this.printArray();
		
		
		println("-----------------");
		if(this.search(3)) {
			println("Number 3 IS in the array!");
		}else{
			println("Number 3 IS NOT in the array!");
		}
		
		if(this.search(6)) {
			println("Number 6 IS in the array!");
		}else{
			println("Number 6 IS NOT in the array!");
		}
		
		if(this.search(14)) {
			println("Number 14 IS in the array!");
		}else{
			println("Number 14 IS NOT in the array!");
		}
		
		if(this.search(21)) {
			println("Number 21 IS in the array!");
		}else{
			println("Number 21 IS NOT in the array!");
		}
		
		if(this.search(41)) {
			println("Number 41 IS in the array!");
		}else{
			println("Number 41 IS NOT in the array!");
		}
		
		println("-----------------");
		
		return 0;
	}
	
	def printArray() : Int = {
		var i : Int;
		
		i = 1;
		while(i < sizeArray) {
			println(intArray[i]);
			i = i + 1;
		}
		
		return 0;		
	}
	
	def init(size : Int) : Int = {
		var i : Int;
		var aux : Int;
		
		sizeArray = size;
		intArray = new Int[size];
		
		aux = 1;
		i = 1;
		while(i < size) {
			aux = aux + i*size;
		    intArray[i] = aux;
			i = i + 1;
		}	
		
		return 0;
	}
	
	def search(number : Int) : Bool = {
		var i : Int;
		var found : Bool;
		
		found = false;
		i = 1;
		while(i < sizeArray && !found) {
			if(intArray[i] == number) {
				found = true;
			}
			i = i + 1;
		}
		
		return found;
  }
}
	
