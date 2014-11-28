object MinMaxArray{
	def main() : Unit = {	
		println(new FindMinMax().initArray(6).getMinMax());
	}
}

class FindMinMax{
	var numbers : Int[];


	def initArray(dimension : Int) : FindMinMax = {
		if( this.mod(dimension,2) == 0){
			numbers = new Int[dimension+1]; //Ajout place sentinel		
		}
		
		numbers[0]=3;
		numbers[1]=7;
		numbers[2]=12;
		numbers[3]=4;
		numbers[4]=0;
		numbers[5]=2;
		
		if( !(dimension == numbers.length)){
			numbers[numbers.length-1]=numbers[0];
		}
		
		return this;
		
	}
	
	
	def getMinMax() : String = {
		var minimum : Int; 
		var maximum : Int; 
		var it: Int; 
		
		minimum=numbers[0];
		maximum=minimum;
		
		it=1;
		
		while(it<numbers.length){
			if(numbers[it]<numbers[it+1]){	
				if(numbers[it]<minimum){
					minimum=numbers[it];
				}
				if(maximum < numbers[it+1]){
					maximum=numbers[it+1];
				} 
			}
			else{
				if(numbers[it+1]<minimum){
					minimum=numbers[it+1];
				}
				if(maximum < numbers[it]){
					maximum=numbers[it];
				} 
			}		
		it=it+2;
		}
		
		return "minimum : "+minimum+" "+"maximum :"+" "+maximum;
	}
	
	def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
	
}