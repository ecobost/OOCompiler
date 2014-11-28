object BubbleSort {
	def main() : Unit = {
		println(new BubbleSorter().Sort());
	}
}


class BubbleSorter {
	var array : Int[];
	var size : Int;
	var  bidule : Int;
	
	def Sort() : String = {		
		bidule = this.Init(10);
		println("input is : ");
		bidule = this.PrintArray();
		println("sorting...");
		bidule = this.SortArray();	
		bidule = this.PrintArray();
		return "finished";	
	}
		
	def SortArray() : Int = {
		var i : Int;
		var j : Int;
		i = 0;
		j = 0;
		
		while( i < size - 1){
			while( j < size - i - 1){
				
				if(array[j] < array[j + 1]){
					bidule = array[j];
					array[j] = array[j+ 1];
					array[j + 1] = bidule;
				}
				j = j + 1;
			}
			j = 0;
			i = i + 1;
		}
		return 0;
	}
	
	
	def Init(sz : Int) : Int = {
        size = sz ;
        array = new Int[size] ;

        array[0] = 20 ;
        array[1] = 7  ; 
        array[2] = 12 ;
        array[3] = 18 ;
        array[4] = 2  ; 
        array[5] = 11 ;
        array[6] = 6  ; 
        array[7] = 9  ; 
        array[8] = 19 ; 
        array[9] = 5  ;

        return 0 ;  
    }
	
	def PrintArray() : Int	= {
		bidule = 0;
		while(bidule < size){
			println(array[bidule]);
			bidule = bidule + 1;
		}
		return 0;
	}
}