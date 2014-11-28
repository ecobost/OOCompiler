object MergeSort {
	def main() : Unit = {
		println(new MergeSorter().mergeSort(new Helper().initArray(16)));
	    println("finished");
	}


}

class MergeSorter {
    var bidule : Int;
	var array : Int[];

	def mergeSort(a: Int[]) : Int = {
      array = a;
      println("array to sort :");
      println(new Helper().arrayToString(array));
      bidule = this.recursive(0, array.length);
      println("sorted :");
      println(new Helper().arrayToString(array));

      return 0;
	}
	
	def recursive(begin : Int, a_length : Int) : Int = {

	   if(!(a_length == 1)){
	     bidule = this.recursive(begin, a_length/2);
	     bidule = this.recursive(begin + a_length/2, a_length - a_length/2);
	     bidule = this.merge(begin, begin + a_length/2 , begin + a_length);
	   }

	   return 0;
	}

	def merge(begin : Int, middle : Int, end : Int ) : Int = {
	    var right : Int;
	    var left : Int;
	    var temp : Int;
        var i : Int;
        var swp : Int;
        var swp_two : Int;

	    right = middle;
	    left = begin;


	    while( (left < middle) && (right < end)){
	        if(array[left] < array[right]){
	           left = left + 1;
	        }
	        else{
	            temp = array[right];

	            //move everything to the right
	            i = 1;
	            swp = array[left];
                while(i < (right - left + 1)){
                    swp_two = array[left + i];
                    array[left + i] = swp;
                    swp = swp_two;
                    i = i + 1;
                }

	            array[left] = temp;

	            left = left + 1;
	            middle = middle + 1;
	            right = right + 1;
	        }
	     }
	      return 0;
	}
}


class Helper {

   def initArray(sz : Int) : Int[] = {
          var array : Int[];
          var index : Int;
          var size : Int;
          size = sz;
          array = new Int[sz];

          index = 0;
          while(index < size){
             array[index] = sz;
             index = index + 1;
             sz = sz - 1;
          }
          return array;
       }

     def arrayToString(array : Int[]) : String = {
        var i : Int;
        var str : String;
        i = 0;
        str = "| ";

        while(i < array.length){
            str = str + array[i] + " | ";
            i = i + 1;
        }
        return str;
     }

}
