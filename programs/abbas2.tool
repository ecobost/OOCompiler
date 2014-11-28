object HofstadterSequence {
    def main() : Unit = {
        println(new HS().Start());
    }
}

class HS {
	//start of program
	def Start() : Int = {
		var tmp : Bool;
		var list1 : Int [];
		var list2 : Int [];
		var counter : Int;
		var total : Int;
		var i : Int;
		var j : Int;
		var check : Int;
		var length1 : Int;
		var length2 : Int;
		
		counter = 0;
		list1 = new Int[20];
		list2 = new Int[20];
		j = 2;
		check = 1;

		
		println ("");
		println ("This program computes the first 20 numbers in Hofstadter Sequence");
		println ("");
		println ("--------------------------------------------------------------------");
		println ("");
		println ("");
		
		list1[0] = 1;
		list2[0] = 2;
		length1 = 1;
		length2 = 1;
		counter = 1;
		
		while(counter < 20)
		{
			
			total = list1[length1 - 1] + list2[length2 - 1];
			list1[counter] = total;
			length1 = length1 + 1;
			i = j;
			check = 1;
			while(check == 1)
			{
				if(!this.ArrayContains(list1,length1,i))
				{
					if(!this.ArrayContains(list2,length2,i))
					{
						list2[counter] = i;
						length2 = length2 + 1;
						i = j;
						check = 0;
					}
				}
				i = i + 1;
			}
			counter = counter + 1;
		}
		tmp = this.PrintList(list1, length1);
		return 0;
	}
	
	//checking if array contains an element
	def ArrayContains (arr : Int [],len : Int, elem : Int ) : Bool = {
		var result : Bool;
		var check : Int;
		
		check  = 0;
		result = false;
		while (check < len)
		{
			if(arr[check] == elem)
			{	
				result = true;
			}
			check = check + 1;
		}
		return result; 
	}
	
	//printing a list
	def PrintList (array : Int[], len : Int) : Bool = {
		var counter : Int;
		var res : Bool;
		
		counter = 0;
		res = true;
		
		while ( counter < len - 1)
		{
			println(array[counter]);
			counter = counter + 1;
		}
		return res;
	}
}
