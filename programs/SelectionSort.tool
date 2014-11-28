/** 
  * Program: SelectionSort.tool
  * Purpose: Sort an array using the Selection Sort Algorithm
  * Usage: new SelectionSortComputer().Sort()
  * 
  * Written by Loic Habegger (Group ForestAnimals, SCIPER 195804)
  *
  **/

object SelectionSort {
	def main(): Unit = {
        println(new SelectionSortComputer().Sort());
    }
}

class SelectionSortComputer {
	var table : Int[];

	def Sort(): String = {
		var i : Int;
		var j : Int;
		var position : Int;
		var buffer : Int;
		
		table = new Int[10];
		table[0] = 0;
		table[1] = 2;
		table[2] = 3;
		table[3] = 4;
		table[4] = 9;
		table[5] = 7;
		table[6] = 1;
		table[7] = 8;
		table[8] = 6;
		table[9] = 5;
		println("Before: " + this.Serialize());
		i = 0;
		while (i < table.length) {
			position = i;
			j = i+1;
			while (j < table.length) {
				if (table[j] < table[position]) {
					position = j;
				}
				j = j + 1;
			}
			if (!(position == i)) {
				buffer = table[i];
				table[i] = table[position];
				table[position] = buffer;
			}
			i = i + 1;
		}
		return "After: " + this.Serialize();
	}
	
	def Serialize(): String = {
		var i : Int;
		var result : String;
		
		i = 0;
		result = "";
		while (i < table.length) {
			result = result + table[i] + " ";
			i = i + 1;
		}
		return result;
	}
}