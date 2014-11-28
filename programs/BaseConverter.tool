/** 
  * Program: BaseConverter.tool
  * Purpose: Converts a number from base 'a' to base 'b'. Does not work with bases >10
  * Usage: new BaseConverterComputer().Convert(number, base_a, base_b)
  * 
  * Written by Loic Habegger (Group ForestAnimals, SCIPER 195804)
  *
  **/

object BaseConverter {
	def main(): Unit = {
		println(new BaseConverterComputer().Example());
    }
}

class BaseConverterComputer {	// We need this because the online interpreter testing this file while refuse multiple println in main method and \n is not correctly parsed
	def Example(): String = {
		println("Example :");
		println(new BaseConverterComputer().Convert(42, 10, 3));
		println(new BaseConverterComputer().Convert(42, 10, 2));
		println(new BaseConverterComputer().Convert(1120, 3, 10));
		println(new BaseConverterComputer().Convert(101010, 2, 10));
		println(new BaseConverterComputer().Convert(1120, 3, 2));
		return "End of example";
	}

	def Convert(number : Int, from : Int, to : Int): String = {
		var temp : Int;
		temp = number;
		if (!(from == 10)) {
			number = this.TenFrom(number, from);
		}
		return temp + " from base " + from + " to base " + to + " is " + this.TenTo(number, to);	
	}
	
	def TenTo(number : Int, to : Int): String = {
		var result : String;
		var div : Int;
		var mod : Int;
		
		result = "";
		while (!(number == 0)) {
			div = number/to;
			mod = number - to*div;
			number = div;	
			result = mod + result;
		}
		return result;
	}
	
	def TenFrom(number : Int, from : Int): Int = {
		var div : Int;
		var mod : Int;
		var i : Int;
		var result : Int;
		
		i = 0;
		result = 0;
		while (!(number == 0)) {
			div = number/10;
			mod = number - 10*div;
			number = div;
			result = result + mod*this.Power(from, i);
			i = i + 1;
		}
		return result;
	}
	
	def Power(base : Int, power : Int): Int = {
		var i : Int;
		var result : Int;
		
		i = 0;
		result = 1;
		while (!(i == power)) {
			result = result*base;
			i = i + 1;
		}
		return result;
	}
}