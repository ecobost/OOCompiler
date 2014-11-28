// Prints a Pascal triangle


object PascalTriangle {
  def main(): Unit = {
    println(new PascalT().Start(10));
  }
}

class PascalT {
  var line1: Int[];
  var line2: Int[];
  var prout: Int;
  
  def Start(iter: Int): String = {
    println("Strated");
    prout = this.Init(iter);
    prout = this.PrintPascal(line1, line2, 0, iter);
	return "Done!";
  }
  
  def PrintPascal(line1: Int[], line2: Int[], currIter: Int, maxIter: Int): Int = {
  
	var i: Int;
	
    if(currIter<maxIter){
	  i = 0;
      while(i < currIter){
	    if(i == 0) {
		  line2[i] = 1;
		}
		else{
		  line2[i] = line1[i-1]+line1[i];
		}
		i = i+1;
      }
      prout = this.PrintTab(line2);
      prout = this.PrintPascal(line2, line1, currIter+1, maxIter);
	  
    }
	return prout;
  }
  
  def Init(size: Int): Int = {
    var i: Int;
	i = 1;
    line1 = new Int[size];
    line2 = new Int[size];
    line1[0] = 1;
    while(i < size) {
      line1[i] = 0;
	  i = i+1;
    }
	return 0;
  }
  
  def PrintTab(tab: Int[]): Int = {
    var i: Int;
    var toPrint: String;
	i = 0;
	toPrint = "";
    while(i < tab.length) {
      if(!(tab[i] == 0)) {
        toPrint = toPrint + tab[i] + " ";
      }
	  i = i+1;
    }
    println(toPrint);
	return 0;
  }
}