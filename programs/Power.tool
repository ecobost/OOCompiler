object Power{
	def main() : Unit = {
	println(new PowerCalcul().init(6,9));
	}
}

class PowerCalcul{
	var nb : Int;
	var pow : Int;

	def init(x : Int , y : Int) : Int ={
		nb=x;
		pow=y;
		return this.startCalc();
	}
	
	def startCalc() : Int = {
		var it : Int;
		var result:Int;
		println("Methode non-recursive");
		it=0;
		result=1;
		while(it<pow){
			result=nb*result;
			it=it+1;
		}
		println(result);
		println("Methode recursive");
		return this.calcRecurs(pow);
	}
	
	def calcRecurs(powRec : Int) : Int ={
		var result : Int;
		if(powRec==0)
			result=1;
		else{
			result = nb*this.calcRecurs(powRec-1);
		}
		return result;
	}
	
	

}
