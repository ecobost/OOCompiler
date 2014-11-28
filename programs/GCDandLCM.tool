object GCDandLCM {
    def main(): Unit = {
	if(new gcdlcm().init(144, 60)) {
	    println("Done!");	
	}
    }
}

class gcdlcm {
    def init(n1: Int, n2: Int): Bool = {
	var gcd : Int;
	var lcm : Int;

	if (n2 < n1) {
	    gcd = this.calcul(n1,n2);
	} else {
	    gcd = this.calcul(n2,n1);
	}
	
	lcm=n1*n2/gcd;
	
	println("The GCD of "+n1+" and "+n2+" is "+gcd);
	println("The LCM of "+n1+" and "+n2+" is "+lcm);
	return true;
    }

    def calcul(n1 : Int, n2: Int): Int = {
	var res : Int;
	var q : Int;
	var r : Int;

	q = n1/n2;
	r = n1-(q*n2);
	
	if (r == 0) {
	    res = n2;
	} else {
	    res = this.calcul(n2, r);
	}

	return res;
    }
}
