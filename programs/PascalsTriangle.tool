object PascalsTriangle {
    def main(): Unit = {
	if(new line().init(9, 1).print()) {
	    println("Done");
	}
    }
}

class line {
    var rest : Int;
    var size : Int;
    var next : line;
    var nb_chars : Int;
    var numbers : Int[];

    def init(n: Int, l: Int): line = {
	rest = n-1;
	size = l;
	numbers = new Int[size];

	if (0 < rest) {
	    next = new line().init(rest,size+1);
	}

	if (size == 1) {
	    numbers[0] = 1;
            nb_chars = next.make(this);
	}

	return this;
    }

    def make(prev: line): Int = {
	var prevnums : Int[];
	var i : Int;

	prevnums = prev.getNum();
	
	numbers[0]=1;
	numbers[size-1]=1;

	i = 1;
	while(i < size-1) {
	    numbers[i]=prevnums[i-1]+prevnums[i];
	    i=i+1;
	}
	
	if (rest==0) {
	    nb_chars=1;
	    i = numbers[size/2];
	    while(9<i) {
		nb_chars=nb_chars+1;
		i=i/10;
	    }
	} else {
	    nb_chars= next.make(this);
	}

	return nb_chars;
    }

    def getNum(): Int[] = {
	return numbers;
    }

    def print(): Bool = {
	var i : Int;
	var j : Int;
	var temp : Int;
	var dis : String;
	var done : Bool;
	var space : String;

	i=0;
	space="";
	while(i<nb_chars) {
	    space=space+" ";
	    i=i+1;
	}

	i=0;
	dis="";
	while(i<rest*2) {
	    dis=dis+space;
	    i=i+1;
	}
	
	i=0;
	while(i<size) {
	    temp = numbers[i];
	    j = 1;
	    while(9<temp) {
		j=j+1;
		temp=temp/10;
	    }

	    while(j<nb_chars) {
		dis = dis+" ";
		j=j+1;
	    }

	    dis = dis + numbers[i]+space+space+space;
	    i=i+1;
	}
	
	println(dis);

	if(0<rest) {
	    done = next.print();
	} else {
	    done = true;
	}

	return done;
    }
}
