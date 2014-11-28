object GnomeSort {
	def main() : Unit = {
		println("Fin" + new GS().Start(20));
	}
}

class GS{
	var ar : Int[];
	var size : Int;
	
	def Start(sz : Int) : Int = {
		var trash : Int;
		
		trash = this.CreateArray(sz);
		trash = this.Print();
		println("**********************");
		trash = this.GSort();
		println("The array sorted");
		trash = this.Print();
		return 42;
	
	}
	
	def CreateArray(sze : Int) : Int = {
		
		size = sze;
		ar = new Int[size];

		ar[0] = 763;
		ar[2] = 712;
		ar[3] = 14;
		ar[1] = 7;
		ar[4] = 1264;
		ar[5] = 76224;
		ar[6] = 71;
		ar[7] = 264;
		ar[8] = 345;
		ar[11-2] = 7632756/764 - 65;
		ar[10] = 764+874-24+873/34;
		ar[55/5] = 764/72846 + 11 - 89734 + 73465;
		ar[12] = (764+78324)/896 - 90745*(8937 +948);
		ar[13] = 764763 / 9864;
		ar[14] = 9835;
		ar[15] = 21;
		ar[16] = 434;
		ar[17] = 4;
		ar[18] = 0;
		ar[19] = 354;

		
		return 42;
		}
		
		def Print() : Int = {
        var i : Int;

        i = 0 ;
        while (i < size) {
            println(ar[i]);
            i = i + 1 ;
        }
        return 42 ;
    }

	
	def GSort () : Int = {
	var i : Int;
	var tmp : Int;
	i = 0;

	
		
		while (i < size) {
			if (i == 0 || !(ar[i] < ar[i-1])) {
				i = i + 1;
			}
			else {
					tmp = ar[i]; 
					ar[i] = ar[i-1];
					i = i - 1;
					ar[i] = tmp;
				}
		}
		
	return 42;
	}
}

