object MainFarm {
	def main():Unit={
		println(new Farm().run());
	}
}

class Farm {
	// Creates some animals make them talk and kills them
	def run(): Bool ={
		var animal: Animal;
		var duck: Duck;
		var crab: Crab;
		var void: Bool;
		
		println("Checking inheritance and overriding");
		duck = new Duck();
		crab = new Crab();
		void = duck.Duck();
		void = crab.Crab();
		void = duck.setName("Lucas");
		void = crab.setName("Mr.Crab");
		void = duck.cuack();
		void = crab.makeSound();
		void = duck.die();
		void = crab.die();

		println("Checking subtype polymhorpism");
		animal = new Animal();
		void = animal.Animal();
		println ("Is animal a Duck " + this.isAnimalADuck(animal));
		println ("Is Lucas a Duck " + this.isAnimalADuck(duck));

		animal = crab;
		void = animal.makeSound();
		void = animal.die();

		if(crab.isAlive()){
			println("Mr.Crab is alive!");
		}else{
			println("So long to Mr.Crab :(");
		}		

		return true;
	}

	def isAnimalADuck(animal: Animal): String = {
		var res: String;

		println("Checking longest match rule in parsing Expression ????");
		if(animal.getSound() == "Cuack"){
			res= "Yes";
		}
		else {
			res = "No";
		}
		return res;
	}

}

class Animal {
	var sound: String;
	var name: String;
	var isAlive: Bool;
	
	def Animal(): Bool = {
		sound = "";
		name = "";
		isAlive = true;
		return true;
	}
	
	def makeSound(): Bool = { 
		println(sound);
		return true;
	}
	def die(): Bool = { 
		println(name + ": I'm dead");
		isAlive = false;
		return true;
	}
	def isAlive(): Bool = { 
		return isAlive;
	}
	def setName(nameIn: String):Bool = {
		name = nameIn;
		return true;
	}
	def getSound():String = {
		return sound;
	}

}

class Duck extends Animal{
	def Duck(): Bool = {
		//super();
		sound = "Cuack";
		name = "";
		isAlive = true;
		return true;
	}
	
	def cuack(): Bool = {
		return this.makeSound();
	}
}

class Crab extends Animal{
 	def Crab(): Bool = {
		sound = "What does the crab say?";
		name = "";
		isAlive = true;
		return true;
	}
	
	def die(): Bool = {
		println("Oh please, I'm inmortal!");
		return true;
	}
}