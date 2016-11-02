/**
  *		Universe.tool
  *		Group 20, CB
  */

object Universe {
	def main(): Unit = {
		println("#### Universe ####");
		if (new Run().run()) { println(">> Simulation successful"); }
		else { println(">> Simulation failed!"); }
		println("##################");
	}
}

class Run {
	def run(): Bool = {
		// Declarations (N.B.: this is an inline comment!)
		var earth: Planet;
		var moon: Asteroid;
		var mars: Planet;
		var terrien: Human;
		var alien1: Alien;
		var martien: Alien;
		var rValue: Bool;
		
		/*
		 * Init (this is a block comment!)
		 */
		earth = new Planet().init("Terre");
		moon = new Asteroid().init("Lune");
		mars = new Planet().init("Mars");
		terrien = new Human().init("Jean-Jacques");
		alien1 = new Alien().init("Random-Alien");
		martien = new Alien().init("Martien");
		
		// Invading planets
		rValue = false;
		println(">> Objects created, now running the simulation...");
		if(terrien.invade(earth)) {
			if(!terrien.invade(moon)) {
				if(martien.invade(mars)) {
					if(!alien1.invade(earth)) {
						if(!terrien.invade(mars)) {
							rValue = true;
						}
					}
				}
			}
		}
		return (rValue);
	}
}

class Astre {
	var name: String;
	var isInvaded: Bool;
	var invader: Inhabitant;
	
	def invade(i: Inhabitant): Bool = {return false;}
	def isPlanet(): Bool = {return false;}
	def getName(): String = { return name; }
}

class Planet extends Astre {
	def init(n: String): Planet = {
		name = n;
		isInvaded = false;
		return this;
	}
	
	def invade(i: Inhabitant): Bool = {
		var rValue: Bool;
		if (isInvaded) {
			rValue = false;
			println("Failed to invade "+ name +", "+ i.getName() +": "+ invader.getName() +" is already here!");
		} else {
			isInvaded = true;
			rValue = isInvaded;
			invader = i;
			println("Invasion of "+ name +" successful, "+ invader.getName());
		}
		return rValue;
	}
	
	def isPlanet(): Bool = {return true;}
}

class Asteroid extends Astre {
	def init(n: String): Asteroid = {
		name = n;
		isInvaded = false;
		return this;
	}
	
	def invade(i: Inhabitant): Bool = {
		var rValue: Bool;
		if (isInvaded) {
			rValue = false;
			println("Failed to invade "+ name +", "+ i.getName() +": "+ invader.getName() +" is already here!");
		} else {
			isInvaded = true;
			rValue = isInvaded;
			invader = i;
			println("Invasion of "+ name +" successful, "+ invader.getName());
		}
		return rValue;
	}
}

class Inhabitant {
	var name: String;
	
	def getName(): String = { return name; }
}

class Human extends Inhabitant {
	def init(n: String): Human = {
		name = n;
		return this;
	}
	
	def invade(astre: Astre): Bool = {
		var rValue: Bool;
		if (astre.isPlanet()) {
			rValue = astre.invade(this);
		} else {
			println("Failed to invade "+ astre.getName() +", "+ name +": you're human and need oxygen!");
			rValue = false;
		}
		return rValue;
	}
}

class Alien extends Inhabitant {
	def init(n: String): Alien = {
		name = n;
		return this;
	}
	
	def invade(astre: Astre): Bool = {
		return astre.invade(this);
	}
}