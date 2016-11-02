object Contaminator {
    def main() : Unit = {
        if(new Computer().compute()) { println("Ok"); } else { println("error"); }
    }
}

class Computer {
	def compute() : Bool = {
        var simulation : Simulation;
        var seed : Int;
        var numHumans : Int;
        var infectionPercent : Int;
        var contaminationPercent : Int;
        var width : Int;
        var height : Int;
        
        // Change the seed to have new random values
        seed = 1046543;
        
        // Choose the number of random humans
        numHumans = 35;
        
        // Choose the percent of infection and contamination
        infectionPercent = 10;
        contaminationPercent = 20;
        
        // Choose the size of the map
        width = 20;
        height = 10;
        
        simulation = new Simulation().init(width, height, seed, contaminationPercent);
        return simulation.start(numHumans, infectionPercent);
	}
}

class HumanManager {
	// 0 = ok
	// 1 = infected
	// 2 = dead
	var rand : Random;
	
	def init(r: Random) : HumanManager = {
		rand = r;
		return this;
	}
	
	def infect(ref : Int) : Int = {
		if (ref == 0)
			ref = 1;
		return ref;
	}
	
	def kill(ref : Int) : Int = {
		return 2;
	}
	
	def isInfected(ref : Int) : Bool = {
		return ref == 1 || ref == 2;
	}
	
	def isOkay(ref : Int) : Bool = {
		return ref == 0;
	}
	
	def getHumanToken(ref : Int) : String = {
		var str : String;
		str = " e"; // Must have been an error if displayed
		
		if (ref == 0)
			str = " o";
		if (ref == 1)
			str = " x";
		if (ref == 2)
			str = " T";
		
		return str;
		
	}
	
	def createHuman(infectionPercent : Int) : Int = {
		var p : Int;
		var res : Int;
		
		p = rand.getInt(0,100);
		res = 0;
		
		if (p < infectionPercent)
			res = 1;
		
		return res;
	}
	
}


class Simulation {
	var map : Int[];
	var width : Int;
	var height : Int;
	
	var humans : Int[];
	var neighborX : Int[];
	var neighborY : Int[];
	var neighborCell : Int[];
	
	var rand : Random;
	var size : Int;
	var numFreeCells : Int;
	var contaminationPercent : Int;
	
	var humanManager : HumanManager;
	
	//   x 0  1  2
	// y  ________
	// 0 | 0  1  2			-------------------------
	// 1 | 3  4  5			--- Coordinate System ---
	// 2 | 6  7  8			-------------------------
	// 3 | 9 10 11
	//
	// getIndex(x,y) = y * width + x
	//
	// getX(index) = index % width;
	// getY(index) = floor(index / width); 
	
	def init(w : Int, h : Int, seed : Int, cp : Int) : Simulation = {
		var i : Int;
		
		rand = new Random().init(seed);
		humanManager = new HumanManager().init(rand);
		contaminationPercent = cp;
		
		width = w;
		height = h;
		size = w * h;
		
		numFreeCells = size;
		map = new Int[size];
		
		i = 0;
		while (i < size) {
			map[i] = 0-1;
			i = i + 1;
		}
		
		neighborX = new Int[4];
		neighborX[0] = 0-1;
		neighborX[1] = 1;
		neighborX[2] = 0;
		neighborX[3] = 0;
		
		neighborY = new Int[4];
		neighborY[0] = 0;
		neighborY[1] = 0;
		neighborY[2] = 0-1;
		neighborY[3] = 1;
		
		neighborCell = new Int[4];
		
		return this;
	}
	
	def start(numHumans : Int, infectionPercent : Int) : Bool = {
		var i : Int;
		var h : Int;
		var round : Int;
		var str : String;
		var foo : Bool;
		
		var infectedSum : Int;
		var okaySum : Int;
		
		humans = new Int[numHumans];
		
		println("-----------------------------------------");
		println("------------ Creating Humans ------------");
		println("-----------------------------------------");
		i = 0;
		while (i < numHumans) {
			if ( !( this.addHuman(humanManager.createHuman(infectionPercent)) ) )
				println(">> Too many humans for this map");
			i = i + 1;
		}
		
		println("");
		println("-----------------------------------------");
		println("---------- Starting Simulation ----------");
		println("-----------------------------------------");
		println("");
		
		round = 1;
		okaySum = humans.length;
		
		while (!(okaySum < 1)) {
		
			infectedSum = 0;
			okaySum = 0;
			
			println("");
			println("");
			println(">>>> Round " + round);
			i = 0;
			h = 0;
			str = "";
			while (i < size) {
				if (rand.mod(i, width) == 0) {
					println(str);
					str = "";
				}
				if(map[i] < 0)
					str = str + " _";
				else {
					str = str + humanManager.getHumanToken(map[i]);
					if (humanManager.isOkay(map[i]))
						okaySum = okaySum + 1;
					if (humanManager.isInfected(map[i]))
						infectedSum = infectedSum + 1;
					humans[h] = i;
					h = h + 1;
				}
				i = i + 1;
			}
			println(str);
			println("Infected Humans = " + infectedSum);
			println("Healthy Humans = " + okaySum);
			h = 0;
			while (h < humans.length) {
				foo = this.updateHuman(humans[h]);
				h = h + 1;
			}
			round = round + 1;
		}
		
		return true;
	}
	
	def addHuman(ref : Int) : Bool = {
		var cell : Int;
		var i : Int;
		var res : Bool;
		
		if (numFreeCells < 0 || numFreeCells == 0)
			res = false;
		else {
			cell = rand.getInt(0,numFreeCells);
		
			i = 0;
			while (i < size && !(cell < 0)) {
				if (map[i] == 0-1) {
					if (cell == 0) {
						map[i] = ref;
					}
					cell = cell - 1;
				}
				i = i + 1;
			}
			numFreeCells = numFreeCells - 1;
			res = true;
		}
		
		return res;
	}
	
	def moveHuman(index : Int, tX : Int, tY : Int) : Bool = {
		var x : Int;
		var y : Int;
		var currentCell : Int;
		
		x = rand.mod(index, width) + tX;
		y = index / width + tY;
		currentCell = map[index];
		
		map[index] = 0-1;
		map[y * width + x] = currentCell;
		
		return true;
	}
	
	def updateHuman(index : Int) : Bool = {
		var x : Int;
		var y : Int;
		var i : Int;
		var res : Bool;
		var foo : Bool;
		
		var infectedNeighborsCount : Int;
		var freeNeighborCellsCount : Int;
		var p : Int;
		
		var currentCell : Int;
		// Int code :
		// >0 => human
		// -1 => empty
		// -2 => wall
		
		currentCell = map[index];
		if (currentCell < 0)
			res =  false;
			
		else {
		
			x = rand.mod(index, width);
			y = index / width;
			
			if (x - 1 < 0)
				neighborCell[0] = 0-2;
			else
				neighborCell[0] = map[y * width + (x-1)];
				
			if (!(x + 1 < width))
				neighborCell[1] = 0-2;
			else
				neighborCell[1] = map[y * width + (x+1)];
			
			if (y - 1 < 0)
				neighborCell[2] = 0-2;
			else
				neighborCell[2] = map[(y-1) * width + x];
			
			if (!(y + 1 < height))
				neighborCell[3] = 0-2;
			else
				neighborCell[3] = map[(y+1) * width + x];
				
			infectedNeighborsCount = 0;
			freeNeighborCellsCount = 0;
			
			i = 0;
			while (i < 4) {
				if (humanManager.isInfected(neighborCell[i]))
					infectedNeighborsCount = infectedNeighborsCount + 1;
					
				else if (neighborCell[i] == 0-1)
					freeNeighborCellsCount = freeNeighborCellsCount + 1;
				
				i = i + 1;
			}
				
			p = rand.getInt(0,100);
			if(p < contaminationPercent * infectedNeighborsCount)
				map[index] = humanManager.infect(currentCell);
			
			
			// Moving
			if (!(freeNeighborCellsCount < 0) && !(freeNeighborCellsCount == 0)) {
				p = rand.getInt(0,freeNeighborCellsCount);
				i = 0;
				while (i < 4) {
					if (neighborCell[i] == 0-1) {
						if (p == 0) {
							foo = this.moveHuman(index, neighborX[i], neighborY[i]);
						}
						p = p - 1;
					}
					i = i + 1;
				}
			}
		
			res = true;
		}
		return res;
	}
}

class Random {
	var a : Int;
	var b : Int;

	def init(seed : Int) : Random = {
    	a = seed;
    	b = 67890;
    	return this;
	}

	def getInt(min : Int, max : Int) : Int = {
    	var posInt : Int;

    	posInt = this.nextInt();
    	if(posInt < 0)
    		posInt = 0 - posInt;

    	return min + (this.mod(posInt, max - min));
	}

	def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }

	def nextInt() : Int = {
    	b = 36969 * ((b * 65536) / 65536) + (b / 65536);
    	a = 18000 * ((a * 65536) / 65536) + (a / 65536);
    	return (b * 65536) + a;
	}
}