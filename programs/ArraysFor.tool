/**
  *		ArraysFor.tool
  *		Group 20, CB
  *		
  *		Specs: errcode return value is anything >= 999999
  *		See Ops:foreach() for opcodes
  */

object ArraysFor {
	def main(): Unit = {
		println("#### ArraysFor ####");
		if (new Run().run()) { println(">> Simulation successful"); }
		else { println(">> Simulation failed!"); }
		println("###################");
	}
}

class Run {
	def run(): Bool = {
		// Declarations
		var rValue: Bool;
		var numbers: Int[];
		var ops: Ops;
		var size: Int;
		
		// Init
		numbers = new Int[10];
		size = 10;
		ops = new Ops(); // "static"
		
		// Running simulation
		rValue = false;
		println(">> Objects created, now running the simulation...");
		if(ops.init(numbers, size)) {
			if(ops.foreach(numbers, size, 1) == 0) {
				if(ops.foreach(numbers, size, 3) == 206) {
					if(ops.foreach(numbers, size, 2) == 6) {
						if (!(ops.foreach(numbers, size, 42) < 999999)) { // invalid cmd
							rValue = true;
						}
					}
				}
			}
		}
		
		return rValue;
	}
	
}

class Ops {
	def init(a: Int[], size: Int): Bool = {
		var rValue: Bool;
		rValue = false;
		if (!(size == 10)) {
			println("Wrong size, should be 10!");
		} else {
			a[0] = 5; a[1] = 10; a[2] = 8; a[3] = 3; a[4] = 7; // no negative values !!
			a[5] = 13; a[6] = 75; a[7] = 42; a[8] = 1; a[9] = 42;
			rValue = true;
		}
		return rValue;
	}
	
	/**
	  *		Possible command codes:
	  *		1 ->	print
	  *		2 ->	maxPos
	  *		3 ->	sum
	  */
	def foreach(a: Int[], size: Int, cmd: Int): Int = {
		var rValue: Int;
		var i: Int;
		i = 0;
		rValue = 999999;
		
		if (cmd == 1) {				// print
			println("Printing array (foreach):");
			while (i < size) {
				println("  a["+i+"] = "+ a[i]);
				i = i + 1;
			}
			rValue = 0;
		} else if (cmd == 2) {		// maxPos
			rValue = 0;
			while (i < size) {
				if (a[rValue] < a[i]) {
					rValue = i;
				}
				i = i + 1;
			}
			println("Max (foreach) = "+ a[rValue] +", at position "+ rValue);
		} else if (cmd == 3) {		// sum
			rValue = 0;
			while (i < size) {
				rValue = rValue + a[i];
				i = i + 1;
			}
			println("Sum (foreach) = "+ rValue);
		} else {
			println(cmd +": Error: no such command!");
		}
		
		return rValue;
	}
}