object TailRecursive {
    def main() : Unit = {
        if(new Computer().compute()) println("done");
    }
}

class Computer {

	def compute() : Bool = {
		var head : Node;
		
		head = this.createList();
		
		println("-------------------------");
		println("--> Sum : " + this.sumList(head));
		println("-------------------------");
		
		return true;
	}
	
	// Returns the head of the list
	def createList() : Node = {
	
		var listLength : Int;
		var rand : Random;
		var head : Node;
		var tail : Node;
		var tempNode : Node;
		var foo : Bool;
	
		listLength = 10;
		rand = new Random().init(16756454);
		head = new Node().init(rand.getInt(0,1000));
		tail = head;
		
		while(!(listLength < 0)) {
			tempNode = new Node().init(rand.getInt(0,1000));
			foo = tail.addNext(tempNode);
			tail = tempNode;
			listLength = listLength - 1;
		}
		
		return head;
	}
	
	// sums all the elements of the list
	def sumList(head : Node) : Int = {
		return this.sumLoop(head, 0);
	}
	
	def sumLoop(node : Node, accumulator : Int) : Int = {
		if (node.hasNext()) {
			return this.sumLoop(node.next(), node.getValue() + accumulator);
		} else {
			return accumulator;
		}
	}
}

class Node {
	var value : Int;
	var next : Node;
	var hasNext : Bool;
	
	def init(v : Int) : Node = {
		value = v;
		hasNext = false;
		return this;
	}
	
	def addNext(n : Node) : Bool = {
		next = n;
		hasNext = true;
		return true;
	}
	
	def hasNext() : Bool = {
		return hasNext;
	}
	
	def next() : Node = {
		return next;
	}

    def getValue() : Int = {
        return value;
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