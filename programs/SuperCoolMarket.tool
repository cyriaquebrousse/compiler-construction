object SuperCoolMarket {
    def main() : Unit = {
        if(new Computer().compute()) { println("Ok"); } else { println("error"); }
    }
}

class Computer {
	def compute() : Bool = {
        var basket : Basket;
        var foo : Bool;
        var seed : Int;
        var numItems : Int;
        
        // Change the seed to have new random values
        seed = 10469543;
        
        // Choose the number of random items you want to buy
        numItems = 6;
        
        
        basket = new Basket().init(seed);
        foo = basket.random(numItems);
        foo = basket.checkout();
        
        return true;
	}
}

class Item {
	var name : String;
	var price : Int;
	var ref : Int;
	var color : String;
	
	def init(n : String, p : Int, r : Int, c : String) : Item = {
		name = n;
		price = p;
		ref = r;
		color = c;
		
		return this;
	}
	
	def getPrice() : Int = {
		return price;
	}
	
	def toString() : String = {
		return "Item #" + ref + " --" + name + "-- " + price + ".- CHF // color : " + color;
	}
	
	def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }
	
	def refToItem(r: Int) : Item = {
		var lastDigitRef : Int;
		var firstDigitsRef : Int;
		
		ref = r;
		
		lastDigitRef = this.mod(ref,10);
		if (lastDigitRef == 0) {
			color = "Oak";
		}
		if (lastDigitRef == 1) {
			color = "Dark Oak";
		}
		if (lastDigitRef == 2) {
			color = "Light Ash";
		}
		if (lastDigitRef == 3) {
			color = "Iron";
		}
		if (lastDigitRef == 4) {
			color = "Bamboo";
		}
		if (lastDigitRef == 5) {
			color = "Glass";
		}
		if (lastDigitRef == 6) {
			color = "Natural Stone";
		}
		if (lastDigitRef == 7) {
			color = "Slate";
		}
		if (lastDigitRef == 8) {
			color = "Dark Bamboo";
		}
		if (lastDigitRef == 9) {
			color = "Aluminum";
		}
		
		firstDigitsRef = ref / 10;
		if (!(firstDigitsRef < 0) && 
			firstDigitsRef < 100) {
			name = "IKEA Table";
			price = 169;
		}
		if (!(firstDigitsRef < 100) && 
			firstDigitsRef < 180) {
			name = "IKEA Chair";
			price = 39;
		}
		if (!(firstDigitsRef < 180) && 
			firstDigitsRef < 500) {
			name = "FLY Sofa";
			price = 1999;
		}
		if (!(firstDigitsRef < 500) && 
			firstDigitsRef < 600) {
			name = "FLY Table";
			price = 359;
		}
		if (!(firstDigitsRef < 600) && 
			firstDigitsRef < 760) {
			name = "FLY Chair";
			price = 59;
		}
		if (!(firstDigitsRef < 760) && 
			firstDigitsRef < 1000) {
			name = "FLY Mattress";
			price = 350;
		}
		if (!(firstDigitsRef < 1000) && 
			firstDigitsRef < 1500) {
			name = "IKEA Armory";
			price = 100;
		}
		if (!(firstDigitsRef < 1500) && 
			firstDigitsRef < 1800) {
			name = "IKEA Coffee Table";
			price = 139;
		}
		if (!(firstDigitsRef < 1800) && 
			firstDigitsRef < 2010) {
			name = "FLY Coffee Table";
			price = 399;
		}
		if (!(firstDigitsRef < 2010) && 
			firstDigitsRef < 2300) {
			name = "IKEA Bed";
			price = 200;
		}
		if (!(firstDigitsRef < 2300) && 
			firstDigitsRef < 2500) {
			name = "FLY Bed";
			price = 590;
		}
		if (!(firstDigitsRef < 2500) && 
			firstDigitsRef < 2800) {
			name = "FLY Armory";
			price = 299;
		}
		if (!(firstDigitsRef < 2800) && 
			firstDigitsRef < 3200) {
			name = "INTERIO Sofa";
			price = 3000;
		}
		if (!(firstDigitsRef < 3200) && 
			firstDigitsRef < 3500) {
			name = "INTERIO Table";
			price = 500;
		}
		if (!(firstDigitsRef < 3500) && 
			firstDigitsRef < 3670) {
			name = "INTERIO Chair";
			price = 75;
		}
		if (!(firstDigitsRef < 3670) && 
			firstDigitsRef < 3900) {
			name = "INTERIO Mattress";
			price = 250;
		}
		if (!(firstDigitsRef < 3900) && 
			firstDigitsRef < 4110) {
			name = "INTERIO Coffee Table";
			price = 289;
		}
		if (!(firstDigitsRef < 4110) && 
			firstDigitsRef < 4500) {
			name = "INTERIO Bed";
			price = 855;
		}
		if (!(firstDigitsRef < 4500) && 
			firstDigitsRef < 5000) {
			name = "INTERIO Armory";
			price = 539;
		}
		
		return this;
	}

}

class Basket {
	var items : Int[];
	var rand : Random;
	
	def init(seed: Int) : Basket = {
		rand = new Random().init(seed);
		
		return this;
	}
	
	def random(numItems: Int) : Bool = {
		var i : Int;
		
		items = new Int[numItems];
		
		i = 0;
		while (i < numItems) {
			items[i] = rand.getInt(0, 50000);
			i = i + 1;
		}
		
		return true;
	}
	
	def checkout() : Bool = {
		var i : Int;
		var item : Item;
		var sum : Int;
		
		println("");
		println("----------------------------------------------");
		println("--------- Welcome to SuperCoolMarket ---------");
		println("----------------------------------------------");
		println("");
		if (items.length == 0)
			println("--- Your Purchase :");
		else
			println("--- Your Purchases :");

		sum = 0;
		i = 0;
		while (i < items.length) {
			item = new Item().refToItem(items[i]);
			println(item.toString());
			sum = sum + item.getPrice();
			i = i + 1;
		}
		println("");
		println("--- Total :");
		println("excl tax = " + sum + ".- CHF");
		println("VAT = 20%");
		println("incl tax = " + (sum * 120) / 100 + ".- CHF");
		println("");
		
		println("--------- Thank you for your fidelity ---------");
		
		return true;
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