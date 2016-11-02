object Factorial {
    def main() : Unit = {
        println(new Fact().computeFactorial(10));        
    }
}

class Fact {
    def computeFactorial(num : Int) : Int = {
        if (num < 1) {
            return 1;
        } else {
            return num * (this.computeFactorial(num - 1));
        }
    }
}