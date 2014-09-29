package embedded

// the "world" of sets
trait Sets[Element] {
    // Representations
    type Set
    
    // Constructors
    def empty: Set
    def univ:  Set
    def singleton(e: Element): Set
    def union(s1: Set, s2: Set): Set

    // Observations
    def contains(s: Set, e: Element): Boolean
}

// more features, if the user knows that the set is enumerable
trait EnumerableSets[Element] extends Sets[Element] {
    // Observations
    def elements(s: Set): Seq[Element] 
    
    def issubset(s1: Set, s2: Set): Boolean = 
        (true /: elements(s1))(_ && contains(s2, _))
    def isequal(s1: Set, s2: Set): Boolean = 
        issubset(s1, s2) && issubset(s2, s1)
}

// generic testing
trait TestSets {
    // a proof-of-concept function to show that our 
    // library provides extensible, type-safe sets
    def test[Element](semantics: Sets[Element], element: Element) = {
        
        import semantics._

        val s = singleton(element)
        
        doTest("e ∈ {e}", 
               contains(s, element))

        doTest("e ∉ ∅", 
               !contains(empty, element))
               
        doTest("e ∉ (∅ ∪ ∅)", 
               !contains(union(empty, empty), element))
               
        doTest("e ∈ (∅ ∪ {e})", 
               contains(union(empty, s), element))
        
        doTest("e ∈ univ",
               contains(univ, element))
               
        doTest("e ∈ (∅ ∪ univ)", 
               contains(union(empty, univ), element))
               
        println()
    }
    
    
    def doTest(msg: String, test: =>Boolean) = 
        try {
            assert(test, msg)
            println(Console.GREEN + "[PASSED] " + Console.RESET + msg)
        } catch {
            case e: AssertionError => 
                println(Console.RED + "[FAILED] " + Console.RESET + msg)
            case e: Throwable => 
                println(Console.RED + "[ERROR]  " + Console.RESET + msg)
                e.printStackTrace()
        }
}
