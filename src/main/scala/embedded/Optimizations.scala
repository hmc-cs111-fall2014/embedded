package embedded

// an optimization for empty sets: empty is the identity for union
trait OptSets[Element] extends Sets[Element] {
    // Representations
    val representation: Sets[Element]
    override type Set = (representation.Set, Boolean)
   
    // Constructors
    override def empty = (representation.empty, true)
    override def univ  = (representation.univ, false)
    override def singleton(e: Element): Set = 
        (representation.singleton(e), false)
    override def union(s1: Set, s2: Set): Set = 
        (s1._2, s2._2) match {
            case (true, true) => (representation.empty, true)
            case (true, _) => (s2._1, false)
            case (_, true) => (s1._1, false)
            case _ => (representation.union(s1._1, s2._1), false)
        }
    
    // Observations
    override def contains(s: Set, e: Element): Boolean = 
        representation.contains(s._1, e)
}

// an "optimization" for infinite sets
trait InfiniteSets[Element] extends Sets[Element] {
    // Representations
    val representation: Sets[Element]
    override type Set = (representation.Set, Boolean)
    
    // Constructors
    override def empty = (representation.empty, false)
    override def univ  = (representation.univ, true)
    override def singleton(e: Element): Set = 
        (representation.singleton(e), false)
    override def union(s1: Set, s2: Set): Set = 
        if (s1._2 || s2._2) 
            (representation.univ, true)
        else
            (representation.union(s1._1, s2._1), false)
    
    // Observations
    override def contains(s: Set, e: Element): Boolean = 
        s._2 || representation.contains(s._1, e)
}

trait TestOptSets extends TestSets {
    override def test[Element](semantics: Sets[Element], element: Element) =
 
        super.test(new OptSets[Element] with InfiniteSets[Element] {
                        val representation = semantics
                   },
                   element)
}
