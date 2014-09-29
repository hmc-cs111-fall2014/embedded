package embedded

// encoding sets as characteristic functions
trait CharacteristicSets[Element] extends Sets[Element] {
    // Representations
    override type Set = Element => Boolean

    // Constructors
    override def empty = i => false
    override def univ  = i => true
    override def singleton(e: Element) = i => i == e
    override def union(s1: Set, s2: Set) = i => s1(i) || s2(i)
    
    // Observations
    override def contains(s: Set, e: Element) = s(e)
}

// sets of Ints, encoded as a characteristic function
object IntCharacteristicSets extends CharacteristicSets[Int]

// use generic testing to test our encoding
object TestCharacteristicSets extends TestSets with App {
    println("Testing characteristic functions encoding...")
    test(IntCharacteristicSets, 5)
}
