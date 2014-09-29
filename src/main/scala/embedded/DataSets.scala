package embedded

// encoding sets by representing the elements
trait StreamSets[Element] extends EnumerableSets[Element] {
    // Representations
    override type Set = Stream[Element]
    
    // Constructors
    val seed: Element
    def generator(current: Element): Element
    
    override def empty = Stream.empty[Element]
    override def univ = {
        def build(i: Element): Stream[Element] = i #:: build(generator(i))
        build(seed)
    }
    override def singleton(e: Element) = Stream(e)
    override def union(s1: Set, s2: Set) = s1 ++ s2
    
    // Observations
    override def contains(s: Set, e: Element) = s contains e
    override def elements(s: Set) = s
}

// sets of natural numbers, encoded as an ordered stream of elements
object OrderedNatSets extends StreamSets[Int] {
    // Constructors
    override val seed = 0
    override def generator(n: Int) = n + 1
}

// sets of Ints, encoded as a random stream of elements
object RandomIntSets extends StreamSets[Int] {
    // Constructors
    import scala.util.Random
    private val prng = new Random()
    override val seed: Int = prng.nextInt()
    override def generator(n: Int): Int = prng.nextInt()
}

// use generic testing to test our encoding
// NOTE: Try changing TestOptSets to TestSets and see what happens!
object TestDataSets extends TestOptSets with App {
    println("Testing ordered data encoding:")
    test(OrderedNatSets, 1000000)
    
    println("Testing random data encoding:")
    test(RandomIntSets, 1000000)
}
