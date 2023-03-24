package u03

import Streams.*
import org.junit.*
import org.junit.Assert.*
import Lists.*

class StreamsTest {
  import List.*

  @Test def testDrop() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

}
