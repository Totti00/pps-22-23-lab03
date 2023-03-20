package u02

import org.junit.*
import org.junit.Assert.*
import Lab.*

class LabTest {
  import List.*

  val lst = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop() =
    assertEquals(Cons(20,Cons(30,Nil())), drop(lst,1))
    assertEquals(Cons(30,Nil()), drop(lst,2))
    assertEquals(Nil(), drop(lst,-20))

  @Test def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10,Cons(20,Cons(30,Cons(40,Nil())))), append(lst, tail))
}
