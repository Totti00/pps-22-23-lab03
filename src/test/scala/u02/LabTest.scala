package u02

import org.junit.*
import org.junit.Assert.*
import u02.Optionals.*
import Lab.*
import u02.AlgebraicDataTypes.*

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

  @Test def testFlatMap() =
    assertEquals(Cons(11,Cons(21,Cons(31,Nil()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11,Cons(12,Cons(21,Cons(22,Cons(31,Cons(32,Nil())))))), flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMapwithFlatMap() =
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(lst)(_ + ""))
    assertEquals(Cons(13, Cons(23, Cons(33, Nil()))), mapWithFlatMap(lst)(_ + 3))

  @Test def testFilterwithFlatMap() =
    assertEquals(Cons(20, Nil()), filterWithFlatMap(lst)(_ == 20))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), filterWithFlatMap(lst)(_ >= 3))

  @Test def testMax() =
    assertEquals(Option.Some(30), max(lst))
    assertEquals(Option.None(), max(Nil()))
    assertEquals(Option.Some(20), max(Cons(10, Cons(20, Cons(15, Nil())))))

  @Test def testTeacherCourses() =
    assertEquals(Cons("OS", Cons("PCD", Nil())), getTeacherCourses(Cons(Person.Teacher("Ghini", "OS"), Cons(Person.Student("Giacomo", 23), Cons(Person.Teacher("Ricci", "PCD"), Nil())))))
    assertEquals(Nil(), getTeacherCourses(Cons(Person.Student("Luca", 16), Cons(Person.Student("Giacomo", 23), Cons(Person.Student("Marco", 32), Nil())))))
}
