package u02

import scala.annotation.tailrec
import u02.Optionals.*
import u02.AlgebraicDataTypes.*
import u02.AlgebraicDataTypes.Person.Teacher

object Lab extends App{

  import Option.*

  enum List[A]:
    case Cons(head: A, tail: List[A])
    case Nil()

  object List:
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (l, 0) => l
      case (Cons(_, t), n) => drop(t, n - 1)

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    def mapWithFlatMap[A, B](l: List[A])(f: A => B): List[B] =
      flatMap(l)(v => Cons(f(v), Nil()))

    def filterWithFlatMap[A](l: List[A])(pred: A => Boolean): List[A] =
      flatMap(l)(a => pred(a) match
        case true => Cons(a, Nil())
        case _ => Nil()
      )

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match
        case Some(i) if h > i => Some(h)
        case Some(i) => Some(i)
        case None() => Some(h)
      case Nil() => None()

    def getTeacherCourses(l: List[Person]): List[String] =
      flatMap(l)({case Person.Teacher(_, c) => Cons(c, Nil()) case Person.Student(_, _) => Nil()})

    def foldLeft[A, B](l: List[A])(acc: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
      case _ => acc

    def foldRight[A, B](l: List[A])(acc: B)(f: (A, B) => B): B = l match
      case Cons(h, t) => f(h, foldRight(t)(acc)(f))
      case _ => acc
}
