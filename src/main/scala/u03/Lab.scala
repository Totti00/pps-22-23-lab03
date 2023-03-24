package u03

import u02.Optionals.Option
import u02.Optionals.Option.{None, Some}

import scala.annotation.tailrec

object Lab extends App {

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

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
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

    @tailrec
    def foldLeft[A, B](l: List[A])(acc: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
      case _ => acc

    def foldRight[A, B](l: List[A])(acc: B)(f: (A, B) => B): B = l match
      case Cons(h, t) => f(h, foldRight(t)(acc)(f))
      case _ => acc


  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    import List.*

    def getTeacherCourses(l: List[Person]): List[String] =
      flatMap(l)({ case Person.Teacher(_, c) => Cons(c, Nil()) case Person.Student(_, _) => Nil() })

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:
    def empty[A](): Stream[A] = Empty()

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    @tailrec
    def drop[A](s: Stream[A])(n: Int): Stream[A] = s match
      case Cons(_, t) if n > 0 => drop(t())(n - 1)
      case Cons(_, _) => s
      case _ => Empty()

    def constant[A](init: => A): Stream[A] =
      cons(init, constant(init))

    def fibStream(): Stream[Int] =
      def _fibStream(prev: Int, current: Int): Stream[Int] =
        cons(current, _fibStream(current, current + prev))
      _fibStream(1, 0)
}
