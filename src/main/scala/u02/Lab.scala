package u02

import scala.annotation.tailrec

object Lab extends App{

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
}
