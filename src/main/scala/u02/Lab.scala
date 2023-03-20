package u02

object Lab extends App{

  enum List[A]:
    case Cons(head: A, tail: List[A])
    case Nil()

  object List:
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

}
