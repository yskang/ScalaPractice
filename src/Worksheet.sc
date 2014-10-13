object Worksheet {
  trait List[+A]
  case object Nil extends List[Nothing] {
    override def toString:String = "List()"
  }

  case class Cons[A](head: A, tail: List[A]) extends List[A] {
//    override def toString:String = tail match {
//      case Nil => head + ")"
//      case Cons(x, xs) => head + ", " + tail.toString()
//    }

    override def toString:String = this match {
      case Cons(x, Nil) => "List("+ x + ")"
      case Cons(x, xs) => "List("+ x + foldLeft(xs, "")(_+","+_) + ")"
    }

  }
  List(1, 2, 3, 4, 5, 6).toString()
  List("a", "b", "c")
  List(1)
  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail:_*))
  }
  def sum(is: List[Int]): Int = is match  {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, t) => t
  }
  def drop[A](as: List[A], n: Int): List[A] =
    if (n == 0) as
    else drop(tail(as), n-1)
  def dropWhile[A](as: List[A]) (f:A=>Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }
  val xs: List[Int] = List(1, 2, 3, 4, 5)
  val ex1 = dropWhile(xs) (x => x < 4)
  val ex2 = dropWhile(xs) (_ < 4)
  def foldRight[A, B](as: List[A], z:B)(f:(A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  def sum2(is: List[Int]): Int = foldRight(is, 0)(_ + _)
  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)
  foldRight(xs, 0)(_+_)
  foldRight(Cons(1, Nil), 0)(_+_)

  def foldRight2(as: List[Int], z:Int)(f:(Int, Int) => Int): Int = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight2(xs, z)(f))
  }

  foldRight2(Cons(1, Nil), 0)(_+_)

  1 + foldRight2(Nil, 0)(_+_)

  1 + 0
  foldRight2(Cons(1, Cons(2, Nil)), 0)(_+_)
  1 + foldRight2(Cons(2, Nil), 0)(_+_)
  1 + 2 + foldRight2(Nil, 0)(_+_)
  1 + 2 + 0
//
//  foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)(_+_)
//
//  foldLeft(Cons(1, Cons(2, Nil)), 0)(_+_) + 3
//
//  foldLeft(Cons(1, Nil), 0)(_+_) + 2 + 3
//
//  0 + 1 + 2 + 3
//
  def getLast[A](as: List[A]): A = as match {
    case Cons(x, Nil) => x
    case Cons(x, xs) => getLast(xs)
  }

  def getFronts[A](as: List[A]): List[A] = as match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, getFronts(xs))
  }

  def foldLeft[A, B](as: List[A], z:B)(f:(B, A)=>B): B = as match {
    case Cons(x, Nil) => f(z, x)
    case Cons(x, xs) => f(foldLeft(getFronts(as), z)(f), getLast(as))
  }

  def foldLeft2[A, B](as: List[A], z:B)(f:(B, A)=>B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(foldLeft2(xs, z)(f), x)
  }

  def foldLeft3[A, B](list: List[A], z: B )(f: (B, A )=> B ) = list match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight3[A, B](as: List[A], z:B)(f:(A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight3(xs, z)(f))
  }
  val ys: List[String] = List("a", "b", "c", "d")
  val ys2: List[String] = List("e", "f", "g", "h")
  val y: List[Double] = List(1, 2, 3, 4)
  getLast(ys)
  getFronts(ys)
  foldLeft(ys, "0")(_+_)
  foldLeft2(ys, "0")(_+_)
  foldLeft3(ys, "0")(_+_)
  foldRight(ys, "0")(_+_)
  foldLeft[Double, Double](y, 1)(_/_)
  foldLeft2[Double, Double](y, 1)(_/_)
  foldLeft3[Double, Double](y, 1)(_/_)
  foldRight[Double, Double](y, 1)(_/_)
  foldLeft[Double, Double](y, 1)(_+_)
  foldLeft2[Double, Double](y, 1)(_+_)
  foldLeft3[Double, Double](y, 1)(_+_)
  foldRight[Double, Double](y, 1)(_+_)

  def map[A, B](as: List[A])(f:A=>B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x),map(xs)(f))
  }

  map(y)(_+1)

  def filter[A](as: List[A])(f:A=>Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if(f(x)) => Cons(x, filter(xs)(f))
    case Cons(x, xs) if(!f(x)) => filter(xs)(f)
  }
  filter(y)(_%3 == 0)
  def concat[A](as:List[A], bs:List[A]): List[A] = as match{
    case Cons(x, xs) => Cons(x, concat(xs, bs))
    case Nil => bs
  }

  def concat2[A](as:List[A], bs:List[A]): List[A] = as match{
    case Nil => bs
    case Cons(x, xs) => Cons(x, concat2(xs, bs))
  }

  concat2(ys, ys2)

  concat(ys, ys2)

  def flatMap[A, B](as: List[A])(f: A=>List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => concat(f(x), flatMap(xs)(f))
  }
  val testList = List(1,2,3)
  flatMap(testList)(x => List(x,x,x))


}

