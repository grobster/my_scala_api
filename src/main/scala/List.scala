package collection

import scala.annotation._

/**
  * trait of list object
  */
sealed trait List[+A] {
	//def map[B](f: A => B): List[B] = foldLeft(Nil: List[B])((h,t) => Cons(f(h), t))
	
	def foldLeft[B](z: B)(f: (B,A) => B): B = this match {
		case Cons(h,t) => t.foldLeft(f(z, h))(f)
		case _ => z
	}
	
	/**
	  * Reverses order of a list
	  */
	def reverse(): List[A] = foldLeft(List[A]())((b,a) => Cons(a,b))
}
/**
  * Represents an empty List.
  */
case object Nil extends List[Nothing]

/**
  * This is a class object of a linkedlist
  */
case class Cons[+A](h: A, t: List[A]) extends List[A]

/**
  * Singleton list object
  */
object List {
	def apply[A](as: A*): List[A] = if(as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
	
	def range(start: Int, end: Int): List[Int] = {
		@tailrec
		def _range(start: Int, end: Int, acc: List[Int]): List[Int] = start match {
			case x if(x < end) => _range(start + 1, end, Cons(x, acc))
			case _ => acc
		}
		_range(start, end, Nil).reverse
	}
}