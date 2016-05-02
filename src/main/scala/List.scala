package collection

import scala.annotation._

/**
  * trait of list object
  */
sealed trait List[+A] {
	def map[B](f: A => B): List[B] = this match {
		case Nil => Nil
		case Cons(h,t) => Cons(f(h), t.map(f))
	}
	
	def filter(f: A => Boolean): List[A] = foldLeft(Nil: List[A])((t,h) => if(f(h)) Cons(h, t) else t)
	
	def foldLeft[B](z: B)(f: (B,A) => B): B = this match {
		case Cons(h,t) => t.foldLeft(f(z, h))(f)
		case _ => z
	}
	
	/**
	  * Reverses order of a list
	  */
	def reverse: List[A] = foldLeft(List[A]())((b,a) => Cons(a,b))
	
	/**
	  * Returns last element in list.
	  */
	def last: Option[A] = this match {
		case Cons(h,t) if(t == Nil) => Some(h)
		case Cons(h,t) if(t != Nil) => t.last
		case _ => None
	}
	
	/**
	  * Returns first element in list.
	  */
	def headOption: Option[A] = this match {
		case Cons(h,_) => Some(h)
		case _ => None
	}
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
	
	def fill[A](n: Int, a: A): List[A] = {
		def _fill(n: Int, a: A, acc: List[A]): List[A] = n match {
			case x if(x > 0) => _fill(n - 1, a, Cons(a, acc))
			case _ => acc
		}
		_fill(n, a, Nil)
	}
}