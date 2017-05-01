import hw.generics._

sealed trait BinTree[A] extends ListLike[A, BinTree[A]]

case class Node[A](lhs: BinTree[A], value: A, rhs: BinTree[A]) extends BinTree[A] {
	
	def consHelper(h: A, l: BinTree[A], v: A, r: BinTree[A]): BinTree[A] = l match{
		case Leaf() => Node[A](Node[A](Leaf(), h, Leaf()), v, r)
		case Node(a,b,c) => Node(consHelper(h,a,b,c),v,r)
	}
	
	def cons(head: A):  BinTree[A] = consHelper(head, lhs, value, rhs)
	
	def headHelper(l: BinTree[A], v: A): Option[A] = l match{
		case Leaf() => Some(v)
		case Node(a,b,c) => headHelper(a,b)
	}
    def head(): Option[A] = headHelper(lhs, value)
    
    def isEmpty(): Boolean = false
    
    def tailHelper(l: BinTree[A], v: A, r: BinTree[A]): BinTree[A] = l match{
    	case Leaf() => r match{
    		 case Leaf() => Leaf()
    		 case Node(a,b,c) => Node(a,b,c)
    	}
    	case Node(a,b,c) => Node(tailHelper(a,b,c), v, r)
    } 
    
    def tail(): Option[BinTree[A]] = {
    	if(lhs==Leaf()) Some(rhs)
    	else Some(tailHelper(lhs, value,rhs))
    }
}
case class Leaf[A]() extends BinTree[A] {
	
	def cons(head: A): BinTree[A] = Node(Leaf(), head, Leaf())
	
	def head() : Option[A] = None;
	
	def isEmpty(): Boolean = true;
	
	def tail(): Option[BinTree[A]] = None
}

object ListFunctions {

	def filter[A,C<:ListLike[A,C]](f:A=> Boolean, alist: C):C = {
		if(alist.isEmpty()) {
			alist
		}
		else if(f(alist.head().get)==true) {
			filter[A,C](f,alist.tail().get).cons(alist.head.get)
		}
		else {
			filter[A,C](f,alist.tail().get)
		}
	}

	def append[A, C<:ListLike[A,C]](alist1:C , alist2: C): C = {
		if(alist1.isEmpty()) {
			alist2
		}
		else if(alist2.isEmpty()) {
			alist1
		}
		else {
			append[A,C](alist1.tail().get, alist2).cons(alist1.head().get)
		}
	}

	case class IntLike(i:Int) extends Ordered[IntLike]{
		def compare(other:IntLike): Ordering={
			if(this.i == other.i){
				EQ
			}
			else if(this.i< other.i){
				LT
			}
			else {
				GT
			}
		}
	}
 
	
	def insert[A <: Ordered[A], C <: ListLike[A, C]](alist: C, x: A): C = {
		if(alist.isEmpty())alist.cons(x)
		else if(x.compare(alist.head().get)==LT){
			alist.cons(x)
		}
		else{
			insert[A,C](alist.tail().get, x).cons(alist.head().get)
		}
	}

	def sort[A <: Ordered[A], C <: ListLike[A, C]](alist: C): C = {
		if(alist.tail().get.isEmpty()){
			alist
		}
		else if(alist.tail().get.tail().get.isEmpty()){
			if(alist.head().get.compare(alist.tail().get.head().get)==LT){
				alist
			}
			else{
				alist.tail().get.tail().get.cons(alist.head().get).cons(alist.tail().get.head().get)
			}	
		}
		else {
			insert[A,C](sort[A,C](alist.tail().get),alist.head().get)
		}
}
}

class C1 extends T2[Int, Int, String, String] with T3[Int, Int, Int, String, String, String,Int]{
	// Do not change the class body. Simply extend T1, T2, and/or T3.
	def f(a: Int, b: Int): Int =0
	def g(c: String): String = ""
	def h(d:String): Int = 0
}

class C2 extends T1[Int, Int] with T2[Int, Int, Int, Int] with T3[Int,Int, Int, Int,Int,Int,Int]{
	// Do not change the class body. Simply extend T1, T2, and/or T3. 
	def f(a: Int, b: Int): Int = 0
	def g(c: Int): Int = 0
	def h(d: Int): Int = 0
}
class C3[A](x: A) extends T3[Int, A, Int, A, String, String, A]{
	// Do not change the class body. Simply extend T1, T2, and/or T3. 
	def f(a: Int, b: A): Int = 0
	def g(c: A): String = ""
	def h(d: String): A = x
}
class C4[A](x: Int , y: C4[A])extends T1[Int, C4[A]] with T3[Int, C4[A], C4[A], Int, C4[A], C4[A], Int]{
	// Do not change the class body. Simply extend T1, T2, and/or T3. 
	def f(a: Int, b: C4[A]): C4[A] = b
	def g(c: Int): C4[A] = y
	def h(d: C4[A]): Int = x
}

// def filter[A](f:A=> Boolean, alist:BinTree[A]): List[A]=alist match{
// 		case Nil => Nil
//     	case a::b::c => {
//     					if(f(a)){
//     						a::b::c
//     					} 
//     					else b::c
//     				}
//     	case a::Nil => {
//     					if(f(a)){
//     						a::Nil
//     					}
//     					else Nil
//     				}
// }
// 	def append[A](alist1:List[A] , alist2: List[A]): List[A] = alist1 match{
// 		case a::b => a::append[A](b, alist2)
// 		case Nil => alist2 match{
// 			case c::d => c::append[A](Nil, d)
// 			case Nil => Nil
// 		}
// 	}

	// def sort[A](list:List[A]):List[A]= list match{
	// 	case Nil => Nil
	// 	case a::b::c=>{
	// 		if(a>b){
	// 		b::sort(a::c)
	// 	}
	// 	else {
	// 		a::sort(b::c)
	// 	}
	// }
	// 	case a::Nil=>a::Nil
	// }

// def sort[A <: Ordered[A], C <: ListLike[A, C]](alist: C): C = alist match{
// 		case Empty()=> alist
// 		case a.cons(b.cons(c))=>{
// 			if(a.compare(b)==true){
// 				sort(c.cons(a)).cons(b)
// 			}
// 			else sort(c.cons(b)).cons(a)
// 		}
// 		case a.cons(Empty()) =>a.cons(Empty())
// 	}

// else if((alist.head().get.compare(alist.tail().get.head().get))==LT){
// 			sortHelper[A,C](alist.tail().get).cons(alist.head.get)
// 		}
// 		else sortHelper[A,C](alist.tail().get.tail().get.cons(alist.head.get)).cons(alist.tail().get.head().get)
// 	}