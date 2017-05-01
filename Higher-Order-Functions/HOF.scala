object HOF {
	def mergeList[A](alist1: List[A], alist2: List[A]): List[A]= alist1 match{
		case Nil => alist2
		case a::Nil => a::mergeList(alist2, Nil)
		case a::b => a::mergeList(b, alist2)
		
	}

	def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = lst1 match{
		case Nil => Nil
		case a::b => lst2 match{
			case Nil => Nil
			case c::d => f(a,c)::map2[A,B,C](f,b,d)
		}
	}	//1

	def zip[A,B](alist1: List[A], alist2: List[B]): List[(A, B)] = alist1 match{
		case Nil => Nil
		case a::b => alist2 match{
			case Nil => Nil
			case c::d => (a, c)::zip[A,B](b, d)
		}
	}	//2

	def flatten[A](alist: List[List[A]]): List[A] = alist match{
		case Nil => Nil
		case a::b => mergeList(a,flatten(b))
	}
	//3

	def flatten3[A](alist: List[List[List[A]]]): List[A] = alist match{
		case Nil => Nil
		case a::b => mergeList(flatten(a), flatten3(b))
	}
	//4

	def buildListlHeler[A](index1: Int, f: Int => A, index2: Int = 0): List[A] = index1 match {
		case 0 => Nil
		case other => f(index2)::buildListlHeler[A](index1-1, f, index2+1)
	}
	def buildList[A](length: Int, f: Int => A): List[A] = {
		buildListlHeler(length, f)
	}	//5

	def mapList[A, B](alist: List[A], f: A => List[B]): List[B] = alist match{
		case Nil => Nil
		case a::b => mergeList(f(a), mapList(b, f))
	}
	//6

	def partitionHelper1[A](f: A => Boolean, list: List[A]): List[A] = list match{
		case Nil => Nil
		case a::b =>{
			f(a) match{
				case true => partitionHelper1(f, b)
				case false => a::partitionHelper1(f, b)
			}
		}
	}

	def partitionHelper2[A](f: A => Boolean, list: List[A]): List[A] = list match{
		case Nil => Nil
		case a::b =>{
			f(a) match{
				case false => partitionHelper2(f, b)
				case true => a::partitionHelper2(f, b)
			}
		}
	}

	def partition[A](f: A => Boolean, alist: List[A]): (List[A], List[A]) = {
		(partitionHelper2(f, alist), partitionHelper1(f, alist))
	}	//7

	def mergeHelper1[A](alist1: List[A], alist2: List[A], num: Int=1): List[A] = {
		if(num==1) alist1 match{
			case Nil => Nil
			case n :: tail => n :: mergeHelper1(tail, alist2, num*(-1))
		}
		else alist2 match{
			case Nil => Nil
			case n :: tail => n :: mergeHelper1(alist1, tail, num*(-1))
		}
	}

	def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = {
		sort[A](lessThan, mergeHelper1[A](alist1, alist2))
	}//8

	def insert[A](lessThan: (A, A) => Boolean, x: A, alist: List[A]): List[A] = alist match {
		case Nil => List(x)
		case hd :: tl => {
			if (lessThan(x, hd)) { 
				x :: hd :: tl
			}
			else {
				hd :: insert(lessThan ,x ,tl)
			}	
		}
	}

	def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = alist match { 
		case Nil => Nil
		case hd :: tl => insert(lessThan , hd, sort(lessThan , tl))
	}//9


		
}