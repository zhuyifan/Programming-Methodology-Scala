object FunctionalDataStructures {

case class Queue[A](front: List[A], back: List[A])

def enqueueHelper[A](elt: A, b: List[A]): List[A] = b match{
	case Nil => List(elt)
	case head :: tail  => elt::head::tail
}

def enqueue[A](elt: A, q: Queue[A]): Queue[A] = q match{
	case Queue(Nil,Nil) => new Queue[A](Nil, List(elt))
	case _ => new Queue[A](q.front , enqueueHelper(elt, q.back))
}

def dequeueHelper1[A](q: Queue[A]): Option[(A, Queue[A])] = q.front match{
	case Nil => dequeueHelper2(q.back.reverse)
	case head :: tail => Some((head, new Queue(tail, q.back)))
} 

def dequeueHelper2[A](a: List[A]): Option[(A, Queue[A])] = a match{
	case Nil => None
	case head :: tail => Some((head, new Queue(tail, Nil))) 
} 

def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = {
	dequeueHelper1(q)
}

sealed trait JoinList[A] { 
	val size: Int
}

case class Empty[A]() extends JoinList[A] { 
	val size = 0
}

case class Singleton[A](elt: A) extends JoinList[A] { 
	val size = 1
}

case class Join[A](lst1: JoinList[A], lst2: JoinList[A], size: Int) extends JoinList[A]  

def maxHelper[A](lst: JoinList[A], compare: (A, A) => Boolean):Option[A] = lst match{
	case Empty() => None
	case Singleton(elt) => Some(elt)
	case Join(lst1, lst2, _) => (maxHelper(lst1, compare), maxHelper(lst2, compare)) match{
  		case (Some(elt), None) => Some(elt)
  		case (None, Some(elt)) => Some(elt)
  		case (None, None) => None
  		case (Some(elt1), Some(elt2)) => {
  			if(compare(elt1,elt2) == true){
  				Some(elt1)
  			}
  			else{
  				Some(elt2)
  			}
  		}
    }
}

def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = lst match{
	case Empty() => None
	case _ => maxHelper(lst, compare)
}


def first[A](lst: JoinList[A]): Option[A] = lst match{
	case Empty() => None
	case Singleton(elt) => Some(elt)
	case Join(Empty(), lst2, _) => first(lst2)
	case Join(lst1, lst2, _) => if(lst1.size == 0){
		first(lst2)
	}
	else{
	first(lst1)
}
}

def restHelper[A](lst: JoinList[A]): JoinList[A] = lst match{
	case Empty() => Empty()
	case Singleton(elt) => Empty()
	case Join(Empty(), lst2, x) => Join(Empty(), restHelper(lst2), x-1)
	case Join(lst1, lst2, x) => if(lst1.size == 0){
		Join(lst1, restHelper(lst2), x-1)
	}
	else{
	Join(restHelper(lst1), lst2, x-1)
}
}

def rest[A](lst: JoinList[A]): Option[JoinList[A]] = lst match{
	case Empty() => None
	case _ => if(restHelper(lst) == Join(Empty(),Empty(), 0)){
		Some(Empty())
	}
	else{
	Some(restHelper(lst))
}
}

def nthHelper[A](lst: JoinList[A], n: Int): JoinList[A] = lst match{
	case Empty() => Empty()
	case _ => if(n == 0){
		lst
	}
	else{
		nthHelper(restHelper(lst), n-1)
	}
}


def nth[A](lst: JoinList[A], n: Int): Option[A] = lst match{
	case Empty() => None
	case _ => first(nthHelper(lst, n))
}


def map[A,B](f: A => B, lst: JoinList[A]): JoinList[B] = lst match{
	case Empty() => Empty()
	case Singleton(elt) => Singleton(f(elt))
	case Join(lst1, lst2, x) => Join(map(f, lst1), map(f, lst2), x)
}


def filter[A](pred: A => Boolean, lst: JoinList[A]): JoinList[A] = lst match{
	case Empty() => Empty()
	case Singleton(elt) => 
		if(pred(elt)==true){
			Singleton(elt)
		}
		else{
			Empty()
		}
	case Join(lst1, lst2, x) => Join(filter(pred, lst1), filter(pred, lst2), x)
}

}