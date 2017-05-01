import FunctionalDataStructures._

class Tests extends org.scalatest.FunSuite { 


	val Q1 = Queue(List(1,2,3),List(1,2,3))
	val Q2 = Queue(List(1,2,3), Nil)
	val Q3 = Queue(Nil, List(1,2,3))
	val Q4 = Queue[Int](Nil, Nil)

	test("enqueue"){
		assert(enqueue(8, Q1) == Queue(List(1, 2, 3),List(8, 1, 2, 3)))
	}

	test("enqueue1"){
		assert(enqueue(1, Q2) == Queue(List(1,2,3), List(1)))
	}

	test("enqueue2"){
		assert(enqueue(8, Q3) == Queue(Nil, List(8,1,2,3)))
	}

	test("euqueue3"){
		assert(enqueue(1, Q4) == Queue(Nil, List(1)))
	}

	test("dequeue"){
		assert(dequeue(Q1) == Some(1, Queue(List(2,3),List(1,2,3))))
	}

	test("dequeue1"){
		assert(dequeue(Q2) == Some(1, Queue(List(2,3), Nil)))
	}

	test("dequeue2"){
		assert(dequeue(Q3) == Some(3, Queue(List(2,1), Nil)))
	}

	test("dequeue3"){
		assert(dequeue(Q4) == None)
	}

	/*def fromList[A](alist: List[A]): JoinList[A] = alist match { 
		case Nil => Empty()
		case List(x) => Singleton(x)
		case _ => {
			val len = alist.length
			val (lhs, rhs) = lst.splitAt(len / 2) 
			Join(fromList(lhs), fromList(rhs), len)
		} 
	}

	def toList[A](alist: JoinList[A]): List[A] = lst match { 
		case Empty() => Nil
		case Singleton(x) => List(x)
		case Join(alist1 , alist2 , _) =>
		}*/

	val J0 = Join(Empty(), Empty(), 0)
	val J1 = Join(Singleton(3), Join(Singleton(4),Empty(), 1), 2)
	val J2 = Join(Empty(), Join(Singleton(1), Singleton(2), 2), 2)
	val J3 = Join(Join(Empty(), Singleton(5), 1), Join(Singleton(6), Singleton(7), 2), 3)
	val J4 = Join(Join(Singleton(1), Singleton(2), 2), Join(Join(Empty(), Singleton(3), 1), Join(Singleton(4), Singleton(5), 2),3),5)

	test("first"){
		assert(first(J1) == Some(3))
	}

	test("first1"){
		assert(first(J2) == Some(1))
	}

	test("first2"){
		assert(first(J3) == Some(5))
	}

	test("first3"){
		assert(first(J0) == None)
	}

	test("first4"){
		assert(first(J4) == Some(1))
	}

	test("rest"){
		assert(rest(J1) ==  Some(Join(Empty(),Join(Singleton(4),Empty(),1),1)))
	}
	
	test("rest1"){
		assert(rest(J2) == Some(Join(Empty(),Join(Empty(),Singleton(2),1),1)))
	}

	test("rest2"){
		assert(rest(J3) ==  Some(Join(Join(Empty(),Empty(),0),Join(Singleton(6),Singleton(7),2),2)))
	}

	test("rest2.5"){
		assert(rest(restHelper(J3)) ==  Some(Join(Join(Empty(),Empty(),0),Join(Empty(),Singleton(7),1),1)))
	}

	test("rest3"){
		assert(first(J0) == None)
	}

	test("nth"){
		assert(nth(J0, 2) == None)
	}

	test("nth1"){
		assert(nth(J1, 1) == Some(4))
	}

	test("nth2"){
		assert(nth(J2, 0) == Some(1))
	}

	test("nth3"){
		assert(nth(J3, 0) == Some(5))
	}

	test("nth4"){
		assert(nth(J3, 1) == Some(6))
	}

	test("nth5"){
		assert(nth(J3, 2) == Some(7))
	}

	def compare(a: Int, b: Int):Boolean ={
		if(a>b){
			true
		}
		else{
			false
		}
	} 

	test("max"){
		assert(max(J4,compare) == Some(5))
	}

	def fi(a: Int): Boolean = {
		if(a%2 == 0){
			true
		}
		else{
			false
		}
	}

	test("filter"){
		assert(filter(fi, J3)== Empty())
	}

	def fromList [ A ]( alist : List [A ]): JoinList [A ] = alist match {
	case Nil => Empty ()
	case List ( x) => Singleton ( x)
	case _ => {
	val len = alist . length
	val ( lhs , rhs ) = alist . splitAt ( len / 2)
	Join ( fromList ( lhs ), fromList ( rhs ) , len )
	}
}
def toList [A ]( alist : JoinList [ A ]): List [A] = alist match {
	case Empty () => Nil
	case Singleton ( x) => List ( x)
	case Join ( alist1 , alist2 , _) => toList ( alist1 ) ++ toList ( alist2 )
}
	val a=Queue(List(1,2),List(4,3))
	val aJoinList= Join(Join(Singleton(1),Singleton(2),2),Singleton(3),3)

test ("test enqueue"){
	assert(enqueue(5,a) == Queue(List(1,2),List(5,4,3)))
}

test ("test enqueue Empty"){
	val a=Queue[Int](List(),List())
	assert(enqueue(5,a) == Queue(List(),List(5)))
}

test("reverse"){
	val alist = List(1,2,3)
	assert(alist.reverse==List(3,2,1))
}
test ("test dequeue from empty"){
	val a=Queue[Any](List(),List())
	assert(dequeue(a) == None)
}

test ("test dequeue from front empty"){
	val a=Queue(List(),List(5,4,3))
	assert(dequeue(a) == Some(3,Queue(List(4, 5),List())))

}
test ("test first"){
	assert(first(aJoinList)==Some(1))
}


test ("test firstEmpty"){
	val aJoinList= Join(Empty(),Empty(),0)
	assert(first(aJoinList)==None)
}

test ("test first-OnlyOne"){
	val aJoinList= Join(Empty(),Singleton(1),1)
	assert(first(aJoinList)==Some(1))
}

test ("test rest1"){
	val alist= List(2,3)
	val blist=toList(rest(aJoinList).get)
	assert(alist==blist)

}

test ("test rest2"){
	val aJoinList= Join(Join(Singleton(1),Singleton(2),2),Join(Singleton(3),Singleton(4),2),4)
	val alist= List(2,3,4)
	val blist=toList(rest(aJoinList).get)
	assert(alist==blist)
	//assert(rest(twoElt) == Some(fromList(List(2))))

}
test("test restEmpty"){
	val aJoinList= Join(Empty(),Singleton(1),1)
	assert(rest(aJoinList)==Some(Empty()))
// Question Mark
}

test("test restEmptySecond"){
	val aJoinList= Join(Join(Singleton(1),Singleton(2),2),Empty(),2)
	assert(toList(rest(aJoinList).get)==List(2))
}

test("test rest3"){
	val aJoinList= Join(Empty(),Join(Singleton(1),Singleton(2),2),2)
	assert(toList(rest(aJoinList).get)==List(2))
}

test("test filter"){
	val aJoinList= Join(Join(Singleton(1),Singleton(2),2),Join(Singleton(0),Singleton(8),2),4)
	def check(x : Int):Boolean  = (x >1)
	assert(toList(filter(check,aJoinList))==List(2,8))
	val bJoinList= Join(Join(Singleton(8),Singleton(2),2),Join(Singleton(0),Singleton(7),2),4)
	assert(toList(filter(check,bJoinList))==List(8,2,7))
}
test("test filter1"){
	val aJoinList= Join(Join(Singleton(1),Singleton(2),2),Empty(),2)
	def check(x : Int):Boolean  = (x >1)
	assert(toList(filter(check,aJoinList))==List(2))
}

test("test map"){
	val aJoinList= Join(Join(Singleton(1),Singleton(2),2),Join(Singleton(0),Singleton(8),2),4)
	def double(x : Int):Int  = x*2
	assert(toList(map(double,aJoinList))==List(2,4,0,16))
	val bJoinList= Join(Join(Singleton(8),Singleton(2),2),Join(Singleton(0),Singleton(7),2),4)
	assert(toList(map(double,bJoinList))==List(16,4,0,14))
}

test("test mapEmpty"){
	val aJoinList= Join(Empty(),Join(Singleton(1),Singleton(2),2),2)
	def double(x : Int):Int  = x*2
	assert(toList(map(double,aJoinList))==List(2,4))
}

test("test nth"){
	val aJoinList= Join(Join(Singleton(1),Singleton(2),2),Empty(),2)
	assert(nth(aJoinList,1)==Some(2))
}

test("test nth2"){
	val aJoinList= Join(Empty(),Join(Singleton(1),Singleton(2),2),2)
	assert(nth(aJoinList,1)==Some(2))
	val bJoinList= Join(Join(Singleton(8),Singleton(2),2),Join(Singleton(0),Singleton(7),2),4)
	assert(nth(bJoinList,3)==Some(7))
	val cJoinList= Join(aJoinList,bJoinList,6)
	assert(nth(cJoinList,3)==Some(2))
}

test("test nthNone"){
	val aJoinList= Join(Empty(),Join(Singleton(1),Singleton(2),2),2)
	assert(nth(aJoinList,-7)==None)
}

test("test Max"){
	val aJoinList= Join(Join(Singleton(1),Singleton(5),2),Join(Singleton(0),Singleton(8),2),4)
	def compare(x: Int, y:Int): Boolean = (x>y)
	assert(max(aJoinList,compare)==Some(8))
}

}

