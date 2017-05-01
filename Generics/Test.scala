import ListFunctions._ 
import hw.generics._


class Test extends org.scalatest.FunSuite {
	val node1 = new Node(Leaf(), 1, Leaf())
	val node2 = new Node(Node(Node(Leaf(),1,Leaf()),2,Leaf()),3,Node(Leaf(),4,Leaf()))
	val node3 = new Node(Node(Leaf(),1,Node(Leaf(),2,Leaf())),3,Node(Leaf(),4,Leaf()))
	val node4 = new Node(Node(Leaf(),IntLike(2),Leaf()), IntLike(1), Leaf())
	test("test Node cons"){
		assert(node1.cons(2)==Node(Node(Leaf(), 2, Leaf()),1,Leaf()))
	}
	test("test Node head"){
		assert(node1.head()==Some(1))
	}
	test("test Node head1"){
		assert(node2.head()==Some(1))
	}
	test("test Node isEmpty"){
		assert(node1.isEmpty()==false)
	}
	test("test Node tail"){
		assert(node1.tail()==Some(Leaf()))
	}
	test("test Node tail1"){
		assert(node2.tail()==Some(Node(Node(Leaf(),2,Leaf()),3,Node(Leaf(),4,Leaf()))))
	}
	test("test Node tail2"){
		assert(node3.tail()==Some(Node(Node(Leaf(),2,Leaf()), 3, Node(Leaf(),4,Leaf()))))
	}
	def isEven(x: Int): Boolean = x % 2 == 0
	test("filter"){
		assert(filter[Int, BinTree[Int]](isEven,node2)==Node(Node(Leaf(),2,Leaf()),4,Leaf()))
	}
	test("append"){
		assert(append[Int,BinTree[Int]](node1,node2)== Node(Node(Node(Node(Leaf(),1,Leaf()),1,Leaf()),2,Leaf()),3,Node(Leaf(),4,Leaf())))
	}
	test("append1"){
		assert(append[Int,BinTree[Int]](node2,node1)== Node(Node(Node(Node(Node(Leaf(),1,Leaf()),2,Leaf()),3,Leaf()),4,Leaf()),1,Leaf()) )
	}

	val node5 = new Node(Node(Leaf(),IntLike(4),Node(Leaf(),IntLike(1),Leaf())),IntLike(3),Node(Leaf(),IntLike(2),Leaf()))

	
	test("sort"){
	  	assert(sort[IntLike, BinTree[IntLike]](node4)==Node(Node(Leaf(),IntLike(1),Leaf()), IntLike(2), Leaf()))
	  }

	 test("sort1"){
	  	assert(sort[IntLike, BinTree[IntLike]](node5)== Node(Node(Node(Node(Leaf(),IntLike(1),Leaf()),IntLike(2),Leaf()),IntLike(3),Leaf()),IntLike(4),Leaf()))
	  }

}