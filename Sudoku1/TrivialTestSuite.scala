import Solution._
class TrivialTestSuite extends org.scalatest.FunSuite {
	
	val a0 = "8................................................................................"
	val a1 = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
	val a2 = ".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2"
	val a3 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
	val a4 = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
	val b = new Board(parseHelper(0))
	
	// test("The solution object must be defined") { 
	// 	val obj : hw.sudoku.SudokuLike = Solution
	// } 

	//  test("peers"){
	//  	assert(peers(1,2)== List((1,0), (0,2), (1,1), (2,2), (1,3), (3,2), (1,4), (4,2), (1,5), (5,2), (1,6), (6,2), (1,7), (7,2), (1,8), (8,2), (0,1), (0,0), (2,1), (2,0)))
	//  }

	//  test("place"){
	//  	assert(b.place(5,6,9).place(1,2,3).place(7,8,8)==b)
	//  }

	//  test("valueat"){
	//  	assert(b.place(5,6,9).valueAt(5,6)==None)
	//  }

	//  test("isSolved"){
	//  	assert(b.place(5,6,8).isSolved()==false)
	//  }

	//  test("isuncolvable"){
	//  	assert(b.place(5,6,7).isUnsolvable()==false)
	//  }


	//  test("parse"){
	//  	assert(parse(a2)==b)
	//  }

	 // test("nextState"){
	 // 	assert(parse("85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58").nextStates==List())
	 // }

	//  test("s"){
	//  	assert(parse("85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58").solve()==None)
 // 	}

 // 	test("p1"){
 // 		assert(parse(a2)==new Board(Map()))
 // 	}

 // 	test("p2"){
 // 		assert(parse(a2).place(0,1,2)==new Board(Map()))
 // 	}
 // 	test("solvee1"){
 // 		assert(parse("85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58").solve()==None)
 // 	}

 	test("solve1"){
 		assert(parse(a1).solve()==None)
 	}
 		test("solve2"){
 		assert(parse(a2).solve()==None)
 	}
 		test("solve3"){
 		assert(parse(a3).solve()==None)
 	}
 		test("solve4"){
 		assert(parse(a4).solve()==None)
 	}
}