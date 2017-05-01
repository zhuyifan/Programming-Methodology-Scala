import Solution._ 
import hw.tictactoe._

class Test extends org.scalatest.FunSuite {
	val game1=new Game(X, 3, Map((0,0)->O))
	val game2=new Game(O , 3, Map ())
	val game3=new Game(X , 3, Map ((0 , 0) -> X , (0 , 2) -> X , (2 , 2) -> O ))
	val game4=new Game(O , 4, Map ((0 , 0) -> X , (2 , 2) -> O , (3 , 3) -> X ))
	val game5=new Game(O, 3, Map((0,0)->X, (0,1)->X,(0,2)->X,(1,2)->O, (1,3)->O))
	val game6=new Game(X, 3, Map((0,0)->X, (0,1)->O,(0,2)->X,(1,0)->O,(1,1)->O,(1,2)->X,(2,0)->X,(2,1)->X,(2,2)->O))
	val game7=new Game(O, 3, Map((0,0)->X, (1,1)->X,(2,2)->X,(1,2)->O, (1,3)->O))
	val game8=new Game(X, 3, Map((1,0)->X, (2,0)->O,(0,1)->X,(1,1)->O,(0,2)->O,(2,2)->X))
	val game9= new Game(X,2, Map((0,0)->X))

	test("isfinished1"){
		assert(game1.isFinished()==false)
	}
	test("isfinished2"){
		assert(game2.isFinished()==false)
	}
	test("isfinished3"){
		assert(game3.isFinished()==false)
	}
	test("isfinished4"){
		assert(game4.isFinished()==false)
	}
	test("isfinished5 vertical"){
		assert(game5.isFinished()==true)
	}
	test("isfinished6 no winner"){
		assert(game6.isFinished()==true)
	}
	test("isfinished7 diagonal"){
		assert(game7.isFinished()==true)
	}
	test("isfinished8 diagonal"){
		assert(game8.isFinished()==true)
	}
	test("getWinner8"){
		assert(game8.getWinner()==Some(O))
	}
	test("getWinner7"){
		assert(game7.getWinner()==Some(X))
	}
	test("getWinner6"){
		assert(game6.getWinner()==None)
	}
	test("getWinner5"){
		assert(game5.getWinner()==Some(X))
	}
	/*test("nextBoards1"){
		assert(game1.nextBoards()==List())
	}
	test("nextBoards2"){
		assert(game2.nextBoards()==List())
	}
	test("nextBoards3"){
		assert(game3.nextBoards()==List())
	}*/

	test("nextBoards"){
		assert(game9.nextBoards()==List())
	}
	/*test("nextBoardsHelper"){
		assert(game9.nextBoardsHelper(2, Map((0,0)->X,(0,1)->X,(1,0)->O),0,0)==((1,1)))
	}*/

	

}