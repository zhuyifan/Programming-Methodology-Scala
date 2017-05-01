import hw.tictactoe._

class Game (turn : Player , dim : Int , board : Map [( Int , Int ) , Player ]) extends GameLike [ Game ] {

def isFinishedHelper(dim: Int,  board : Map [( Int , Int ) , Player ]): Boolean = {
	if(board.size == dim*dim) true
	else {
		Xwin(dim,playerX(board))||Xwin1(dim,playerX(board))||Xwin2(dim,playerX(board))||Xwin3(dim,playerX(board))||Owin(dim,playerO(board))||Owin1(dim,playerO(board))||Owin2(dim,playerO(board))||Owin3(dim,playerO(board))
	}
}

def getTurn(): Player={
	turn
}

def playerX(board: Map [( Int , Int ) , Player ]): List[((Int,Int), Player)]= {
	board.filter((t) => t._2 == X).toList
}

def playerO(board: Map [( Int , Int ) , Player ]): List[((Int, Int), Player)] = {
	board.filter((t) => t._2==O).toList
}

def Xwin(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0): Boolean  = {
	if(list.filter(a=>a._1._2==i).size==dim){
		true
	}
	else if(i==dim-1) false
	else Xwin(dim, list, i+1)
}

def Xwin1(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0): Boolean  = {
	if(list.filter(a=>a._1._1==i).size==dim){
		true
	}
	else if(i==dim-1) false
	else Xwin1(dim, list, i+1)
}

def Xwin2(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0): Boolean  = {
	if(list.contains(((i,i),X))){
		if(i==dim-1){
			true
		}
		else Xwin2(dim, list, i+1)
	}
	else false
}

def Xwin3(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0, j:Int=dim-1): Boolean  = {
	if(list.contains(((i,j),X))){
		if(i==dim-1){
			true
		}
		else Xwin3(dim, list, i+1,j-1)
	}
	else false
}

def Owin(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0): Boolean  = {
	if(list.filter(a=>a._1._2==i).size==dim){
		true
	}
	else if(i==dim-1) false
	else Owin(dim, list, i+1)
}

def Owin1(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0): Boolean  = {
	if(list.filter(a=>a._1._1==i).size==dim){
		true
	}
	else if(i==dim-1) false
	else Owin1(dim, list, i+1)
}

def Owin2(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0): Boolean  = {
	if(list.contains(((i,i), O))){
		if(i==dim-1){
			true
		}
		else Owin2(dim, list, i+1)
	}
	else false
}

def Owin3(dim: Int, list: List [(( Int , Int ) , Player)], i:Int=0, j:Int=dim-1): Boolean  = {
	if(list.contains(((i,j),O))){
		if(i==dim-1){
			true
		}
		else Owin3(dim, list, i+1,j-1)
	}
	else false
}



def isFinished (): Boolean = {
	isFinishedHelper(dim, board)
}



/* Assume that isFinished is true */
def getWinner (): Option [ Player ] = {
	if(Xwin(dim,playerX(board))||Xwin1(dim,playerX(board))||Xwin2(dim,playerX(board))||Xwin3(dim,playerX(board))) Some(X)
	else if(Owin(dim,playerO(board))||Owin1(dim,playerO(board))||Owin2(dim,playerO(board))||Owin3(dim,playerO(board))) Some(O)
	else None
}

def nextBoardsHelper(dim : Int , board : Map [( Int , Int ) , Player ], i: Int,j:Int): List[(Int,Int)] ={
	if(j==dim-1&&i==dim-1) {
		if(board.exists(a=>a._1 == (i,j))){
			Nil
		}
		else (i,j)::Nil
	}
	else if(j==dim-1){
		if(board.exists(a=>a._1 == (i,j))){
			nextBoardsHelper(dim,board,i+1,j)
		}
		else{
			(i,j)::nextBoardsHelper(dim,board,i+1,j)
		}
	}
	else if(i==dim-1){
		if(board.exists(a=>a._1 == (i,j))){
			nextBoardsHelper(dim,board,0,j+1)
		}
		else{
			(i,j)::nextBoardsHelper(dim,board,0,j+1)
		}
	}
	else if(board.exists(a=>a._1 == (i,j))){
		nextBoardsHelper(dim, board, i+1,j)
	}
	else{
		(i,j)::nextBoardsHelper(dim, board,i+1,j)
	}
}

def nextBoardsHelper1(turn: Player): Player={
	if(turn==X)O
	else X
}

def nextBoardsHelper2(turn : Player , dim : Int , board : Map [( Int , Int ) , Player ], list: List[(Int,Int)], i:Int=0): List[Game]={
	if(i<list.length){
		new Game(nextBoardsHelper1(turn), dim, board+(list(i)->turn))::nextBoardsHelper2(turn,dim,board,list,i+1)
	}
	else Nil
}

def nextBoards (): List [ Game ] = {
	nextBoardsHelper2(turn,dim,board,nextBoardsHelper(dim, board,0,0))
}
}

object Solution extends MinimaxLike {
type T = Game // T is an " abstract type member " of MinimaxLike

def createGame ( turn : Player , dim : Int , board : Map [( Int , Int ) , Player ]): Game = {
	if(dim<=2) throw new Exception("dim less than 2")
	else new Game(turn, dim, board)
}

def minimax ( board : Game ): Option [ Player ] = {
	if(board.getTurn==X){
		if(board.getWinner()==Some(X)) Some(X)
		else if(board.getWinner()==None) None
		else {
			val a=(board.nextBoards().map(game=>minimax(game)))
			if(a.contains(Some(X))){
				Some(X)
			}
			else if(a.contains(None)){
				None
			}
			else Some(O)
		}
	}
	else if(board.getTurn==O){
		
		if(board.getWinner()==Some(O)) Some(O)
		else if(board.getWinner()==None) None
		else {
			val b=(board.nextBoards().map(game=>minimax(game)))
			if(b.contains(Some(O))){
				Some(O)
			}
			else if(b.contains(None)){
				None
			}
			else Some(X)
		}
	}
	else None
	

}
}




/*def Xwin1(dim: Int, list: List [(( Int , Int ) , Player)]): Boolean  = {
	if(list.contains(((dim-1,0),X))){
		if((dim-1)==0){
			true
		}
		else Xwin1(dim-1, list)
	}
	else false
}

def Xwin2(dim: Int, list: List [(( Int , Int ) , Player)]): Boolean  = {
	if(list.contains(((0,dim-1),X))){
		if((dim-1)==0){
			true
		}
		else Xwin2(dim-1, list)
	}
	else false
}*/
/*if(board.isFinished ()){
		board.getWinner ()
	}
	else if(i==board.nextBoards.length) None
	else minimaxHelper(board.nextBoards()(i),i+1)*/
