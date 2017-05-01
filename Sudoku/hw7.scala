import hw.sudoku._

object Solution extends SudokuLike {

type T = Board

def parseHelper(a: Int):Map[(Int, Int), List[Int]] = {
		if(a==9){
			Map()
		}
		else{
			Map((a,0)->List(1,2,3,4,5,6,7,8,9))+((a,1)->List(1,2,3,4,5,6,7,8,9))+((a,2)->List(1,2,3,4,5,6,7,8,9))+((a,3)->List(1,2,3,4,5,6,7,8,9))+((a,4)->List(1,2,3,4,5,6,7,8,9))+((a,5)->List(1,2,3,4,5,6,7,8,9))+((a,6)->List(1,2,3,4,5,6,7,8,9))+((a,7)->List(1,2,3,4,5,6,7,8,9))+((a,8)->List(1,2,3,4,5,6,7,8,9))++parseHelper(a+1)
		}
}

// def parseHelper1(str:String):Map[(Int, Int), List[Int]] = str.toList match{
// 	case Nil => Map()
// 	case a::b => if(a=='.'){
// 		a::parseHelper1(b)
// 	}
// 		else{

// 		}
// }

def parseHelper1(row: Int, col: Int, str: List[Char], board: Board): Board=str match{
	case Nil => board
	case a::b => if(row!=8){
			if(a=='.'){
				parseHelper1(row+1, col, b, board)
			}
			else{
				parseHelper1(row+1, col, b, board.place(row, col, a.asDigit))
			}
		}
		else{
			if(a=='.'){
				parseHelper1(0, col+1, b, board)
			}	
			else{
				parseHelper1(0, col+1, b, board.place(row, col, a.asDigit))
			}
		}
}

def parse(str: String): Board = {
	parseHelper1(0, 0, str.toList, new Board(parseHelper(0)))
}
// You can use a Set instead of a List (or, any Iterable)

def peersHelper(row: Int, col: Int, num: Int=0): List[(Int,Int)] = {
	if(num!=9)(row,num)::(num,col)::peersHelper(row,col,num+1) 
	else peersHelper1(row, col)
}

def peersHelper1(row: Int, col: Int): List[(Int,Int)] = {
	if(row%3==2){
		if(col%3==2)List((row,col-1),(row,col-2),(row-1,col),(row-1,col-1),(row-1,col-2),(row-2,col),(row-2,col-1),(row-2,col-2))
		else if(col%3==0)List((row,col+1),(row,col+2),(row-1,col),(row-1,col+1),(row-1,col+2),(row-2,col),(row-2,col+1),(row-2,col+2))
		else List((row,col-1),(row,col+1),(row-1,col),(row-1,col-1),(row-1,col+1),(row-2,col),(row-2,col-1),(row-2,col+1))
	}
	else if(row%3==0){
		if(col%3==2)List((row,col-1),(row,col-2),(row+1,col),(row+1,col-1),(row+1,col-2),(row+2,col),(row+2,col-1),(row+2,col-2))
		else if(col%3==0)List((row,col+1),(row,col+2),(row+1,col),(row+1,col+1),(row+1,col+2),(row+2,col),(row+2,col+1),(row+2,col+2))
		else List((row,col-1),(row,col+1),(row+1,col),(row+1,col-1),(row+1,col+1),(row+2,col),(row+2,col-1),(row+2,col+1))
	}
	else{
		if(col%3==2)List((row,col-1),(row,col-2),(row-1,col),(row-1,col-1),(row-1,col-2),(row+1,col),(row+1,col-1),(row+1,col-2))
		else if(col%3==0)List((row,col+1),(row,col+2),(row-1,col),(row-1,col+1),(row-1,col+2),(row+1,col),(row+1,col+1),(row+1,col+2))
		else List((row,col-1),(row,col+1),(row-1,col),(row-1,col-1),(row-1,col+1),(row+1,col),(row+1,col-1),(row+1,col+1))
	}
}

def peersHelper2(row: Int, col: Int, list: List[(Int,Int)]): List[(Int,Int)] = list match{
	case Nil => Nil
	case a::Nil => {
		if(a==(row, col)){
			Nil
		}
		else a::Nil
	}
	case a::b::c=>{
		if(a==(row, col)){
			b::peersHelper2(row, col, c)
		}
		else if(b==(row, col)){
			a::peersHelper2(row, col, c)
		}
		else a::b::peersHelper2(row, col, c)
	}
}

def peers(row: Int, col: Int): List[(Int, Int)] = {
	peersHelper2(row, col, peersHelper(row, col).distinct)
}
}
// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.

class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

def availableValuesAt(row: Int, col: Int): List[Int] = {
// Assumes that a missing value means all values are available. 
// Feel free to change this.
	available.getOrElse((row, col), 1.to(9).toList)
}

def valueAt(row: Int, col: Int): Option[Int] = {
	if(available.get(row,col).get.length==1){
		Some(available.get(row,col).get.head)
	}
	else None
}  

def isSolved (): Boolean = {
	available.toList.size==available.filter((t) => t._2.size == 1).toList.size
}

def isUnsolvable (): Boolean = {
	available.values.exists(_==List())
}

type T=Map[(Int, Int), List[Int]]

// def placeList(value: Int, coords: List[(Int,Int)], avail:T):T={
// 	if(coords==Nil){
// 		avail
// 	}
// 	else{
// 		val ((x,y)::tail)=coords
// 		val values=avail((x,y))
// 		placeList(value, tail, avail+((x,y)->((values.toSet-value).toList).sortWith(_.compareTo(_) < 0)))
// 	}
// }

// def placeRec(x: Int, y: Int, value: Int, avail: T):T={
// 	placeList(value,Solution.peers(x,y),avail)-((x,y))+((x,y)->List(value))
// }

//def placeHelper(list: List[(Int, Int)], value: Int, avail:T):T=list match{	
// 	case Nil => avail
// 	case a::b => {
// 		if(avail(a).contains(value)){
// 			if(avail(a).size==2){
// 				placeHelper(b, value, new Board(avail).place(a._1, a._2, (avail(a).toSet-value).toList(0)).available)
// 			}
// 			else{
// 				placeHelper(b, value, avail)
// 			}
// 		}
			
// 		else{
// 			placeHelper(b, value, avail)
// 		}
// 	}
// }

// def helper(peer:List[(Int, Int)],value: Int, avail: T):T =peer match{
// 	case Nil => avail
// 	case a::b=>if(avail(a._1, a._2).contains(value)&&avail(a).size==2){
// 				place(a._1, a._2, ((avail(a).toSet-value).toList)(0)).available
// 			}
// 			else{
// 				helper(b,value,avail)
// 			}
// }

def placeHelper(row:Int,col:Int,list:List[(Int,Int)],value : Int, board :T) : T= list match {
   case Nil => board
   case a::b => if(board(a).contains(value)){
        val c = board + ((a._1,a._2)->(board(a._1,a._2).filterNot(x => x==value)))
        if(c(a._1,a._2).length == 1){
          placeHelper(row,col,b,value,placeHelper(a._1,a._2,Solution.peers(a._1,a._2),c(a._1,a._2)(0),c))
        }
        else {
        	placeHelper(row,col,b,value,c)
        }
        }
         else  {
         	placeHelper(row,col,b,value,board)
         }
 }


def place(row: Int, col: Int, value: Int): Board = { 
	require(availableValuesAt(row, col).contains(value))
	new Board(placeHelper(row, col, Solution.peers(row, col), value, available))
}
// You can return any Iterable (e.g., Stream)

// def nextStatesHelper0(row: Int, col: Int): List[Int] = {
// 	this.availableValuesAt(row, col)
// }

// def nextStatesHelper(row: Int, col: Int, list: List[Int]):List[Board] = list match{
// 	case Nil => Nil
// 	case a::b => if(this.place(row, col, a).isUnsolvable()){
// 			nextStatesHelper(row, col, b)
// 		}
// 		else this.place(row, col, a)::nextStatesHelper(row, col, b)
// }


// def mergeList(alist1: List[Board], alist2: List[Board]): List[Board]= alist1 match{
// 	case Nil => alist2
// 	case a::Nil => a::mergeList(alist2, Nil)
// 	case a::b => a::mergeList(b, alist2)
		
// 	}

// def nextStatesHelper1(row: Int=0, col: Int=0):List[Board] = {
// 	if(row!=8){
// 		mergeList(nextStatesHelper(row, col, nextStatesHelper0(row, col)),nextStatesHelper1(row+1, col))
// 	}
// 	else{
// 		if(col==8){
// 			nextStatesHelper(8, 8, nextStatesHelper0(8, 8))
// 		}
// 		else{
// 			mergeList(nextStatesHelper(row, col, nextStatesHelper0(row, col)),nextStatesHelper1(0, col+1))
// 		}
// 	}
// }

// def nextStates(): List[Board] = { 
// 	if (isUnsolvable()) {
// 		List() 
// 	}
// 	else { 
// 		nextStatesHelper1().toSet.toList.sortWith((left,right) => left.boardSize() < right.boardSize())
// 	} 
// }

def nextStates(): Stream[Board] = {
  if (isUnsolvable()){
 	 Stream[Board]()
	}
  else
      {
        val s = available.foldLeft(Stream[Board]())((x, y)=>
        	if(available(y._1).length == 1) {
          		x
          	}
        	else {
          		x ++: available(y._1).foldLeft(Stream[Board]())((d, e)=>
           	 	d :+ this.place(y._1._1, y._1._2, e))
          	})
        		s.sortBy(x => x.available.foldLeft(0)((a, b) => a + x.available(b._1).length))
    			
      }
}
// def solveHelper(list: List[Board]): Option[Board] = list match{
// 		case a::b =>{
// 		if(a.isSolved()){
// 			Some(a)
// 		}
// 		else {
// 				val s=a.nextStates().map(board=>board.solve)
// 				if(s.exists(x=>x.get.isSolved())){
// 					s.find(x=>x.get.isSolved()).get
// 				} 
// 				else solveHelper(b)
// 			}
// 		}
// 		case _=> None
// }

// def solve(): Option[Board] = {
// 	if(this.isSolved){
// 		Some(this)
// 	}
// 	else {	
// 		solveHelper(this.nextStates)
// 	}
// }



// def boardSize(row: Int =0, col: Int=0): Int = {
// 	if(row!=8){
// 		availableValuesAt(row, col).size+boardSize(row+1, col)
// 	}
// 	else{
// 		if(col==8){
// 			availableValuesAt(row, col).size
// 		}
// 		else{
// 			availableValuesAt(row, col).size+boardSize(0, col+1)
// 		}
// 	}
// }

// if(availableValuesAt(row, col).contains(value)){
// 		val a=Solution.peers(row, col)
// 		new Board(available.foreach((p)=>{
// 			if(a.contains(p._1)){
// 				((p._2).toSet-value).toList
// 			}
// 		})
// 		)
// 	} 
// 	else new Board(available)

def solveHelper (stream : Stream[Board]): Option[Board] = stream match{
	case a #:: b => {
    	val result = a.solve
    	if (result == None) {
    		solveHelper(b)
    	}
    	else {
			result
		}
 	}	
  	case _ => None
}

def solve(): Option[Board] = {
	if(isSolved == true) {
		Some(this)
	}
	else {
		solveHelper(this.nextStates)
	}
}

}