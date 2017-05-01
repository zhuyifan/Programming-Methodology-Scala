object Lists {
	val oddNumbers = 1 :: 3 :: 5 :: Nil 

	def sumDouble(alist: List[Int]): Int = {
		alist match{
		case Nil => 0
		case n::tail => n*2+sumDouble(tail)
		}	
	}

	def removeZeroes(alist: List[Int]): List[Int] = alist match{
		case Nil => Nil
		case 0 :: tail =>removeZeroes(tail)
		case n :: tail => n :: removeZeroes(tail)
	}

	def countEvens(alist: List[Int]): Int = alist match{
		case Nil => 0
		case n :: tail =>{
			if(n%2==0){
				1+countEvens(tail)
			}
			else{
				countEvens(tail)
			}
		}
	}

	def removeAlternating(alist: List[String]): List[String] = alist match{
		case Nil => Nil
		case x :: Nil => List(x)
		case x :: y :: z => x::removeAlternating(z)
	}

	def isAscending(alist: List[Int]): Boolean = alist match{
		case Nil => false
		case x :: Nil => true
		case x :: y :: z => {
			if(x > y){
				false
			}
			else{
				isAscending(y :: z)
			}
		}
	}

	def addSub(alist: List[Int]): Int = alist match{
		case Nil =>0
		case n :: tail =>{
			n-addSub(tail)
		}
	}

	def alternate(alist: List[Int], blist: List[Int], num: Int=1): List[Int] = {
		if(num==1) alist match{
			case Nil => Nil
			case n :: tail => n :: alternate(tail, blist, num*(-1))
		}
		else blist match{
			case Nil => Nil
			case n :: tail => n :: alternate(alist, tail, num*(-1))
		}
	}

	def fromTo(aint: Int, bint: Int): List[Int] = {
		if(aint==bint){
			Nil
		}
		else{
			aint :: fromTo(aint+1, bint)
		}
	}

	def insertOrdered(n: Int, lst: List[Int]): List[Int] = lst match{
		case Nil => n :: Nil
		case m :: Nil => m :: n :: Nil
		case m :: tail => {
			if(n <= m){
				n :: m ::tail
			}
			else{
				m :: insertOrdered(n, tail)
			}
		}
	}

	def sort(lst: List[Int]): List[Int]= lst match {
		case List() => List()
    	case head :: tail => sortHelper(head, sort(tail))
  	}

  	def sortHelper(data: Int, dataSet: List[Int]): List[Int] = dataSet match {
    	case List() => List(data)
    	case head :: tail =>{
    	 	if (data <= head){
    	 		data :: dataSet	
    	 	}  
    	 	else {
    	 		head :: sortHelper(data, tail)		
    	 	}
    	}
	}
	
}	

