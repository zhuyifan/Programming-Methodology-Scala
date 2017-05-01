import edu.umass.cs.CSV
object Wrangling{
	
	def yearIs(data: List[List[String]], n: Int): List[List[String]] = {
		data.filter(a => a(0).toInt == n)
	}//1


	def yearGT(data: List[List[String]], bound: Int): List[List[String]] = {
		data.filter(a => a(0).toInt > bound)
	}//2

	def yearLT(data: List[List[String]], bound: Int): List[List[String]] = {
		data.filter(a => a(0).toInt < bound)
	}//3

	def onlyName(data: List[List[String]], name: String): List[List[String]] = {
		data.filter(a => a(1) == name)
	}//4

	def mostPopularHelper1(data: List[List[String]]):List[(String, List[List[String]])] = {
		data.groupBy(x=>x(1)).toList
	}

	def mostPopularHelper2(data: List[(String, List[List[String]])], info: List[List[String]]=Nil): List[List[String]] = data match{
		case Nil => info
		case a::b => mostPopularHelper2(b, mergeList(info, (mostPopularHelper3(a._2))))
	}

	def mostPopularHelper3(data: List[List[String]]): List[List[String]] = data match{
		case Nil => Nil
		case a::b => a::mostPopularHelper3(b)	
	}

	def mostPopularHelper4(data: List[List[String]]): List[List[String]] = data match{
		case Nil => Nil
		case a::Nil => Nil
		case a::b::c =>{
			if(a(1)==b(1)){
				a.patch(3, Seq((a(3).toInt+b(3).toInt).toString), 1)::mostPopularHelper4(c)
			}
			else{
				a::mostPopularHelper4(b::c)
			}
		}

	}

	def mostPopularHelper5(data: List[List[String]], S: String = "", I: Int = 0):(String, Int) = data match{
		case Nil => (S, I)
		case a::Nil => (S, I)
		case a::b::c=> {
			if(a(3).toInt<b(3).toInt&&b(3).toInt>I){
			mostPopularHelper5(b::c, b(1),b(3).toInt)
			}
			else{
			mostPopularHelper5(b::c, S, I)
			}
		}
	}

	def mostPopular(data: List[List[String]]): (String, Int) = {
		mostPopularHelper5(mostPopularHelper4(mostPopularHelper2(mostPopularHelper1(data))))
	}//5


	def countHelper(data: List[List[String]], num: Int = 0): Int = data match{
		case Nil => num
		case a :: tail => countHelper(tail, num+a(3).toInt)
	}

	def count(data: List[List[String]]): Int = {
		countHelper(data)
	}//6

	def countGirlsAndBoysHelper(data: List[List[String]], num: Int = 0): Int = data match{
		case Nil => num
		case a::tail => {
			if(a(2) == "F"){
				countGirlsAndBoysHelper(tail, num+a(3).toInt)
			}
			else {
				countGirlsAndBoysHelper(tail, num)
			}
		}
	}

	def countGirlsAndBoys(data: List[List[String]]): (Int, Int) = {
		(countGirlsAndBoysHelper(data), count(data)-countGirlsAndBoysHelper(data))
	}//7


	def genderNeutralNamesHelper1(data: List[List[String]]): List[List[String]] = {
		data.filter(a => a(2) == "F")
	}

	def genderNeutralNamesHelper2(data: List[List[String]]): List[List[String]] = {
		data.filter(a => a(2) == "M")
	}

	def genderNeutralNamesHelper3(data: List[List[String]], data1: List[List[String]], set: Set[String] = Set()): Set[String]= data1 match{
		case Nil => set
		case a::b => genderNeutralNamesHelper3(data, b, genderNeutralNamesHelper4(a, genderNeutralNamesHelper2(data), set))
	}

	def genderNeutralNamesHelper4(a:List[String], data: List[List[String]], set: Set[String]): Set[String] = data match{
		case Nil => set
		case c::d =>{
			if(a(1)==c(1)){
				set+a(1)
			}
			else{
				genderNeutralNamesHelper4(a, d, set)
			}
		}
	}

	def genderNeutralNames(data: List[List[String]]): Set[String] ={
		genderNeutralNamesHelper3(data, genderNeutralNamesHelper1(data))
	}//8

	def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = gender match{
		case "F" => {
			if(((CSV.fromFile("cdc-life-expectancy.csv").filter(a => a(0).toInt == birthYear)(0)(2).toInt+ CSV.fromFile("cdc-life-expectancy.csv").filter(a => a(0).toInt == birthYear)(0)(0).toInt)>currentYear)){
				true
			}
			else{
				false
			}		
		}
		case "M" =>{
			if(((CSV.fromFile("cdc-life-expectancy.csv").filter(a => a(0).toInt == birthYear)(0)(1).toInt+ CSV.fromFile("cdc-life-expectancy.csv").filter(a => a(0).toInt == birthYear)(0)(0).toInt)>currentYear)){
				true
			}
			else{
				false
			}
		}
	}//9

	def estimatePopulationHelper1(data: List[String], year: Int): Int = {
		if(expectedAlive(data(2), data(0).toInt, year) == true){
			data(3).toInt
		}
		else{
			0
		}
	}

	def estimatePopulationHelper2(data: List[List[String]], year: Int, num: Int = 0):Int = data match{
		case Nil => num
		case a::tail => estimatePopulationHelper2(tail, year, num + estimatePopulationHelper1(a, year))
	}

	def estimatePopulation(data: List[List[String]], year: Int): Int = {
		estimatePopulationHelper2(data, year)
	}//10


	def mergeList[A](alist1: List[A], alist2: List[A]): List[A]= alist1 match{
		case Nil => alist2
		case a::Nil => a::mergeList(alist2, Nil)
		case a::b => a::mergeList(b, alist2)
		
	}




}







