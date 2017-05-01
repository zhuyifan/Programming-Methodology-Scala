import Lists._

class TestSuite extends org.scalatest.FunSuite {
	test("oddNumbers properly defined") { 
		assert(oddNumbers == List(1, 3, 5))
	} 

	test("sumDouble properly defined1") { 
		assert(sumDouble(List(1,2,3,4,5)) == 30)
	} 

	test("sumDouble properly defined2") { 
		assert(sumDouble(List(0)) == 0)
	} 

	test("sumDouble properly defined3") { 
		assert(sumDouble(List(-1, -2)) == -6)
	} 

	test("removeZeroes properly defined1") { 
		assert(removeZeroes(List(0,1,2,0,3,4,5,0)) == List(1,2,3,4,5))
	} 

	test("removeZeroes properly defined2") { 
		assert(removeZeroes(List(0)) == List())
	} 

	test("removeZeroes properly defined3") { 
		assert(removeZeroes(List(1,2,3)) == List(1,2,3))
	}

	test("removeZeroes properly defined4") { 
		assert(removeZeroes(List(0,0,0,0,0)) == List())
	}

	test("countEvens properly defined 1"){
		assert(countEvens(List(1,2,3,4,5,6,7,8,9,30,56,34)) == 7)
	}

	test("countEvens properly defined 2"){
		assert(countEvens(List(11,3,5,7,9)) == 0)
	}

	test("countEvens properly defined 3"){
		assert(countEvens(List(0)) == 1)
	}

	test("countEvens properly defined 4"){
		assert(countEvens(List(2,2,2)) == 3)
	}

	test("removeAlternating properly defined1"){
		assert(removeAlternating(List("A", "B")) == List("A"))
	}

	test("removeAlternating properly defined2"){
		assert(removeAlternating(List("A", "B", "C")) == List("A", "C"))
	}

	test("removeAlternating properly defined3"){
		assert(removeAlternating(List("A", "B", "C", "D")) == List("A", "C"))
	}

	test("removeAlternating properly defined4"){
		assert(removeAlternating(List("A")) == List("A"))
	}

	test("removeAlternating properly defined5"){
		assert(removeAlternating(List()) == List())
	}

	test("isAscending properly defined1"){
		assert(isAscending(List(1,2,3,6,8,12,45)) == true)
	}

	test("isAscending properly defined2"){
		assert(isAscending(List(2,1)) == false)
	}

	test("isAscending properly defined3"){
		assert(isAscending(List(1)) == true)
	}

	test("isAscending properly defined4"){
		assert(isAscending(List()) == false)
	}

	test("isAscending properly defined5"){
		assert(isAscending(List(1,2,2,3,4)) == true)
	}

	test("addSub properly defined1"){
		assert(addSub(List(1,2,3,4,5,6,7))== 4)
	}

	test("addSub properly defined2"){
		assert(addSub(List(1))== 1)
	}

	test("addSub properly defined3"){
		assert(addSub(List(1,2))== -1)
	}

	test("alternate properly define1"){
		assert(alternate(List(1, 3, 5, 12), List(2, 4, 6, 8)) == List(1, 2, 3, 4, 5, 6, 12, 8))
	}

	test("alternate properly define2"){
		assert(alternate(List(1, 2), List(3, 4)) == List(1, 3, 2, 4))
	}

	test("alternate properly define3"){
		assert(alternate(List(), List()) == List())
	}

	test("fromTo properly define1"){
		assert(fromTo(9, 13) == List(9, 10, 11, 12))
	}

	test("fromTo properly define2"){
		assert(fromTo(9, 10) == List(9))
	}

	test("fromTo properly define3"){
		assert(fromTo(-1, 2) == List(-1, 0, 1))
	}

	test("inserOrdered properly define1"){
		assert(insertOrdered(0, List(1, 3, 7, 9)) == List(0, 1, 3, 7, 9))
	}

	test("inserOrdered properly define2"){
		assert(insertOrdered(5, List(1, 3, 7, 9)) == List(1, 3, 5, 7, 9))
	}

	test("inserOrdered properly define3"){
		assert(insertOrdered(10, List(1, 3, 7, 9)) == List(1, 3, 7, 9, 10))
	}

	test("inserOrdered properly define4"){
		assert(insertOrdered(0, List(-11, 3, 7, 9)) == List(-11, 0, 3, 7, 9))
	}

	test("insertOrdered properly define5"){
		assert(insertOrdered(0, List(0)) == List(0, 0))
	}

	test("insertOrdered properly difine6"){
		assert(insertOrdered(0, List(0, 0)) == List(0, 0, 0))
	}

	test("insertOrdered properly difine7"){
		assert(insertOrdered(1, List(0, 1, 3)) == List(0, 1, 1, 3))
	}

	test("sort properly define1"){
		assert(sort(List(1, 4, 6, 2, 3, 8, 1, 2, 5, 9, 1)) == List(1, 1, 1, 2, 2, 3, 4, 5, 6, 8, 9))
	}

	test("sort properly define2"){
		assert(sort(List(-9, 5, 8, 1, 4, -6, -2, -10, 7)) == List(-10, -9, -6, -2, 1, 4, 5, 7, 8))
	}

	test("sort properly define3"){
		assert(sort(List()) == List())
	}

	test("sort properly difine4"){
		assert(sort(List(0)) == List(0))
	}

	test("sort properly difine5"){
		assert(sort(List(0, 1)) == List(0, 1))
	}

	test("sort properly difine6"){
		assert(sort(List(1, 0)) == List(0, 1))
	}



}