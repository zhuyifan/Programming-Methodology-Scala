import Wrangling._
import edu.umass.cs.CSV



class TestSuite extends org.scalatest.FunSuite {
	val data = CSV.fromFile("shortened-births.csv")

	test("only"){
		assert(onlyName(data, "Sheneice")==List(List("df")))
	}

	test("count2"){
		assert(count(data) == 1826)
	}

	test("count"){
		assert(countGirlsAndBoys(data) == (12,12))
	}

	test("f and m"){
		assert(countGirlsAndBoys(data) == (1084,742))
	}

	test("expected alive"){
		assert(expectedAlive("M", 1940, 2010) == false)
	}

	test("expected alive1"){
		assert(expectedAlive("F", 1940, 2000) == true)
	}

	test("gender name"){
		assert(genderNeutralNames(data) == Set("Sunny", "Montana", "Kelley", "Maxie", "Vernon", "Charley", "Lucky", "Bernice", "Patrick", "Sydney", "Luis", "Lincoln", "Araceli", "Scott", "Octavius", "Schuyler"))
	}

	test("hhhhhhhh"){
		assert(mostPopular(data) == ("ashe", 23))
	}

	test("1"){
		assert(mostPopularHelper1(data)==List(List("")))
	}

	test("2"){
		assert(mostPopularHelper2(mostPopularHelper1(data))==List(List("")))
	}

	test("3"){
		assert(mostPopularHelper4(mostPopularHelper2(mostPopularHelper1(data)))==List(List("")))
	}

	test("4"){
		assert(mostPopularHelper5(mostPopularHelper4(mostPopularHelper2(mostPopularHelper1(data))))==("", 0))
	}

	test("population"){
		assert(estimatePopulation(data, 2017) == 45)
	}

	test("df"){
		assert(estimatePopulation(yearGT(data , 1980), 2015)==34)
	}

	test("dfs"){
		assert(yearIs(data, 1995)==List(List("")))
	}
}
