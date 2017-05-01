import Regexes._

class TrivialTestSuite extends org.scalatest.FunSuite {

	test("The Regexes object must be defined") { 
		val regexes: hw.regex.RegexLike = Regexes
	} 

	test("notAlphanumeric"){
		assert(notAlphanumeric.pattern.matcher("*").matches())
	}

	test("time"){
		assert(time.pattern.matcher("23:00").matches())
	}

	test("phone"){
		assert(phone.pattern.matcher("(413) 992-6557").matches())
	}

	test("zip"){
		assert(zip.pattern.matcher("12334").matches())
	}

	test("zip2"){
		assert(zip.pattern.matcher("98023-3492").matches())
	}

	test("comment"){
		assert(comment.pattern.matcher("/*z*/").matches())
	}

	test("numberPhrase"){
		assert(numberPhrase.pattern.matcher("twenty").matches())
	}

	test("roman"){
		assert(roman.pattern.matcher("XXIV").matches())
	}

	test("date"){
		assert(date.pattern.matcher("2010-05-01").matches())
	}

	test("date1"){
		assert(date.pattern.matcher("2012-02-29").matches())
	}

	test("evenParity"){
		assert(evenParity.pattern.matcher("02345").matches())
	}

}