import java.nio.file._
import PathImplicits._

class Tests extends org.scalatest.FunSuite { 
	test("/"){
	val file = "a"/"b"/"c.txt"
	file.write("this is a test\nanother test")
	}

	test("read"){
		val file = "a"/"b"/"c.txt"
		file.write("this is a test\nanother test")
		assert(file.read()=="this is a test\nanother test")
	}

	test("append"){
		val file = "a"/"b"/"c.txt"
		file.write("this is a test\nanother test")
		file.append("\nshould be in line 3")
		assert(file.read()=="this is a test\nanother test\nshould be in line 3")
	}
}	