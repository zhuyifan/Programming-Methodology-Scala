import java.nio.file._

object PathImplicits {
	
	
	
	implicit class str(a: String){
		
		def /(s:String): Path = Paths.get(a).resolve(s)
		
		def /(p: Path): Path = Paths.get(a).resolve(p)
	}

	implicit class pa(a: Path){
		
		def /(p: Path): Path = a.resolve(p)
		
		def /(s: String): Path = a.resolve(s)

		def write(s: String): Path = Files.write(a, s.getBytes)

		def read(): String = new String(Files.readAllBytes(a))

		def append(s: String): Path = {
			if(Files.exists(a)){
				Files.write(a, s.getBytes, StandardOpenOption.APPEND)
			}
			else{
				Files.write(a, s.getBytes)
			}
		}
	}
}

import java.time._

object DateImplicits{
	
	implicit class Ints(a: Int){
		
		def jan(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def feb(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a) 
		def mar(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a) 
		def apr(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def may(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def jun(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def jul(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def aug(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def sept(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def oct(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def nov(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)
		def dec(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, a)

		def jan(b: Int): LocalDate = LocalDate.of(b, 1, a)
		def feb(b: Int): LocalDate = LocalDate.of(b, 2, a)
		def mar(b: Int): LocalDate = LocalDate.of(b, 3, a)
		def apr(b: Int): LocalDate = LocalDate.of(b, 4, a)
		def may(b: Int): LocalDate = LocalDate.of(b, 5, a)
		def jun(b: Int): LocalDate = LocalDate.of(b, 6, a)
		def jul(b: Int): LocalDate = LocalDate.of(b, 7, a)
		def aug(b: Int): LocalDate = LocalDate.of(b, 8, a)
		def sept(b: Int): LocalDate = LocalDate.of(b, 9, a)
		def oct(b: Int): LocalDate = LocalDate.of(b, 10, a)
		def nov(b: Int): LocalDate = LocalDate.of(b, 11, a)
		def dec(b: Int): LocalDate = LocalDate.of(b, 12, a)

		def days():(Int, String) = {
			(a, "days")
		}

		def months():(Int, String)= {
			(a, "months")
		}

		def years():(Int, String)= {
			(a, "years")
		}
	}

	implicit class LD(a: LocalDate){

		def +(s:(Int, String)): LocalDate = s match{
			case (int, "days") => a.plusDays(int.toLong)
			case (int, "months") => a.plusMonths(int.toLong)
			case (int, "years") => a.plusYears(int.toLong)
		}
	}
}