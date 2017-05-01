import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike { 

	def notAlphanumeric: Regex = "[^0-9a-zA-Z]*".r

	def time: Regex = "(([0-1][0-9])|([2][0-3])):[0-5][0-9]".r

	def phone: Regex = "\\(\\d{3}\\) \\d{3}-\\d{4}".r

	def zip: Regex = "(\\d{5})|(\\d{5}-\\d{4})".r

	def comment: Regex = "/\\*.*\\*/".r

	def numberPhrase: Regex = "(twenty|(twenty-(one|two|three|four|five|six|seven|eight|nine)))|(thirty|(thirty-(one|two|three|four|five|six|seven|eight|nine)))|(forty|(forty-(one|two|three|four|five|six|seven|eight|nine)))|(fifty|(fifty-(one|two|three|four|five|six|seven|eight|nine)))|(sixty|(sixty-(one|two|three|four|five|six|seven|eight|nine)))|(seventy|(seventy-(one|two|three|four|five|six|seven|eight|nine)))|(eighty|(eighty-(one|two|three|four|five|six|seven|eight|nine)))|(ninety|(ninety-(one|two|three|four|five|six|seven|eight|nine)))".r 

	def roman: Regex = "(X{0,3}V?I{1,3})|(X{0,3}V)|(X{1,3})|(X{0,3}((IX)|(IV)))".r

	def date: Regex = "(([0-9]{3}[1-9]|[0-9]{2}[1-9][0-9]{1}|[0-9]{1}[1-9][0-9]{2}|[1-9][0-9]{3})-(((0[13578]|1[02])-(0[1-9]|[12][0-9]|3[01]))|((0[469]|11)-(0[1-9]|[12][0-9]|30))|(02-(0[1-9]|[1][0-9]|2[0-8]))))|((([0-9]{2})(0[48]|[2468][048]|[13579][26])|((0[48]|[2468][048]|[3579][26])00))-02-29)".r

	def evenParity: Regex = "(([02468]*[13579]){2})*[02468]*".r

}