import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike { 
	def eval(e: Expr): Double = e match{
		case Num(a) => a
		case Add(a, b) => eval(a)+eval(b)
		case Div(a, b) => eval(a)/eval(b)
		case Exponent(a, b) => scala.math.pow(eval(a),eval(b))
		case Mul(a, b) => eval(a)*eval(b)
		case Sub(a, b) => eval(a)-eval(b)
	}
}

object ArithParser extends ArithParserLike {
// number: PackratParser[Double] is defined in ArithParserLike
	lazy val atom: PackratParser[Expr] = 
		"(" ~ expr ~ ")" ^^ {case _ ~ e ~ _ => e} |
		number ^^ {case num => Num(num)} 

	lazy val exponent: PackratParser[Expr] = 
		exponent ~ "^" ~ atom ^^ {case a ~ "^" ~ b => Exponent(a, b)} |
		atom

	lazy val add: PackratParser[Expr] = 
		add ~ "+" ~ mul ^^{case a ~ "+" ~ b => Add(a,b)}|
		add ~ "-" ~ mul ^^{case a ~ "-" ~ b => Sub(a,b)}|
		mul

	lazy val mul: PackratParser[Expr] = 
		mul ~ "*" ~ exponent ^^{case a ~"*" ~ b => Mul(a,b)} |
		mul ~ "/" ~ exponent ^^{case a ~"/" ~ b => Div(a,b)} |
		exponent

	lazy val expr: PackratParser[Expr] = add
}

object ArithPrinter extends ArithPrinterLike { 
	def print(e: Expr): String = e match{
		case Num(a) => a.toString
		case Add(a, b) => "(" + print(a) + "+" + print(b) + ")"
		case Div(a, b) => "(" + print(a) + "/" + print(b) +")"
		case Exponent(a, b) => "(" + print(a) + "^" + print(b) + ")"
		case Mul(a, b) => "(" + print(a) + "*" + print(b) + ")"
		case Sub(a, b) => "(" + print(a) + "-" + print(b) + ")" 
	}
}
