package Tokenizer

sealed class Token(open val line: Int?)

class EndOfFile(override val line: Int = -1) : Token(line)

data class Unimplemented(val c: Char, override val line: Int = -1) : Token(line)

// False set here to force NaN so exceptions occur if this token is used without proper initalization.
data class Num(
	val is_int: Boolean = false,
	val int: Long = Long.MIN_VALUE,
	val dbl: Double = Double.NaN,
	override val line: Int = -1
) : Token(line)

data class Ident(val ident: String = "GENERIC", override val line: Int = -1) : Token(line)

data class StringTok(val str: String = "ISHOULDNEVEREXIST", override val line: Int = -1) : Token(line)

data class CharTok(val c: Char = 0.toChar(), override val line: Int = -1) : Token(line)

class LParen(override val line: Int = -1) : Token(line)

class RParen(override val line: Int = -1) : Token(line)

class LBracket(override val line: Int = -1) : Token(line)

class RBracket(override val line: Int = -1) : Token(line)

class LBrace(override val line: Int = -1) : Token(line)

class RBrace(override val line: Int = -1) : Token(line)

class Semicolon(override val line: Int = -1) : Token(line)

class Comma(override val line: Int = -1) : Token(line)

class Assign(override val line: Int = -1) : Token(line)

class Dot(override val line: Int = -1) : Token(line)

class AddAssign(override val line: Int = -1) : Token(line)

class SubAssign(override val line: Int = -1) : Token(line)

class MulAssign(override val line: Int = -1) : Token(line)

class DivAssign(override val line: Int = -1) : Token(line)

class ModAssign(override val line: Int = -1) : Token(line)

class PowAssign(override val line: Int = -1) : Token(line)

class LShiftAssign(override val line: Int = -1) : Token(line)

class RShiftAssign(override val line: Int = -1) : Token(line)

class AndAssign(override val line: Int = -1) : Token(line)

class OrAssign(override val line: Int = -1) : Token(line)

class XorAssign(override val line: Int = -1) : Token(line)

class Inc(override val line: Int = -1) : Token(line)

class Dec(override val line: Int = -1) : Token(line)

class Add(override val line: Int = -1) : Token(line)

class Sub(override val line: Int = -1) : Token(line)

class Mul(override val line: Int = -1) : Token(line)

class Div(override val line: Int = -1) : Token(line)

class Pow(override val line: Int = -1) : Token(line)

class Mod(override val line: Int = -1) : Token(line)

class GreaterThan(override val line: Int = -1) : Token(line)

class GreaterEqual(override val line: Int = -1) : Token(line)

class EqualTo(override val line: Int = -1) : Token(line)

class NotEqualTo(override val line: Int = -1) : Token(line)

class LessEqual(override val line: Int = -1) : Token(line)

class LessThan(override val line: Int = -1) : Token(line)

class And(override val line: Int = -1) : Token(line)

class Or(override val line: Int = -1) : Token(line)

class Not(override val line: Int = -1) : Token(line)

class QMark(override val line: Int = -1) : Token(line)

class Colon(override val line: Int = -1) : Token(line)
class Bitwise_And(override val line: Int = -1) : Token(line)

class Bitwise_Or(override val line: Int = -1) : Token(line)

class Bitwise_Not(override val line: Int = -1) : Token(line)

class Bitwise_Xor(override val line: Int = -1) : Token(line)

class Bitwise_LShift(override val line: Int = -1) : Token(line)

class Bitwise_RShift(override val line: Int = -1) : Token(line)

fun tokToString(tok: Token): String {
	return when (tok) {
		is Add -> "plus '+'"
		is AddAssign -> "plusAssign '+='"
		is And -> "and '&&'"
		is AndAssign -> "andAssign '&='"
		is Assign -> "assign '='"
		is Bitwise_And -> "bitwiseAnd '&'"
		is Bitwise_LShift -> "bitwiseLShift '<<'"
		is Bitwise_Not -> "bitwiseNot '~'"
		is Bitwise_Or -> "bitwiseOr '|'"
		is Bitwise_RShift -> "bitwiseRShift '>>'"
		is Bitwise_Xor -> "bitwiseXor '^'"
		is CharTok -> "char"
		is Colon -> "colon ':'"
		is Comma -> "comma ','"
		is Dec -> "decrement '--'"
		is Div -> "division '/'"
		is DivAssign -> "divAssign '/='"
		is Dot -> "dot '.'"
		is EndOfFile -> "endOfFile"
		is EqualTo -> "equalTo '=='"
		is GreaterEqual -> "greaterEqual '>='"
		is GreaterThan -> "greaterThan '>'"
		is Ident -> "identifier"
		is Inc -> "increment '++'"
		is LBrace -> "leftBrace '{'"
		is LBracket -> "leftBracket '['"
		is LParen -> "leftParen '('"
		is LShiftAssign -> "lShiftAssign '<<='"
		is LessEqual -> "lessEqual '<='"
		is LessThan -> "lessThan '<'"
		is Mod -> "modulus '%'"
		is ModAssign -> "modAssign '%='"
		is Mul -> "multiplication '*'"
		is MulAssign -> "mulAssign '*='"
		is Not -> "not '!'"
		is NotEqualTo -> "notEqualTo '!='"
		is Num -> "number"
		is Or -> "or '||'"
		is OrAssign -> "orAssign '|='"
		is Pow -> "power '**'"
		is PowAssign -> "powAssign '**='"
		is QMark -> "questionMark '?'"
		is RBrace -> "rightBrace '}'"
		is RBracket -> "rightBracket ']'"
		is RParen -> "rightParen ')'"
		is RShiftAssign -> "rShiftAssign '>>='"
		is Semicolon -> "semicolon ';'"
		is StringTok -> "string"
		is Sub -> "subtraction '-'"
		is SubAssign -> "subAssign '-='"
		is Unimplemented -> "unimplemented"
		is XorAssign -> "xorAssign '^='"
	}
}