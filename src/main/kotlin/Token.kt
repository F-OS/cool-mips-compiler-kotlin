sealed class Token(open val line: Int)

class EndOfFile(override val line: Int) : Token(line)

data class Unimplemented(val c: Char, override val line: Int) : Token(line)

data class Num(val is_int: Boolean, val int: Long, val dbl: Double, override val line: Int) : Token(line)

data class Ident(val ident: String, override val line: Int) : Token(line)

data class StringTok(val str: String, override val line: Int) : Token(line)

data class CharTok(val c: Char, override val line: Int) : Token(line)


class LParen(override val line: Int) : Token(line)

class RParen(override val line: Int) : Token(line)

class LBracket(override val line: Int) : Token(line)

class RBracket(override val line: Int) : Token(line)

class LBrace(override val line: Int) : Token(line)

class RBrace(override val line: Int) : Token(line)

class Semicolon(override val line: Int) : Token(line)

class Comma(override val line: Int) : Token(line)

class Assign(override val line: Int) : Token(line)

class Dot(override val line: Int) : Token(line)

class AddAssign(override val line: Int) : Token(line)

class SubAssign(override val line: Int) : Token(line)

class MulAssign(override val line: Int) : Token(line)

class DivAssign(override val line: Int) : Token(line)

class ModAssign(override val line: Int) : Token(line)

class PowAssign(override val line: Int) : Token(line)

class LShiftAssign(override val line: Int) : Token(line)

class RShiftAssign(override val line: Int) : Token(line)

class AndAssign(override val line: Int) : Token(line)

class OrAssign(override val line: Int) : Token(line)

class XorAssign(override val line: Int) : Token(line)

class Inc(override val line: Int) : Token(line)

class Dec(override val line: Int) : Token(line)

class Add(override val line: Int) : Token(line)

class Sub(override val line: Int) : Token(line)

class Mul(override val line: Int) : Token(line)

class Div(override val line: Int) : Token(line)

class Pow(override val line: Int) : Token(line)

class Mod(override val line: Int) : Token(line)

class GreaterThan(override val line: Int) : Token(line)

class GreaterEqual(override val line: Int) : Token(line)

class EqualTo(override val line: Int) : Token(line)

class NotEqualTo(override val line: Int) : Token(line)

class LessEqual(override val line: Int) : Token(line)

class LessThan(override val line: Int) : Token(line)

class And(override val line: Int) : Token(line)

class Or(override val line: Int) : Token(line)

class Not(override val line: Int) : Token(line)

class QMark(override val line: Int) : Token(line)

class Colon(override val line: Int) : Token(line)
class Bitwise_And(override val line: Int) : Token(line)

class Bitwise_Or(override val line: Int) : Token(line)

class Bitwise_Not(override val line: Int) : Token(line)

class Bitwise_Xor(override val line: Int) : Token(line)

class Bitwise_LShift(override val line: Int) : Token(line)

class Bitwise_RShift(override val line: Int) : Token(line)
