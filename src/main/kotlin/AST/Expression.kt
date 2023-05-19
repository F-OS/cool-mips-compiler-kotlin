package AST

sealed class Expression {
}

class Lambda(val params: List<String>, val block: Block) : Expression() {

}

enum class BinaryOps {
    And,
    Or,
    GreaterThan,
    GreaterEqual,
    EqualTo,
    NotEqualTo,
    LessEqual,
    LessThan,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Bitwise_And,
    Bitwise_Or,
    Bitwise_LShift,
    Bitwise_RShift,
    Bitwise_Xor,
    Bitwise_Not,
}

enum class UnaryOps {
    Not,
    BNot,
    Invert,
}

class BinaryOp(val left: Expression, val op: BinaryOps, val right: Expression) : Expression()
{

}

class UnaryOp(val op: UnaryOps, val inner: Expression) : Expression()
{

}

class ListAccess(val ident: String, val index: Expression) : Expression()
{

}

class VariableAccess(val ident: String) : Expression()
{

}

class ScopeOf(val inscope: String, val perform: Expression) : Expression()
{

}

class Call(val func: String, val params: List<Expression>) : Expression()
{

}

class Integer(val num: Long) : Expression()
{

}

class Floating(val num: Double) : Expression()
{

}

class Bool(val bool: Boolean) : Expression()
{

}

class StringLit(val str: String) : Expression()
{

}

class Char_(val char_: Char) : Expression()
{

}
