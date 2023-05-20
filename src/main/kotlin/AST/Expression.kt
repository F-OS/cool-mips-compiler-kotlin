package AST

import ASTTransforms.Visitor
import ASTTransforms.Visitable

sealed class ASTRoot : Visitable
{
    override fun accept(visitor: Visitor) {
        throw NotImplementedError("Do not try to visit an abstract class. Bad.")
    }
}
sealed class Expression : ASTRoot() {
    override fun accept(visitor: Visitor) {
        throw NotImplementedError("Do not try to visit an abstract class. Bad.")
    }
}

class Lambda(val params: List<String>, val block: Statement) : Expression() {
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
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
}

enum class UnaryOps {
    Not,
    BNot,
    Invert,
}

class BinaryOp(val left: Expression, val op: BinaryOps, val right: Expression) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class UnaryOp(val op: UnaryOps, val inner: Expression) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class ListAccess(val ident: String, val index: Expression) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class VariableAccess(val ident: String) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Modify(val ident: String, val return_previous: Boolean, val modify_by: Expression) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}
class ScopeOf(val inscope: String, val perform: Expression) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Call(val func: String, val params: List<Expression>) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Integer(val num: Long) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Floating(val num: Double) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Bool(val bool: Boolean) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class StringLit(val str: String) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Char_(val char_: Char) : Expression()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}
