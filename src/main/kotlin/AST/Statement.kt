package AST

import ASTTransforms.Visitor

sealed class Statement : Declaration() {
    override fun accept(visitor: Visitor) {
        throw NotImplementedError("Do not try to visit an abstract class. Bad.")
    }
}

class If(val conditional: Expression, val consequent: Statement, val alternate: Statement?) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class For(val initalizer: Declaration?,
          val conditional: Expression?,
          val iteration: Statement?,
          val body: Statement) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class ForEach(val iterval: String, val collectionvar: String, val body: Statement) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class While(val conditional: Expression, val body: Statement) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class DoWhile(val conditional: Expression, val body: Statement) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Continue() : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Break() : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Label(val ident: String) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Switch(val switchon: Expression, val cases: List<Pair<Expression, Statement>>) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Assignment(val ident: String, val expr: Expression) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Return(val expr: Expression?) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Goto(val gotoident: String) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Try(val block: Statement, val catches: String, val catchesAs: String, val catch: Statement) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class Throw(val ident: String, val params: List<Expression>) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}
// Handles void function calls or expressions without side effects.
class ExprStatement(val expr: Expression) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}
class Block(val statements: List<Declaration>) : Statement()
{
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

