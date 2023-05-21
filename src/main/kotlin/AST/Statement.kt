package AST

import ASTTransforms.Visitor

sealed class Statement(line: Int) : Declaration(line) {
	override fun accept(visitor: Visitor) {
		throw NotImplementedError("Do not try to visit an abstract class. Bad.")
	}
}

class If(val conditional: Expression, val consequent: Statement, val alternate: Statement?, line: Int) :
	Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class For(
	val initalizer: Declaration?,
	val conditional: Declaration?,
	val iteration: Declaration?,
	val body: Statement, line: Int
) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class ForEach(val iterval: String, val collectionvar: String, val body: Statement, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class While(val conditional: Expression, val body: Statement, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class DoWhile(val conditional: Expression, val body: Statement, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Continue(line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Break(line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		visitor.visit(this)
	}
}

class Label(val ident: String, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Switch(val switchon: Expression, val cases: List<Pair<Expression, Statement>>, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Assignment(val ident: Expression, val expr: Expression, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Return(val expr: Expression?, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Goto(val gotoident: String, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		visitor.visit(this)
	}
}

class Try(val block: Statement, val catches: String, val catchesAs: String, val catch: Statement, line: Int) :
	Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Throw(val ident: String, val params: List<Expression>, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

// Handles void function calls or expressions without side effects.
class ExprStatement(val expr: Expression? = null, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Block(val statements: List<Declaration>, line: Int) : Statement(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

