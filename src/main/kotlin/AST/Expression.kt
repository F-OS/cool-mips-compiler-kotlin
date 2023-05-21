package AST

import ASTTransforms.Visitor

sealed class Expression(line: Int) : ASTRoot(line) {
	override fun accept(visitor: Visitor) {
		throw NotImplementedError("Do not try to visit an abstract class. Bad.")
	}
}

class Lambda(val params: List<Pair<String, String>>, val block: Statement, var return_type: String, line: Int) :
	Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
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
	Bitwise_LS,
	Bitwise_RS,
	Bitwise_Xor,
}

enum class UnaryOps {
	Not,
	BNot,
	Invert,
}

class BinaryOp(val left: Expression, val op: BinaryOps, val right: Expression, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class UnaryOp(val op: UnaryOps, val inner: Expression, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class ListAccess(val ident: String = "GENERIC", val index: Expression? = null, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class VariableAccess(val ident: String = "GENERIC", line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Modify(val ident: Expression, val return_previous: Boolean, val modify_by: Expression, line: Int) :
	Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class ScopeOf(val inscope: String = "GENERIC", val perform: Expression, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Call(val func: String = "GENERIC", val params: List<Expression> = listOf(), line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class IntegerNode(val num: Long, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Floating(val num: Double, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class Bool(val bool: Boolean, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class StringLit(val str: String = "GENERIC", line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class CharNode(val char_: Char, line: Int) : Expression(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}
