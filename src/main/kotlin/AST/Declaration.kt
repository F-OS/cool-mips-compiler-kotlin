package AST

import ASTTransforms.Visitor

sealed class Declaration(line: Int) : ASTRoot(line) {
	override fun accept(visitor: Visitor) {
		throw NotImplementedError("Do not try to visit an abstract class. Bad.")
	}
}

class SimpleVarDeclaration(val name: Pair<String, String>, val definition: Expression?, line: Int) : Declaration(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class ArrayDeclaration(
	val name: Pair<String, String>,
	val size: Long,
	var definition: List<Expression>,
	line: Int
) : Declaration(
	line
) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class EnumDeclaration(val name: String, var members: List<Pair<String, Long>>, line: Int) : Declaration(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class ClassDeclaration(val name: String, var members: List<Declaration>, var inheritsfrom: List<String>, line: Int) :
	Declaration(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class StructDeclaration(val name: String, var members: List<Declaration>, line: Int) :
	Declaration(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}

class FunctionDeclaration(
	val name: String, var parameters: List<Pair<String, String>>, var returnType: String, var body: Block,
	line: Int
) : Declaration(line) {
	override fun accept(visitor: Visitor) {
		return visitor.visit(this)
	}
}