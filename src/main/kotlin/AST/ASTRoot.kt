package AST

import ASTTransforms.Visitable
import ASTTransforms.Visitor

sealed class ASTRoot(val line: Int) : Visitable {
	override fun accept(visitor: Visitor) {
		throw NotImplementedError("Do not try to visit an abstract class. Bad.")
	}
}

fun nodeToString(node: ASTRoot): String {
	return when (node) {
		is ArrayDeclaration -> "Array Declaration"
		is ClassDeclaration -> "Class Declaration"
		is EnumDeclaration -> "Enum Declaration"
		is FunctionDeclaration -> "Function Declaration"
		is SimpleVarDeclaration -> "Var Declaration"
		is Assignment -> "Assignment"
		is Block -> "Block"
		is Break -> "Break"
		is Continue -> "Continue"
		is DoWhile -> "Do While"
		is ExprStatement -> "Expression Statement"
		is For -> "For"
		is ForEach -> "For Each"
		is Goto -> "Goto"
		is If -> "If"
		is Label -> "Label"
		is Return -> "Return"
		is Switch -> "Switch"
		is Throw -> "Throw"
		is Try -> "Try"
		is While -> "While"
		is BinaryOp -> "Binary Op"
		is Bool -> "Boolean"
		is Call -> "Function Call"
		is CharNode -> "Char"
		is Floating -> "Floating number"
		is IntegerNode -> "Integer number"
		is Lambda -> "Lambda"
		is ListAccess -> "List Access"
		is Modify -> "Modify"
		is ScopeOf -> "Scope Of"
		is StringLit -> "String"
		is UnaryOp -> "Unary Op"
		is VariableAccess -> "Variable Access"
		is StructDeclaration -> "Struct Declaration"
		is Ternary -> "Ternary"
	}
}