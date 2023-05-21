package AST

import ASTTransforms.Visitable
import ASTTransforms.Visitor

sealed class ASTRoot(val line: Int) : Visitable {

	override fun accept(visitor: Visitor) {
		throw NotImplementedError("Do not try to visit an abstract class. Bad.")
	}
}

fun nodeToString(node: ASTRoot): String {
	when (node) {
		is ArrayDeclaration -> return "Array Declaration"
		is ClassDeclaration -> return "Class Declaration"
		is EnumDeclaration -> return "Enum Declaration"
		is FunctionDeclaration -> return "Function Declaration"
		is SimpleVarDeclaration -> return "Var Declaration"
		is Assignment -> return "Assignment"
		is Block -> return "Block"
		is Break -> return "Break"
		is Continue -> return "Continue"
		is DoWhile -> return "Do While"
		is ExprStatement -> return "Expression Statement"
		is For -> return "For"
		is ForEach -> return "For Each"
		is Goto -> return "Goto"
		is If -> return "If"
		is Label -> return "Label"
		is Return -> return "Return"
		is Switch -> return "Switch"
		is Throw -> return "Throw"
		is Try -> return "Try"
		is While -> return "While"
		is BinaryOp -> return "Binary Op"
		is Bool -> return "Boolean"
		is Call -> return "Function Call"
		is CharNode -> return "Char"
		is Floating -> return "Floating number"
		is IntegerNode -> return "Integer number"
		is Lambda -> return "Lambda"
		is ListAccess -> return "List Access"
		is Modify -> return "Modify"
		is ScopeOf -> return "Scope Of"
		is StringLit -> return "String"
		is UnaryOp -> return "Unary Op"
		is VariableAccess -> return "Variable Access"
		is StructDeclaration -> return "Struct Declaration"
	}
}