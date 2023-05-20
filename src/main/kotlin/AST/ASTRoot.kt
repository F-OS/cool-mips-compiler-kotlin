package AST

import ASTTransforms.Visitable
import ASTTransforms.Visitor

sealed class ASTRoot(val line: Int) : Visitable {

	override fun accept(visitor: Visitor) {
		throw NotImplementedError("Do not try to visit an abstract class. Bad.")
	}
}

fun nodeToString(node: ASTRoot): String {

}