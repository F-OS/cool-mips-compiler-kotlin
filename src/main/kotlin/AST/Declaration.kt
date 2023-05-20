package AST

import ASTTransforms.Visitor

sealed class Declaration: ASTRoot() {
    override fun accept(visitor: Visitor) {
        throw NotImplementedError("Do not try to visit an abstract class. Bad.")
    }
}

class SimpleVarDeclaration(val name: String, val definition: Expression?) : Declaration() {
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class ArrayDeclaration(val name: String, val size: Integer?, var definition: List<Expression>) : Declaration() {
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class EnumDeclaration(val name: String, var members: List<Pair<String, Int>>) : Declaration() {
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class ClassDeclaration(val name: String, var members: List<Declaration>, var inheritsfrom: List<String>) : Declaration() {
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}

class FunctionDeclaration(val name: String, var parameters: List<String>, var body: Block) : Declaration() {
    override fun accept(visitor: Visitor) {
        visitor.visit(this)
    }
}