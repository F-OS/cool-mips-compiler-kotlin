package ASTTransforms
interface Visitable {
    fun accept(visitor: Visitor): Any?
}

