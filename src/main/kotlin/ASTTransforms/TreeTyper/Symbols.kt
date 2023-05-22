package ASTTransforms.TreeTyper

import AST.Declaration

open class Symbol(open val name: String)

data class ClassSymbol(
    override val name: String,
    val inherit: List<ClassSymbol>,
    val associatedScope: Scope
) : Symbol(name) {
    override fun toString(): String {
        val inheritString = inherit.joinToString(", ") { x -> x.toString() }
        return "class $name : $inheritString {\n    $associatedScope \n}"
    }
}

data class StructSymbol(
    override val name: String,
    val associatedScope: Scope
) : Symbol(name) {
    override fun toString(): String {
        return "struct $name {\n    $associatedScope \n}"
    }
}

data class EnumSymbol(override val name: String, val members: List<String>) : Symbol(name) {
    override fun toString(): String {
        val membersString = members.joinToString(", ")
        return "enum $name {\n    members: $membersString\n}"
    }
}

data class enumMembSymb(override val name: String, val id: Long) : Symbol(name) {
    override fun toString(): String {
        return "enum member $name - $id"
    }
}

data class FunctionSymbol(
    override val name: String,
    val params: List<Pair<String, String>>,
    val returnType: String,
    var decls: List<Declaration> = listOf()
) :
    Symbol(name) {
    override fun toString(): String {
        val paramsString = params.joinToString(", ")
        return "$name: ($paramsString) -> $returnType"
    }
}

data class VariableSymbol(override val name: String, val type: Type, var defined: Boolean = false) : Symbol(name) {
    override fun toString(): String {
        return "var $name: $type"
    }
}

data class ArraySymbol(override val name: String, val size: Int, val elementType: Type, var defined: Boolean = false) :
    Symbol(name) {
    override fun toString(): String {
        return "array $name[$size] of $elementType"
    }
}
