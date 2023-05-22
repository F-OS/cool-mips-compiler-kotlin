package ASTTransforms.TreeTyper

sealed class Type {
    override fun toString(): String {
        return "!!BADTYPE!!"
    }

    protected open val promotionlist: List<Type> = listOf()
    fun canPromoteTo(right: Type): Boolean = promotionlist.any { it == right }
}

object boolType : Type() {
    override val promotionlist = listOf(integerType)
    override fun toString(): String {
        return "Boolean"
    }
}

object integerType : Type() {
    override val promotionlist = listOf(floatingType)
    override fun toString(): String {
        return "Integer"
    }
}

object floatingType : Type() {
    override fun toString(): String {
        return "Floating"
    }
}

object charType : Type() {
    override val promotionlist = listOf(integerType)
    override fun toString(): String {
        return "Character Literal"
    }
}

object stringType : Type() {
    override fun toString(): String {
        return "String"
    }
}

data class lambdaType(val func: FunctionSymbol? = null) : Type() {
    override fun toString(): String {
        return "Anonymous Function ${if (func != null) "with signature " + func.toString() else ""}"
    }
}

data class objectType(val name: String? = null, val associatedScope: Scope? = null) : Type() {
    override fun toString(): String {
        return "Object ${if (name != null) "with name " + name else ""}"
    }
}

data class enumMembType(val name: String, val id: Long) : Type() {
    override fun toString(): String {
        return "Enum member with name " + name + " and ID " + id
    }
}

object nullType : Type() {
    override fun toString(): String {
        return "Null Type"
    }
}