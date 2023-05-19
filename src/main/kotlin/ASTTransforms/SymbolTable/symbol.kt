package ASTTransforms.SymbolTable

data class symbol(val name: String, val type: String, val associatedScope: symbolScope?/* = null*/)