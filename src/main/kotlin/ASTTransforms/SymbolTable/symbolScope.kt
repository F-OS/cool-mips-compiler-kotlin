package ASTTransforms.SymbolTable


data class symbolScope(var parent: symbolScope?) {
    val symbols: MutableList<symbol> = mutableListOf()
    var children: MutableList<symbolScope> = mutableListOf();
}

data class typeScope(val parent: typeScope?, val symbols: MutableList<String> = mutableListOf()) {
    val children: MutableList<typeScope> = mutableListOf();
}