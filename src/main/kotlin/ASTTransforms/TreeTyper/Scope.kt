package ASTTransforms.TreeTyper

class Scope(var parent: Scope? = null) {
    private val symbolTable: MutableMap<String, Symbol> = mutableMapOf()
    private val typeTable: MutableMap<String, Type> = mutableMapOf()

    override fun toString(): String {
        var symbStr: String = ""
        for (symb in symbolTable) {
            symbStr += "\t" + symb + "\n"
        }
        return symbStr
    }

    fun defineSymbol(name: String, symbol: Symbol) {
        symbolTable[name] = symbol
    }

    fun resolveSymbol(name: String): Symbol? {
        return symbolTable[name] ?: parent?.resolveSymbol(name)
    }

    fun resolveSymbolCurScope(name: String): Symbol? {
        return symbolTable[name]
    }

    fun setIsDefined(name: String) {
        val resolv = resolveSymbol(name)
        if (resolv is VariableSymbol) {
            resolv.defined = true
        }
    }

    fun defineType(name: String, type: Type) {
        typeTable[name] = type
    }

    fun resolveType(name: String): Type? {
        return typeTable[name] ?: parent?.resolveType(name)
    }

    fun verifyIsDefined(symbol: String): Boolean {
        val resolv = resolveSymbol(symbol)
        if (resolv is VariableSymbol) {
            return resolv.defined
        }
        return false
    }
}