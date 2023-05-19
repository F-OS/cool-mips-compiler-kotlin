package ASTTransforms.SymbolTable

import AST.*
import ASTTransforms.Visitor

class SymbolTableGenerator : Visitor {
	private var builtin = listOf(
		"print",
		"strlen",
		"read"
	)
	private var delaySymbol: MutableList<Pair<String, ASTRoot>> = mutableListOf()
	private var currentScope = symbolScope(null)
	private var unresolvedSymbols: MutableList<String> = mutableListOf()
	private var unresolvedTypes: MutableList<String> = mutableListOf()
	private var currentTypeScope = typeScope(null, mutableListOf<String>(
		"int",
		"long",
		"short",
		"char",
		"string",
		"bool",
		"Struct",
		"Class",
		"Function",
		"Enum",
		"Enum member",
	))
	var errors = 0;

	fun isVariableInSymbolTable(variableName: String, symbolTable: symbolScope): symbolScope? {
		var checkScope: symbolScope? = symbolTable

		while (checkScope != null) {
			for (symbol in checkScope.symbols) {
				if (symbol.name == variableName) {
					return checkScope
				}
			}
			checkScope = checkScope.parent
		}

		return null
	}

	fun verifySymbol(variableName: String, line: Int): symbolScope {
		val symbol = isVariableInSymbolTable(variableName, currentScope)
		if(symbol != null)
		{
			return currentScope
		}
		if(unresolvedSymbols.all {it != variableName}) {
			throw SymbolException("Line ${line} - Error: Symbol ${variableName} used before declaration.", variableName)
		}
		return currentScope
	}

	fun insertSymbol(variableName: String, variableType: String, associatedScope: symbolScope?/* = null*/)
	{
		var typeExists = false;
		var checkScope: typeScope? = currentTypeScope
		while (checkScope != null) {
			for (symbol in checkScope.symbols) {
				val type = variableType.lowercase()
				val cursym = symbol.lowercase()
				if (cursym == type) {
					typeExists = true
					break;
				}
			}
			checkScope = checkScope.parent
		}
		if(!typeExists && unresolvedTypes.all {it != variableType})
		{
			unresolvedTypes += variableType
			errors++
			println("Error: Type ${variableType} used before declaration.")
		}
		if(isVariableInSymbolTable(variableName, currentScope) == null)
		{
			currentScope.symbols += symbol(variableName, variableType, associatedScope)
		}
	}

	fun visitTree(program: List<Declaration>) {
		while (true)
		{
			try {
				for (node in program) {
					node.accept(this)
				}
				break;
			} catch (symerror: SymbolException) {
				println(symerror.s)
				errors++
				unresolvedSymbols += symerror.variableName
				currentScope = symbolScope(null)
				visitTree(program)
			}
		}
		for((item, node) in delaySymbol)
		{
			verifySymbol(item, node.line)
		}
	}
	override fun visit(node: Lambda) {
		val lambdaScope: symbolScope = symbolScope(currentScope)
		val oldScope = currentScope
		currentScope.children += lambdaScope
		currentScope = lambdaScope
		for((name, type) in node.params)
		{
			insertSymbol(name, type, lambdaScope)
		}
		node.block.accept(this)
		currentScope = oldScope
	}

	override fun visit(node: BinaryOp) {
		node.left.accept(this)
		node.right.accept(this)
	}

	override fun visit(node: UnaryOp) {
		node.inner.accept(this)
	}

	override fun visit(node: ListAccess) {
		verifySymbol(node.ident, node.line)
	}

	override fun visit(node: VariableAccess) {
		verifySymbol(node.ident, node.line)
	}

	override fun visit(node: Modify) {
		node.ident.accept(this)
		node.modify_by.accept(this)
	}

	override fun visit(node: ScopeOf) {
		verifySymbol(node.inscope, node.line)
	}

	override fun visit(node: Call) {
		if(builtin.all {it != node.func.lowercase()}) {
			verifySymbol(node.func, node.line)
		}
	}

	override fun visit(node: IntegerNode) {
		// No symbols.
	}

	override fun visit(node: Floating) {
		// No symbols.
	}

	override fun visit(node: Ternary) {
		node.condition.accept(this)
		node.consequent.accept(this)
		node.alternate.accept(this)
	}
	override fun visit(node: Bool) {
		// No symbols.
	}

	override fun visit(node: StringLit) {
		// No symbols.
	}

	override fun visit(node: CharNode) {
		// No symbols.
	}

	override fun visit(node: If) {
		node.conditional.accept(this)
		node.consequent.accept(this)
		if(node.alternate != null)
		{
			node.alternate.accept(this)
		}
	}

	override fun visit(node: For) {
		val oldScope = currentScope
		val forScope = symbolScope(oldScope)
		oldScope.children += forScope
		currentScope = forScope

		if(node.initalizer != null)
		{
			node.initalizer.accept(this)
		}
		if(node.conditional != null)
		{
			node.conditional.accept(this)
		}
		if(node.iteration != null)
		{
			node.iteration.accept(this)
		}
		node.body.accept(this)
		currentScope = oldScope
	}

	override fun visit(node: ForEach) {
		val forEachScope = symbolScope(currentScope)
		currentScope.children += forEachScope
		val oldScope = currentScope
		currentScope = forEachScope

		forEachScope.symbols += symbol(node.iterval, "!!AWAITTYPE!!", forEachScope)
		verifySymbol(node.collectionvar, node.line)
		node.body.accept(this)

		currentScope = oldScope
	}

	override fun visit(node: While) {
		node.conditional.accept(this)
		node.body.accept(this)
	}

	override fun visit(node: DoWhile) {
		node.conditional.accept(this)
		node.body.accept(this)
	}

	override fun visit(node: Continue) {
		// No symbols
	}

	override fun visit(node: Break) {
		// No symbols
	}

	override fun visit(node: Label) {
		if(isVariableInSymbolTable(node.ident, currentScope) != null)
		{
			return;
		}
		var localScope = currentScope
		while(localScope.parent != null)
		{
			localScope = localScope.parent!!
		}
		localScope.symbols += symbol(node.ident, "!!LABEL!!", currentScope)
	}

	override fun visit(node: Switch) {
		node.switchon.accept(this)
		for((cond, body) in node.cases)
		{
			cond.accept(this)
			body.accept(this)
		}
	}

	override fun visit(node: Assignment) {
		node.ident.accept(this)
		node.expr.accept(this)
	}

	override fun visit(node: Return) {
		if(node.expr != null)
		{
			node.expr.accept(this)
		}
	}

	override fun visit(node: Goto) {
		if(isVariableInSymbolTable(node.gotoident, currentScope) == null && delaySymbol.any {it.first != node.gotoident}) {
			println("WARNING: forward declaration of label in goto at line ${node.line}. This might lead to spagetthi code.")
			delaySymbol += node.gotoident to node
		}
	}

	override fun visit(node: Try) {
		val tryScope = symbolScope(currentScope)
		currentScope.children += tryScope
		val oldScope = currentScope
		currentScope = tryScope

		node.block.accept(this)
		insertSymbol(node.catches, node.catchesAs, tryScope)
		verifySymbol(node.catchesAs, node.line)
		node.catch.accept(this)

		currentScope = oldScope
	}

	override fun visit(node: Throw) {
		verifySymbol(node.ident, node.line)
		for(param in node.params)
		{
			param.accept(this)
		}
	}

	override fun visit(node: ExprStatement) {
		node.expr?.accept(this)
	}

	override fun visit(node: Block) {
		val blockScope = symbolScope(currentScope)
		currentScope.children += blockScope
		val oldScope = currentScope
		currentScope = blockScope

		val blockTScope: typeScope = typeScope(currentTypeScope)
		currentTypeScope.children += blockTScope
		val oldTypeScope: typeScope = currentTypeScope

		for(statement in node.statements)
		{
			statement.accept(this)
		}

		currentScope = oldScope
		currentTypeScope = oldTypeScope
	}
	override fun visit(node: SimpleVarDeclaration) {
		insertSymbol(node.name.first, node.name.second, currentScope)
	}

	override fun visit(node: ArrayDeclaration) {
		insertSymbol(node.name.first, node.name.second, currentScope)
		for(item in node.definition)
		{
			item.accept(this)
		}
	}

	override fun visit(node: EnumDeclaration) {
		insertSymbol(node.name, "Enum", currentScope)
		for(items in node.members)
		{
			insertSymbol(items.first, "Enum member", currentScope)
		}
	}

	override fun visit(node: ClassDeclaration) {
		val classTScope: typeScope = typeScope(currentTypeScope)
		currentTypeScope.children += classTScope
		currentTypeScope.symbols += node.name
		val oldTypeScope: typeScope = currentTypeScope

		val classScope = symbolScope(currentScope)
		insertSymbol(node.name, "Class", classScope)
		for(inherit in node.inheritsfrom)
		{
			verifySymbol(inherit, node.line)
		}

		currentScope.children += classScope
		val oldScope = currentScope
		currentScope = classScope

		for(decls in node.members)
		{
			decls.accept(this)
		}

		currentScope = oldScope
		currentTypeScope = oldTypeScope
	}

	override fun visit(node: StructDeclaration) {
		val structScope = symbolScope(currentScope)

		val structTScope: typeScope = typeScope(currentTypeScope)
		currentTypeScope.children += structTScope
		currentTypeScope.symbols += node.name
		val oldTypeScope: typeScope = currentTypeScope

		insertSymbol(node.name, "Struct", structScope)
		currentScope.children += structScope
		val oldScope = currentScope
		currentScope = structScope

		for(decls in node.members)
		{
			decls.accept(this)
		}

		currentScope = oldScope
		currentTypeScope = oldTypeScope
	}

	override fun visit(node: FunctionDeclaration) {
		if(builtin.any {it == node.name.lowercase()})
		{
			println("WARNING: Redefinition of builtin function ${node.name} ignored.")
			return;
		}
		val funcTScope: typeScope = typeScope(currentTypeScope)
		currentTypeScope.children += funcTScope
		val oldTypeScope: typeScope = currentTypeScope

		val functionScope = symbolScope(currentScope)
		insertSymbol(node.name, "Function", functionScope)
		currentScope.children += functionScope
		val oldScope = currentScope
		currentScope = functionScope

		for((name, type) in node.parameters)
		{
			insertSymbol(name, type, currentScope)
		}
		node.body.accept(this)
		currentScope = oldScope
		currentTypeScope = oldTypeScope
	}

	private fun reduceSymbolTable(table: symbolScope, parent: symbolScope? = null): symbolScope
	{
		var newscope: symbolScope = symbolScope(null)
		val parent = parent
		for(symb in table.symbols)
		{
			newscope.symbols += symb
			if(symb.associatedScope != null && (table != parent))
			{
				var child = reduceSymbolTable(symb.associatedScope, table)
				child.parent = newscope
				newscope.children += child
			}
		}
		return newscope
	}
	val anyMatchingInTree = false
	fun shiftUpSymbolTable(table: symbolScope): symbolScope
	{
		var newtable = table
		var i = 0
		var j = 0
		while(true)
		{
			val child = table.children.getOrNull(j) ?: break
			val verified = shiftUpSymbolTable(child)
			if(verified.symbols.isNotEmpty())
			{
				newtable.children[i++] = verified
				j++
			}
			else
			{
				j++
			}
		}
		newtable.children = newtable.children.filter { x -> (x.symbols.size != 0 || x.children.size != 0)}.toMutableList()
		return newtable
	}

	fun getSymbolTable(): symbolScope {
		while(currentScope.parent != null)
		{
			currentScope = currentScope.parent!!
		}
		val reduced = reduceSymbolTable(currentScope)
		val shifted = shiftUpSymbolTable(reduced)
		return shifted
	}
}