package ASTTransforms.TreeTyper

import AST.*
import ASTTransforms.Visitor

object Counter {
    private var counter = 0
    fun count(): Int = counter++
}

class TypeGenerator : Visitor {
    private val inheritanceMap: MutableMap<String, MutableList<String>> = mutableMapOf()
    private var possibleRets: MutableList<Type> = mutableListOf()
    private var returnTracing: Boolean = false
    private var expectingThrow: Boolean = false
    private var expectedThrow: ArrayDeque<String> = ArrayDeque()
    private var currentScope: Scope = Scope()
    var gotoTabel: MutableList<Pair<String, ASTRoot>> = mutableListOf()
    var unresolvedGoto: MutableList<String> = mutableListOf()
    var reservedFuncs: MutableList<Pair<String, Type>> = mutableListOf(
        "print" to nullType,
    )

    var errors = 0
    private val lambdacount = Counter
    fun visitTree(program: List<Declaration>): List<Declaration> {
        for (node in program) {
            node.accept(this)
        }
        return program
    }

    /*
     * Visitor Methods
     */
    override fun visit(node: Lambda): Type {
        val name = lambdaUUID()
        val func: FunctionSymbol = registerFunction(name, node.params, node.return_type)
        enterGenericScope()
        val cons = node.block
        val associatedProcedures: MutableList<Declaration> = mutableListOf()
        if (cons is Block && (cons).statements.isNotEmpty()) {
            val block = (cons).statements
            for (stmt in block) {
                associatedProcedures += stmt
                stmt.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty consequent in lambda on line ${node.line}")
        }
        verifyFunction(func)
        exitScope()
        registerClosureWithFunc(func, associatedProcedures)
        val lambda = lambdaType(func)
        node.associatedType = lambda
        return lambdaType(func)
    }

    override fun visit(node: BinaryOp): Type {
        val leftType = node.left.accept(this) as Type
        val rightType = node.right.accept(this) as Type
        val resType = when (val op = node.op) {
            BinaryOps.And, BinaryOps.Or -> {
                binaryOp(op, leftType, rightType, listOf(boolType), listOf(boolType), listOf(boolType))
            }

            BinaryOps.GreaterThan, BinaryOps.GreaterEqual, BinaryOps.LessEqual, BinaryOps.LessThan -> {
                binaryOp(
                    op,
                    leftType,
                    rightType,
                    listOf(integerType, floatingType, charType),
                    listOf(integerType, floatingType, charType),
                    listOf(boolType)
                )
            }

            BinaryOps.EqualTo, BinaryOps.NotEqualTo -> {
                binaryOp(
                    op,
                    leftType,
                    rightType,
                    listOf(integerType, floatingType, charType, stringType, lambdaType(null), objectType(null)),
                    listOf(integerType, floatingType, charType, stringType, lambdaType(null), objectType(null)),
                    listOf(boolType)
                )
            }

            BinaryOps.Add, BinaryOps.Sub, BinaryOps.Mul, BinaryOps.Div, BinaryOps.Pow -> {
                binaryOp(
                    op,
                    leftType,
                    rightType,
                    listOf(integerType, floatingType),
                    listOf(integerType, floatingType),
                    listOf(integerType, floatingType)
                )
            }

            BinaryOps.Mod -> {
                binaryOp(
                    op,
                    leftType,
                    rightType,
                    listOf(integerType),
                    listOf(integerType),
                    listOf(integerType)
                )
            }

            BinaryOps.Bitwise_LS, BinaryOps.Bitwise_RS -> {
                binaryOp(
                    op,
                    leftType,
                    rightType,
                    listOf(integerType, floatingType),
                    listOf(integerType),
                    listOf(integerType, floatingType)
                )
            }

            BinaryOps.Bitwise_And, BinaryOps.Bitwise_Or, BinaryOps.Bitwise_Xor -> {
                binaryOp(
                    op,
                    leftType,
                    rightType,
                    listOf(integerType, floatingType, charType),
                    listOf(integerType, floatingType, charType),
                    listOf(integerType, floatingType, charType)
                )
            }
        }
        node.associatedType = resType
        return resType
    }


    override fun visit(node: UnaryOp): Type {
        val innerType = node.inner.accept(this) as Type
        val outerType = when (val op = node.op) {
            UnaryOps.Not -> {
                unaryOp(
                    op,
                    innerType,
                    listOf(boolType),
                    listOf(boolType)
                )
            }

            UnaryOps.BNot -> {
                unaryOp(
                    op,
                    innerType,
                    listOf(integerType, floatingType, charType),
                    listOf(integerType, floatingType, charType)
                )
            }

            UnaryOps.Invert -> {
                unaryOp(
                    op,
                    innerType,
                    listOf(integerType, floatingType),
                    listOf(integerType, floatingType)
                )
            }
        }
        node.associatedType = outerType
        return outerType
    }

    override fun visit(node: ListAccess): Type {
        val listAccessSymbol = getSymbol(node.ident)
        if (listAccessSymbol.name.endsWith("!!UNRESOLVED!!")) {
            println("ERROR: List ${node.ident} does not exist.")
            errors++
            return (listAccessSymbol as VariableSymbol).type
        }
        val index = if (node.index != null) {
            node.index.accept(this) as Type
        } else {
            print("Line ${node.line} - ERROR: ListAccess needs variable.")
            errors++
            nullType
        }
        restrictType(index, listOf(integerType))
        val type = symbolToType(listAccessSymbol)
        node.associatedType = type
        return type
    }

    override fun visit(node: VariableAccess): Type {
        val variableAccessSymbol = getSymbol(node.ident)
        if (variableAccessSymbol.name.endsWith("!!UNRESOLVED!!")) {
            println("ERROR: Variable ${node.ident} does not exist.")
            errors++
            return (variableAccessSymbol as VariableSymbol).type
        }
        verifyDefined(node.ident)
        val type = symbolToType(variableAccessSymbol)
        node.associatedType = type
        return type
    }

    override fun visit(node: Modify): Type {
        val modificand = node.ident.accept(this) as Type
        val modifier = node.modify_by.accept(this) as Type
        val result = binaryOp(
            BinaryOps.Add,
            modifier, modificand,
            listOf(integerType, floatingType),
            listOf(integerType, floatingType),
            listOf(integerType, floatingType)

        )
        node.associatedType = result
        return result
    }

    override fun visit(node: ScopeOf): Type {
        val symb = getSymbol(node.inscope)
        if (symb.name.endsWith("!!UNRESOLVED!!")) {
            println("ERROR: Scope ${node.inscope} does not exist.")
            errors++
            return nullType
        }
        setObjScope(symb)
        val variable = node.perform.accept(this) as Type
        exitObjScope()
        node.associatedType = variable
        return variable
    }

    override fun visit(node: Call): Type {
        val paramList: MutableList<Type> = mutableListOf()
        for (param in node.params) {
            paramList.add(param.accept(this) as Type)
        }
        val funcType = funcCall(node.func, paramList)
        node.associatedType = funcType
        return funcType
    }

    override fun visit(node: Ternary): Type {
        restrictType(node.condition.accept(this) as Type, listOf(boolType))
        val cons = node.consequent.accept(this) as Type
        val alt = node.alternate.accept(this) as Type
        equateType(cons, alt)
        node.associatedType = cons
        return cons
    }

    override fun visit(node: IntegerNode): Type {
        node.associatedType = integerType
        return integerType
    }

    override fun visit(node: Floating): Type {
        node.associatedType = floatingType
        return floatingType
    }

    override fun visit(node: Bool): Type {
        node.associatedType = boolType
        return boolType
    }

    override fun visit(node: StringLit): Type {
        node.associatedType = stringType
        return stringType
    }

    override fun visit(node: CharNode): Type {
        node.associatedType = charType
        return charType
    }

    override fun visit(node: If): Type {
        restrictType(node.conditional.accept(this) as Type, listOf(boolType))

        enterGenericScope()
        val cons = node.consequent
        if (cons is Block && (cons).statements.isNotEmpty()) {
            val block = (cons).statements
            for (stmt in block) {
                stmt.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty consequent in If statement on line ${node.line}")
        }
        exitScope()


        enterGenericScope()
        val alt = node.alternate
        if (alt != null && alt is Block) {
            val blockalt = (alt).statements
            for (stmt in blockalt) {
                stmt.accept(this)
            }
        }
        exitScope()

        node.associatedType = nullType
        return nullType
    }

    override fun visit(node: For): Type {
        enterGenericScope()
        if (node.initalizer != null) {
            node.initalizer.accept(this)
        }

        if (node.conditional != null) {
            restrictType(node.conditional.accept(this) as Type, listOf(boolType))
        }

        if (node.iteration != null) {
            node.iteration.accept(this)
        }


        val cons = node.body
        if (cons is Block && (cons).statements.isNotEmpty()) {
            val block = (cons).statements
            for (stmt in block) {
                stmt.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty consequent in For statement on line ${node.line}")
        }

        exitScope()

        return nullType
    }

    override fun visit(node: ForEach): Type {
        val iter = node.iterval
        enterGenericScope()
        registerSymbol(iter, getArrayType(node.collectionvar), true)
        val cons = node.body
        if (cons is Block && (cons).statements.isNotEmpty()) {
            val block = (cons).statements
            for (stmt in block) {
                stmt.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty consequent in ForEach statement on line ${node.line}")
        }
        exitScope()

        return nullType
    }

    override fun visit(node: While): Type {
        enterGenericScope()
        restrictType(node.conditional.accept(this) as Type, listOf(boolType))
        val cons = node.body
        if (cons is Block && (cons).statements.isNotEmpty()) {
            val block = (cons).statements
            for (stmt in block) {
                stmt.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty consequent in While statement on line ${node.line}")
        }

        exitScope()

        return nullType
    }

    override fun visit(node: DoWhile): Type {
        enterGenericScope()

        restrictType(node.conditional.accept(this) as Type, listOf(boolType))
        val cons = node.body
        if (cons is Block && (cons).statements.isNotEmpty()) {
            val block = (cons).statements
            for (stmt in block) {
                stmt.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty consequent in Do-While statement on line ${node.line}")
        }

        exitScope()

        return nullType
    }

    override fun visit(node: Continue): Type {
        return nullType
    }

    override fun visit(node: Break): Type {
        return nullType
    }

    override fun visit(node: Label): Type {
        insertGotoLabel(node.ident, node)
        return nullType
    }


    override fun visit(node: Switch): Type {
        enterGenericScope()

        for ((cond, body) in node.cases) {
            restrictType(cond.accept(this) as Type, listOf(integerType, charType, floatingType, boolType))
            enterGenericScope()
            if (body is Block && body.statements.isNotEmpty()) {
                val block = body.statements
                for (stmt in block) {
                    stmt.accept(this)
                }
            } else {
                println("ERROR: Empty switch case.")
            }
            exitScope()
        }

        exitScope()

        return nullType
    }

    override fun visit(node: Assignment): Type {
        val varType = node.ident.accept(this) as Type
        val assignType = node.expr.accept(this) as Type
        equateType(varType, assignType)
        node.associatedType = assignType
        return assignType
    }

    override fun visit(node: Return): Type {
        val retType = if (node.expr != null) {
            val res = node.expr.accept(this) as Type
            if (returnTracing) {
                possibleRets += res
                res
            } else {
                res
            }

        } else {
            nullType
        }
        node.associatedType = retType
        return retType
    }

    override fun visit(node: Goto): Type {
        if (labelExists(node.gotoident)) {
            println("WARNING: forward declaration of label in goto at line ${node.line}. This might lead to spagetthi code.")
            insertUnresolvedLabel(node.gotoident)
        }
        return nullType
    }


    override fun visit(node: Try): Type {
        enterGenericScope()

        beginBoundThrowScope(node.catchesAs)
        val tryBlock = node.block
        if (tryBlock is Block && tryBlock.statements.isNotEmpty()) {
            val block = tryBlock.statements
            for (stmt in block) {
                stmt.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty try block.")
        }
        endBoundThrowScope(node.catchesAs)

        exitScope()

        enterGenericScope()
        registerSymbol(node.catches, node.catchesAs, true)

        val catch = node.catch
        if (catch is Block && catch.statements.isNotEmpty()) {
            val block = catch.statements
            for (stmt in block) {
                stmt.accept(this)
            }
        } else {
            println("ERROR: Empty catch.")
        }

        exitScope()

        return nullType
    }

    override fun visit(node: Throw): Type {
        val getException = node.ident
        checkExceptionScope(getException)
        for (param in node.params) {
            param.accept(this)
        }
        return nullType
    }

    override fun visit(node: ExprStatement): Type {
        return node.expr?.accept(this) as Type
    }

    override fun visit(node: Block): Type {
        println("Issue with type annotation.")
        return nullType
    }

    // var -> value: type
    override fun visit(node: SimpleVarDeclaration): Type {
        val symb: Symbol
        if ((node.name.second.lowercase() == "function" || node.name.second == "func") && node.definition is Lambda) {
            if (node.definition == null) {
                println("Lambdas must be declared with the define.")
                return nullType
            }
            val type: Type = node.definition.accept(this) as Type
            symb = registerSymbol(node.name.first, type, true)
        } else {
            symb = registerSymbol(node.name.first, node.name.second, node.definition != null)
            if (node.definition != null) {
                equateType(symbolToType(symb), node.definition.accept(this) as Type)
            }
        }
        node.associatedType = nullType
        return nullType
    }

    // arrayType = array(size, type, elems) -> array(forall in array in range 0 to size type is arrayType or null, arrayType = type)
    override fun visit(node: ArrayDeclaration): Type {
        val symb = registerArray(node.name.first, node.name.second, node.size, node.size == -1L)
        val symbType = symbolToType(symb)
        for (item in node.definition) {

            equateType(symbType, item.accept(this) as Type)
        }
        node.associatedType = nullType
        return nullType
    }

    // enumType = enum(enumElems) -> reserve at current scope all enumElems
    override fun visit(node: EnumDeclaration): Type {
        val enum = EnumSymbol(node.name, node.members.map { x -> x.first })
        currentScope.defineSymbol(node.name, enum)
        for (member in node.members) {
            currentScope.defineSymbol(member.first, enumMembSymb(member.first, member.second))
        }
        node.associatedType = nullType
        return nullType
    }


    // classType = class(name, inherit, funcs, fields) -> type(name) -> scope (funcs in inherit + funcs, fields in inherit + fields)
    override fun visit(node: ClassDeclaration): Type {
        val classInherits = processInheritance(node.name, node.inheritsfrom)

        enterGenericScope()

        for (decls in node.members) {
            decls.accept(this)
        }

        val curscope = getScope()
        exitScope()
        registerClass(node.name, classInherits, curscope)
        node.associatedType = nullType
        return nullType
    }

    // classType = class(name, funcs, fields) -> type(name) -> scope (funcs, fields)
    override fun visit(node: StructDeclaration): Type {
        enterGenericScope()
        for (decls in node.members) {
            decls.accept(this)
        }

        val curscope = getScope()
        exitScope()
        registerStruct(node.name, curscope)
        node.associatedType = nullType
        return nullType
    }


    // funType = (params) -> returnType
    override fun visit(node: FunctionDeclaration): Type {
        val func: FunctionSymbol = registerFunction(node.name, node.parameters, node.returnType)
        enterGenericScope()
        val funcBlock = node.body
        val associatedProcedures: MutableList<Declaration> = mutableListOf()
        if (funcBlock.statements.isNotEmpty()) {
            val block = funcBlock.statements
            for (decls in block) {
                associatedProcedures += decls
                decls.accept(this)
            }
        } else {
            errors++
            println("ERROR: Empty function block.")
        }
        verifyFunction(func)
        exitScope()
        registerClosureWithFunc(func, associatedProcedures)
        node.associatedType = nullType
        return nullType
    }

    /*
     * Implementations
     */
    private fun lambdaUUID(): String {
        return "__LAMBDA${lambdacount.count()}__"
    }

    // Exits the current definition scope.
    private fun exitScope() {
        val parent = currentScope.parent
        currentScope = if (parent != null) parent else {
            errors++
            println("ERROR: No parent scope exists.")
            currentScope
        }
    }

    // Enters a new generic scope.
    private fun enterGenericScope() {
        currentScope = Scope(currentScope)
    }

    // Returns the current scope.
    private fun getScope(): Scope {
        return currentScope
    }

    fun registerInheritance(subtypeName: String, superTypeName: String) {
        val superTypes = inheritanceMap.getOrPut(subtypeName) { mutableListOf() }
        superTypes.add(superTypeName)
    }

    fun isSubtypeOf(subtypeName: String, superTypeName: String): Boolean {
        if (subtypeName == superTypeName) return true

        val superTypes = inheritanceMap[subtypeName]
        if (superTypes != null) {
            for (superType in superTypes) {
                if (isSubtypeOf(superType, superTypeName)) {
                    return true
                }
            }
        }

        return false
    }

    private fun restrictType(accept: Type, acceptableTypes: List<Type>) {
        if (accept is objectType && acceptableTypes.any { it is objectType }) {
            val objlist = acceptableTypes.filter { x -> x is objectType }
            if (objlist.none { isSubtypeOf((it as objectType).name!!, accept.name!!) }) {
                errors++
                println(
                    "ERROR: Type violation. Expected one of ${
                        acceptableTypes.joinToString(",") { it.toString() }
                    }, got $accept"
                )
            }
            return
        }
        if (acceptableTypes.all { it != accept }) {
            errors++
            println(
                "ERROR: Type violation. Expected one of ${
                    acceptableTypes.joinToString(",") { it.toString() }
                }, got $accept"
            )
        }
    }

    private fun equateType(left: Type, right: Type) {
        if (left is objectType && right is objectType) {
            if (!isSubtypeOf(left.name!!, right.name!!) && !isSubtypeOf(right.name, left.name)) {
                println("ERROR: Type violation. $left != $right")
                errors++
                return
            }
            return
        }
        if (left != right) {
            if (left.canPromoteTo(right)) {
                return
            }
            println("ERROR: Type violation. $left != $right") // TODO: Actual handling.
            errors++
        }
    }

    private fun binaryOp(
        op: BinaryOps,
        leftType: Type,
        rightType: Type,
        leftExpect: List<Type>,
        rightExpect: List<Type>,
        resExpect: List<Type>
    ): Type {
        var resType = leftType
        if (leftExpect.all { it != leftType }) {
            errors++
            println(
                "ERROR: Invalid types. Binary Op $op does not accept $leftType on left of equation in equation ${
                    leftExpect.joinToString(",") { it.toString() }
                } X ${rightExpect.joinToString(",") { it.toString() }} === ${
                    resExpect.joinToString(",") { it.toString() }
                }"
            )
        }
        if (rightExpect.all { it != rightType }) {
            errors++
            println(
                "ERROR: Invalid types. Binary Op $op does not accept $leftType on right of equation in equation ${
                    leftExpect.joinToString(",") { it.toString() }
                } X ${rightExpect.joinToString(",") { it.toString() }} === ${
                    resExpect.joinToString(",") { it.toString() }
                }"
            )
        }
        resType = if (leftType != rightType) {
            if (leftType.canPromoteTo(rightType) || rightType.canPromoteTo(leftType)) {
                resType = if (leftType.canPromoteTo(rightType)) rightType else leftType
                println("WARNING: Implicit promotion. ${if (leftType.canPromoteTo(rightType)) "$leftType promoted to $rightType" else "$rightType promoted to $leftType"}")
                resType
            } else {
                println("ERROR: Invalid types. Binary Op $op cannot be applied to $leftType X $rightType == $resType")
                errors++
                nullType
            }
        } else {
            leftType
        }
        return when (op) {
            BinaryOps.And, BinaryOps.Or, BinaryOps.GreaterThan, BinaryOps.GreaterEqual, BinaryOps.EqualTo, BinaryOps.NotEqualTo, BinaryOps.LessEqual, BinaryOps.LessThan -> {
                boolType
            }

            else -> resType
        }
    }

    private fun unaryOp(
        op: UnaryOps,
        innerType: Type,
        innerExpect: List<Type>,
        resExpect: List<Type>
    ): Type {
        val resType = innerType
        if (innerExpect.all { it != innerType }) {
            errors++
            println(
                "ERROR: Invalid types. Unary Op $op does not accept $innerType on left of equation in equation f(${
                    innerExpect.joinToString(",") { it.toString() }
                }) === ${
                    resExpect.joinToString(",") { it.toString() }
                }"
            )
        }
        return resType

    }

    private fun funcCall(func: String, paramList: MutableList<Type>): Type {
        if (reservedFuncs.any { it.first == func }) {
            return reservedFuncs.first { (x, y) -> x == func }.second
        }
        val symbol = getSymbol(func)
        if (symbol is FunctionSymbol) {
            // Verify each type in the params matches with a type in the returned symbol. Convert the types in the symbol to Type objects with strToType.
            val paramTypes = symbol.params.map { param -> param.first to strToType(param.second) }
            return if (paramTypes == paramList) {
                strToType(symbol.returnType)
            } else {
                errors++
                println("ERROR: Parameter types mismatch for function $func.")
                strToType(symbol.returnType)
            }
        } else if (symbol is ClassSymbol) {
            val newobjscope = symbol.associatedScope
            return objectType(symbol.name, newobjscope)
        } else if (symbol.name.endsWith("!!UNRESOLVED!!")) {
            errors++
            println("ERROR: Undefined function $func")
            return nullType
        } else {
            errors++
            println("ERROR: Attempt to call a non-function $func.")
            return nullType
        }
    }

    private fun getSymbol(ident: String): Symbol {
        val resolveSymbol = currentScope.resolveSymbol(ident)
        return if (resolveSymbol != null) resolveSymbol else {
            currentScope.defineSymbol(ident, VariableSymbol(ident + " !!UNRESOLVED!!", integerType))
            currentScope.setIsDefined(ident)
            return VariableSymbol(ident + " !!UNRESOLVED!!", integerType)
        }
    }

    private fun registerSymbol(name: String, type: String, isDefined: Boolean): Symbol {
        return registerSymbol(name, strToType(type), isDefined)
    }

    private fun registerSymbol(name: String, type: Type, isDefined: Boolean): Symbol {
        if (currentScope.resolveSymbolCurScope(name) != null) {
            println("WARNING: Declaration of variable $name shadows previously defined variable.")
        }
        val symbol = VariableSymbol(name, type)
        currentScope.defineSymbol(name, symbol)
        if (isDefined) {
            currentScope.setIsDefined(name)
        }
        return symbol
    }

    private fun registerArray(name: String, type: String, size: Long, isDefined: Boolean): Symbol {
        return registerArray(name, strToType(type), size, isDefined)
    }

    private fun registerArray(name: String, type: Type, size: Long, isDefined: Boolean): Symbol {
        if (currentScope.resolveSymbolCurScope(name) != null) {
            println("WARNING: Declaration of array $name shadows previously defined variable.")
        }
        val symbol = ArraySymbol(name, size.toInt(), type)
        currentScope.defineSymbol(name, symbol)
        if (isDefined) {
            currentScope.setIsDefined(name)
        }
        return symbol
    }

    private fun symbolToType(symbol: Symbol): Type {
        return when (symbol) {
            is VariableSymbol -> {
                symbol.type
            }

            is ArraySymbol -> {
                symbol.elementType
            }

            else -> {
                errors++
                println("ERROR: do not call symbolToType on non-variables")
                nullType
            }
        }
    }

    fun strToType(str: String): Type {
        return when (str.lowercase()) {
            "void" -> nullType
            "int" -> integerType
            "float" -> floatingType
            "bool" -> boolType
            "char" -> charType
            "string" -> stringType
            else -> objectToType(str)
        }
    }

    private fun objectToType(str: String): Type {
        if (currentScope.resolveType(str) != null) {
            return currentScope.resolveType(str)!!
        }
        println("ERROR: unable to resolve type ${str}.")
        errors++
        return nullType
    }

    private fun registerFunction(name: String, params: List<Pair<String, String>>, returnType: String): FunctionSymbol {
        if (currentScope.resolveSymbolCurScope(name) != null) {
            println("WARNING: Declaration of array $name shadows previously defined variable.")
        }
        val symbol = FunctionSymbol(name, params, returnType)
        currentScope.defineSymbol(name, symbol)
        return symbol
    }

    private fun verifyFunction(func: FunctionSymbol) {
        val verificationScope = Scope(currentScope)
        val oldscope = currentScope
        currentScope = verificationScope
        for (param in func.params) {
            registerSymbol(param.first, param.second, true)
        }
        returnTracing = true
        for (decl in func.decls) {
            decl.accept(this)
        }
        returnTracing = false
        val verifyList = possibleRets
        possibleRets = mutableListOf()
        for (type in verifyList) {
            equateType(type, strToType(func.returnType))
        }
        currentScope = oldscope
    }

    private fun registerClosureWithFunc(
        func: FunctionSymbol,
        associatedProcedures: MutableList<Declaration>
    ) {
        func.decls = associatedProcedures
    }

    private fun beginBoundThrowScope(catchesAs: String) {
        enterGenericScope()
        expectedThrow.addLast(catchesAs)
        expectingThrow = true
    }

    private fun endBoundThrowScope(catchesAs: String) {
        if (expectedThrow.isEmpty() || expectedThrow.last() == catchesAs) {
            errors++
            println("ERROR: No throw statements in associated try-catch block.")
            return
        }
        expectedThrow.removeLast()
    }

    private fun checkExceptionScope(ident: String) {
        if (expectedThrow.isNotEmpty() && ident == expectedThrow.last()) {
            expectedThrow.removeLast()
        }
    }

    private fun verifyDefined(symbol: String) {
        if (currentScope.verifyIsDefined(symbol)) {
            return
        }
        println("ERROR: Use before symbol definition for symbol $symbol")
        errors++
    }

    private fun getArrayType(collectionvar: String): Type {
        val resolv = currentScope.resolveSymbol(collectionvar)
        if (resolv == null) {
            println("ERROR: Array $collectionvar not defined.")
            return nullType
        }
        return if (resolv is ArraySymbol) {
            resolv.elementType
        } else {
            println("ERROR: $collectionvar is not an array!")
            nullType
        }
    }

    var objScope = Scope(currentScope)
    var objScopeParent = objScope
    var objoldScope = objScopeParent
    private fun setObjScope(symb: Symbol) {
        when (symb) {
            is ClassSymbol -> {
                objoldScope = objScope
                objScope = symb.associatedScope
                objScope.parent = objoldScope
            }

            is StructSymbol -> {
                objoldScope = objScope
                objScope = symb.associatedScope
                objScope.parent = objoldScope
            }

            is VariableSymbol -> {
                if (symb.type is objectType) {
                    objoldScope = objScope
                    if (symb.type.associatedScope != null) {
                        objScope = symb.type.associatedScope
                    } else {
                        val defscope = currentScope.resolveType(symb.type.name!!) as objectType
                        val deref = defscope.associatedScope
                        if (deref != null) {
                            objScope = deref
                        } else {
                            errors++
                            println("ERROR: Unable to resolve object scope for object $symb.")
                            objoldScope = currentScope
                            return
                        }
                    }
                    objScope.parent = objoldScope
                }
            }

            else -> {
                println("ERROR: Invalid object for scopeto call.")
                objoldScope = currentScope
                return
            }
        }
        objoldScope = currentScope
        currentScope = objScope
    }

    private fun exitObjScope() {
        currentScope = objoldScope
        objScope = objScopeParent
    }

    private fun insertGotoLabel(ident: String, associatedNode: ASTRoot) {
        if (gotoTabel.all { it.first != ident }) {
            gotoTabel.add(ident to associatedNode)
            unresolvedGoto = unresolvedGoto.filter { x -> x != ident }.toMutableList()
        }
    }

    private fun insertUnresolvedLabel(ident: String) {
        if (unresolvedGoto.all { it != ident }) {
            unresolvedGoto.add(ident)
        }
    }

    private fun labelExists(ident: String): Boolean {
        return unresolvedGoto.any { it == ident }
    }


    private fun processInheritance(classname: String, inheritsfrom: List<String>): List<ClassSymbol> {
        var resolvedInherits: MutableList<ClassSymbol> = mutableListOf()
        for (inherit in inheritsfrom) {
            val resolv = currentScope.resolveSymbol(inherit)
            if (resolv is ClassSymbol) {
                resolvedInherits.add(resolv)
            } else {
                print("ERROR: Unable to resolve class inherit $inherit.")
            }
            registerInheritance(classname, inherit)
        }
        return resolvedInherits
    }

    private fun registerClass(name: String, classInherits: List<ClassSymbol>, scope: Scope): ClassSymbol {
        if (currentScope.resolveSymbolCurScope(name) != null) {
            println("WARNING: Declaration of class $name shadows previously defined variable.")
        }
        val symbol = ClassSymbol(name, classInherits, scope)
        currentScope.defineSymbol(name, symbol)
        currentScope.defineType(name, objectType(name))

        return symbol
    }

    private fun registerStruct(name: String, scope: Scope): StructSymbol {
        if (currentScope.resolveSymbolCurScope(name) != null) {
            println("WARNING: Declaration of struct $name shadows previously defined variable.")
        }
        val symbol = StructSymbol(name, scope)
        currentScope.defineSymbol(name, symbol)
        currentScope.defineType(name, objectType(name))
        return symbol
    }

}