package ASTTransforms

import AST.*

class ASTPrinter : Visitor {
	fun visitTree(program: List<Declaration>) {
		for (node in program) {
			node.accept(this)
		}
	}

	override fun visit(node: Lambda) {
		println(
			"Line ${node.line} - Visited Lambda with params ${
				(node.params.map { "${it.first} : ${it.second}," }).joinToString(
					", "
				)
			}, returning ${node.return_type}"
		)
		print("Body -")
		node.accept(this)
		println("-- End --")
	}

	override fun visit(node: BinaryOp) {
		println("Line ${node.line} - Visited binary op ${node.op}")
		node.left.accept(this)
		node.right.accept(this)
		println("-- End  binary op ${node.op} --")
	}

	override fun visit(node: UnaryOp) {
		println("Line ${node.line} - Visited unary op ${node.op}")
		node.inner.accept(this)
		println("-- End binary op ${node.op} --")
	}

	override fun visit(node: ListAccess) {
		println("Line ${node.line} - Visited list ${node.ident} at index ${node.index?.accept(this)}")
		println("-- End --")
	}

	override fun visit(node: VariableAccess) {
		println("Line ${node.line} - Visited variable ${node.ident}")
	}

	override fun visit(node: Modify) {
		println(
			"Line ${node.line} - Visited ${if (node.return_previous) "postfix" else "prefix"} modifier of ${node.ident} with value ${
				node.modify_by.accept(
					this
				)
			}"
		)
		println("-- End --")
	}

	override fun visit(node: ScopeOf) {
		println("Line ${node.line} - Visited scope modifier for scope ${node.inscope} with expression")
		node.perform.accept(this)
		println("-- End --")
	}

	override fun visit(node: Call) {
		println("Line ${node.line} - Visited call of function ${node.func} with expression")
		for (item in node.params) {
			item.accept(this)
		}
		println("-- End --")
	}

	override fun visit(node: IntegerNode) {
		println("Line ${node.line} - ${node.num}")
	}

	override fun visit(node: Floating) {
		println("Line ${node.line} -${node.num}")
	}

	override fun visit(node: Bool) {
		println("Line ${node.line} -${node.bool}")
	}

	override fun visit(node: StringLit) {
		println("Line ${node.line} - ${node.str}")
	}

	override fun visit(node: CharNode) {
		println("Line ${node.line} - ${node.char_}")
	}

	override fun visit(node: If) {
		println("Line ${node.line} - If --")
		node.conditional.accept(this)
		println("-- Then --")
		node.consequent.accept(this)
		println("-- Else --")
		node.alternate?.accept(this)
		println("-- End --")
	}

	override fun visit(node: For) {
		println("Line ${node.line} - For --")
		println("-- Init --")
		node.initalizer?.accept(this)
		println("-- Conditional --")
		node.conditional?.accept(this)
		println("-- Increment --")
		node.iteration?.accept(this)
		println("-- Block --")
		node.body.accept(this)
		println("-- End --")
	}

	override fun visit(node: ForEach) {
		println("Line ${node.line} - Foreach --")
		println("-- Iterval --")
		println(node.iterval)
		println("-- Collection --")
		println(node.collectionvar)
		println("-- Block --")
		node.body.accept(this)
		println("-- End --")
	}

	override fun visit(node: While) {
		println("Line ${node.line} - While --")
		println("-- Conditional --")
		node.conditional.accept(this)
		println("-- Block --")
		node.body.accept(this)
		println("-- End --")
	}

	override fun visit(node: DoWhile) {
		println("Line ${node.line} - Do --")
		println("-- Block --")
		node.body.accept(this)
		println("-- Conditional --")
		node.conditional.accept(this)
		println("-- End --")
	}

	override fun visit(node: Continue) {
		println("Line ${node.line} - Continue")
	}

	override fun visit(node: Break) {
		println("Line ${node.line} - Break")
	}

	override fun visit(node: Label) {
		println("Line ${node.line} - Label ${node.ident}")
	}

	override fun visit(node: Switch) {
		println("Line ${node.line} - Switch --")
		node.switchon.accept(this)

		for (item in node.cases) {
			println("case - if")
			item.first.accept(this)
			println("then -")
			item.second.accept(this)
			println("end")
		}
		println("end switch -")
	}

	override fun visit(node: Assignment) {
		println("Line ${node.line} - Assign --")
		println("-- To --")
		node.ident.accept(this)
		println("-- With --")
		node.expr.accept(this)
		println("-- End --")
	}

	override fun visit(node: Return) {
		println("Line ${node.line} - Return --")
		if (node.expr != null) {
			node.expr.accept(this)
		}
		println("-- End --")
	}

	override fun visit(node: Goto) {
		println("Line ${node.line} - Goto ${node.gotoident} --")
	}

	override fun visit(node: Try) {
		println("Line ${node.line} - Try --")
		node.block.accept(this)
		println("Line ${node.line} - Catch -- ${node.catches} -- ${node.catchesAs}")
		node.catch.accept(this)
	}

	override fun visit(node: Throw) {
		println("Line ${node.line} - Throw -- ${node.ident} -- Expr")
		for (item in node.params) {
			item.accept(this)
		}
	}

	override fun visit(node: ExprStatement) {
		node.expr?.accept(this)
	}

	override fun visit(node: Block) {
		println("Line ${node.line} - Block --")
		for (item in node.statements) {
			item.accept(this)
		}
		println("End Block --")
	}

	override fun visit(node: SimpleVarDeclaration) {
		println("Line ${node.line} - Visited var ${node.name.first} with type ${node.name.second}.")
		node.definition?.accept(this)
	}

	override fun visit(node: ArrayDeclaration) {
		println("Line ${node.line} - Visited array ${node.name.first} with type ${node.name.second} with size ${node.size}")
		if (node.definition.isEmpty()) {
			println("Uninitalized.")
		} else {
			var i = 0
			for (member in node.definition) {
				println("Member ${i++} -")
				member.accept(this)
			}
		}
	}

	override fun visit(node: EnumDeclaration) {
		println("Line ${node.line} - Visited enum ${node.name}")
		println(
			"Inherits from ${
				(node.members.map { "${it.first} - ${it.second}" }).joinToString(
					",\n"
				)
			}"
		)
	}

	override fun visit(node: ClassDeclaration) {
		println("Line ${node.line} - Visited class ${node.name}")
		println(
			"Inherits from ${
				(node.inheritsfrom.map { it }).joinToString(
					",\n"
				)
			}"
		)
		println("Body -")
		for (decl in node.members) {
			println("Member - ${decl.accept(this)}")
		}
	}

	override fun visit(node: StructDeclaration) {
		println("Line ${node.line} - Visited struct ${node.name}")
		println("Body -")
		for (decl in node.members) {
			println("Member - ${decl.accept(this)}")
		}
	}

	override fun visit(node: FunctionDeclaration) {
		println(
			"Line ${node.line} - Visited function ${node.name} with params ${
				(node.parameters.map { "${it.first} : ${it.second}," }).joinToString(
					",\n"
				)
			}, returning ${node.returnType}"
		)
		print("Body -")
		node.body.accept(this)
	}
}