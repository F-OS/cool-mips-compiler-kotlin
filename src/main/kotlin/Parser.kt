import AST.*
import ParserExcptions.ExpressionParserException
import ParserExcptions.StatementParserException

class Parser(private val tokens: MutableList<Token>) {
	/*

			EXPRESSIONS

	 */
	private fun parseExpression(): Expression {
		return parseLambda()
	}

	private fun parseLambda(): Expression {
		return if (tokens[0] is Ident && (tokens[0] as Ident).ident == "lambda") {
			tokens.removeFirst()
			parseLambdaBody()
		} else {
			parseLogicalOrExpression()
		}
	}

	private fun parseLambdaBody(): Expression {
		return Lambda(parseParams(), parseBlock())
	}

	private fun parseParams(): List<String> {
		if (tokens[0] !is LParen) {
			throw ExpressionParserException("Line ${tokens[0].line} - A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses.")
		}
		tokens.removeFirst()
		val params = mutableListOf<String>()
		var expectComma = false
		while (true) {
			when (tokens[0]) {
				is Ident -> {
					if (expectComma) {
						println("WARNING: Expected a comma at identifier ${(tokens[0] as Ident).ident} in parameter definition on line ${tokens[0].line}")
					}
					expectComma = true
					params.add((tokens[0] as Ident).ident)
					tokens.removeFirst()
				}

				is RParen -> {
					if (params.isEmpty()) {
						println("WARNING: Empty parameter definition at line ${tokens[0].line}.")
					}
					tokens.removeFirst()
					break
				}

				is Comma -> {
					if (expectComma) {
						expectComma = false
					} else {
						println("WARNING: Unexpected comma in parameter definition at line ${tokens[0].line}")
					}
					tokens.removeFirst()
				}

				else -> throw ExpressionParserException("Line ${tokens[0].line} - Unexpected token in parameter list.")
			}
		}
		return params
	}

	private fun parseLogicalOrExpression(): Expression {
		var lhs = parseLogicalAndExpression()
		while (tokens[0] is Or) {
			tokens.removeFirst()
			val rhs = parseLogicalAndExpression()
			lhs = BinaryOp(lhs, BinaryOps.Or, rhs)
		}
		return lhs
	}


	private fun parseLogicalAndExpression(): Expression {
		var lhs = parseBitwiseAnd()
		while (tokens[0] is And) {
			tokens.removeFirst()
			val rhs = parseBitwiseAnd()
			lhs = BinaryOp(lhs, BinaryOps.And, rhs)
		}
		return lhs
	}

	private fun parseBitwiseAnd(): Expression {
		var lhs = parseBitwiseXor()
		while (tokens[0] is Bitwise_And) {
			tokens.removeFirst()
			val rhs = parseBitwiseXor()
			lhs = BinaryOp(lhs, BinaryOps.Bitwise_And, rhs)
		}
		return lhs
	}

	private fun parseBitwiseXor(): Expression {
		var lhs = parseBitwiseOr()
		while (tokens[0] is Bitwise_Xor) {
			tokens.removeFirst()
			val rhs = parseBitwiseOr()
			lhs = BinaryOp(lhs, BinaryOps.Bitwise_Xor, rhs)
		}
		return lhs
	}

	private fun parseBitwiseOr(): Expression {
		var lhs = parseEquality()
		while (tokens[0] is Bitwise_Or) {
			tokens.removeFirst()
			val rhs = parseEquality()
			lhs = BinaryOp(lhs, BinaryOps.Bitwise_Or, rhs)
		}
		return lhs
	}

	private fun parseEquality(): Expression {
		var lhs = parseRelationalExpression()
		while (true) {
			lhs = when (tokens[0]) {
				is EqualTo -> {
					tokens.removeFirst()
					val rhs = parseRelationalExpression()
					BinaryOp(lhs, BinaryOps.EqualTo, rhs)
				}

				is NotEqualTo -> {
					tokens.removeFirst()
					val rhs = parseRelationalExpression()
					BinaryOp(lhs, BinaryOps.NotEqualTo, rhs)
				}

				else -> return lhs
			}
		}
	}

	private fun parseRelationalExpression(): Expression {
		var lhs = parseBitwiseShift()
		while (true) {
			lhs = when (tokens[0]) {
				is GreaterThan -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.GreaterThan, rhs)
				}

				is GreaterEqual -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.GreaterEqual, rhs)
				}

				is LessEqual -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.LessEqual, rhs)
				}

				is LessThan -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.LessThan, rhs)
				}

				else -> return lhs
			}
		}
	}

	private fun parseBitwiseShift(): Expression {
		var lhs = parseTerm()
		while (tokens[0] is Bitwise_LShift || tokens[0] is Bitwise_RShift) {
			lhs = when (tokens[0]) {
				is Bitwise_LShift -> {
					tokens.removeFirst()
					val rhs = parseTerm()
					BinaryOp(lhs, BinaryOps.Bitwise_LShift, rhs)
				}

				is Bitwise_RShift -> {
					tokens.removeFirst()
					val rhs = parseTerm()
					BinaryOp(lhs, BinaryOps.Bitwise_RShift, rhs)
				}

				else -> return lhs
			}
		}
		return lhs
	}

	private fun parseTerm(): Expression {
		var lhs = parseFactor()
		while (true) {
			lhs = when (tokens[0]) {
				is Add -> {
					tokens.removeFirst()
					val rhs = parseFactor()
					BinaryOp(lhs, BinaryOps.Add, rhs)
				}

				is Sub -> {
					tokens.removeFirst()
					val rhs = parseFactor()
					BinaryOp(lhs, BinaryOps.Sub, rhs)
				}

				else -> return lhs
			}
		}
	}

	private fun parseFactor(): Expression {
		var lhs = parseTightlyBindingFactor()
		while (true) {
			lhs = when (tokens[0]) {
				is Mul -> {
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Mul, rhs)
				}

				is Div -> {
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Div, rhs)
				}

				is Mod -> {
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Mod, rhs)
				}

				else -> return lhs
			}
		}
	}

	private fun parseTightlyBindingFactor(): Expression {
		var lhs = parseUnary()
		while (tokens[0] is Pow) {
			tokens.removeFirst()
			val rhs = parseUnary()
			lhs = BinaryOp(lhs, BinaryOps.Pow, rhs)
		}
		return lhs
	}


	private fun parseUnary(): Expression {
		return if (tokens[0] is Bitwise_Not) {
			tokens.removeFirst()
			UnaryOp(UnaryOps.BNot, parseUnary())
		} else if (tokens[0] is Sub) {
			tokens.removeFirst()
			UnaryOp(UnaryOps.Invert, parseUnary())
		} else {
			parseLogicalNotExpression()
		}
	}

	private fun parseLogicalNotExpression(): Expression {
		return if (tokens[0] is Not) {
			tokens.removeFirst()
			val notExpr = parseLogicalNotExpression()
			UnaryOp(UnaryOps.Not, notExpr)
		} else {
			parseIncDec()
		}
	}

	private fun parseIncDec(): Expression {
		return when (tokens[0]) {

			is Inc -> {
				tokens.removeFirst()
				if (tokens[0] is Ident) {
					val incExpr = (tokens[0] as Ident).ident
					tokens.removeFirst()
					Modify(incExpr, false, Integer(1))
				} else {
					throw ExpressionParserException("Line ${tokens[0].line} - Expected identifier after increment op.")
				}
			}

			is Dec -> {
				tokens.removeFirst()
				if (tokens[0] is Ident) {
					val incExpr = (tokens[0] as Ident).ident
					tokens.removeFirst()
					Modify(incExpr, false, Integer(-1))
				} else {
					throw ExpressionParserException("Line ${tokens[0].line} - Expected identifier after decrement op.")
				}
			}

			is Ident -> {
				when (tokens[1]) {
					is Inc -> {
						val incExpr = (tokens[0] as Ident).ident
						tokens.removeFirst()
						tokens.removeFirst()
						Modify(incExpr, true, Integer(1))
					}

					is Dec -> {
						val incExpr = (tokens[0] as Ident).ident
						tokens.removeFirst()
						tokens.removeFirst()
						Modify(incExpr, true, Integer(-1))
					}

					else -> {
						parseCall()
					}
				}
			}

			else -> {
				parseCall()
			}
		}
	}

	private fun parseCall(): Expression {
		if (tokens[0] is Ident) {
			val identifier = (tokens[0] as Ident).ident
			tokens.removeFirst()
			return when {
				tokens[0] is LBracket -> {
					val idx = parseExpression()

					if (tokens[0] !is RBracket) {
						throw ExpressionParserException("Line ${tokens[0].line} - Expected closing brace on list access.")
					}

					tokens.removeFirst()
					ListAccess(identifier, idx)
				}

				tokens[0] is Dot -> {
					val rhs = parseExpression()

					if (rhs !is VariableAccess && rhs !is ListAccess && rhs !is Call) {
						throw ExpressionParserException("Line ${tokens[0].line} - Invalid ScopeOf expression, expected variable, list, or function call.")
					}

					ScopeOf(identifier, rhs)
				}

				tokens[0] is LParen -> {
					tokens.removeFirst()
					val params = mutableListOf<Expression>()

					while (true) {
						when {
							tokens[0] is RParen -> {
								tokens.removeFirst()
								break
							}

							tokens[0] is Comma -> {
								tokens.removeFirst()
							}

							tokens[0] is EndOfFile || tokens[0] is Semicolon -> {
								throw ExpressionParserException("Line ${tokens[0].line} - Invalid function call. Parameter list is not terminated.")
							}

							else -> {
								params.add(parseExpression())
							}
						}
					}
					// Return the Call expression with identifier and parameters
					Call(identifier, params)
				}

				else -> {
					VariableAccess(identifier)
				}
			}
		}

		return parsePrimary()
	}

	private fun parsePrimary(): Expression {
		when {
			tokens[0] is EndOfFile -> {
				throw ExpressionParserException("Line ${tokens[0].line} - The provided file is empty.")
			}

			tokens[0] is Unimplemented -> {
				throw ExpressionParserException("Line ${tokens[0].line} - Error, bad token provided. Got '${(tokens[0] as Unimplemented).c}'")
			}

			tokens[0] is Num -> {
				val num = tokens[0] as Num
				tokens.removeFirst()
				return if (num.is_int) {
					Integer(num.int)
				} else {
					Floating(num.dbl)
				}
			}

			tokens[0] is StringTok -> {
				tokens.removeFirst()
				return StringLit((tokens[0] as StringTok).str)
			}

			tokens[0] is CharTok -> {
				tokens.removeFirst()
				return Char_((tokens[0] as CharTok).c)
			}

			tokens[0] is Ident -> {
				val idt = tokens[0] as Ident
				return when (idt.ident) {
					"true" -> {
						tokens.removeFirst()
						Bool(true)
					}

					"false" -> {
						tokens.removeFirst()
						Bool(false)
					}

					else -> {
						throw ExpressionParserException("Line ${tokens[0].line} - Error, bad token provided. Got '${idt.ident}', expecting bool.")
						// Todo: Implement types from enums.
					}
				}
			}

			tokens[0] is LParen -> {
				tokens.removeFirst()
				val parenthetical = parseExpression()
				if (tokens[0] !is RParen) {
					throw ExpressionParserException("Line ${tokens[0].line} - Error, unterminated parenthetical. Got '${(tokens[0] as Ident).ident}', expecting right paren ')'.")
				}
				tokens.removeFirst()
				return parenthetical
			}

			else -> throw ExpressionParserException("Line ${tokens[0].line} - Error, unexpected token. Got '${tokens[0]::class.simpleName}', expecting integer or float or double or bool or enum type or parenthetical.")
		}
	}
	/*

		STATEMENTS

	 */

	private fun parseStatement(): Statement {
		val statement = if (tokens[0] is Ident) {
			when ((tokens[0] as Ident).ident) {
				"if" -> {
					tokens.removeFirst()
					parseIf()
				}

				"for" -> {
					tokens.removeFirst()
					parseForLoop()
				}

				"foreach" -> {
					tokens.removeFirst()
					parseForEachLoop()
				}

				"while" -> {
					tokens.removeFirst()
					parseWhile()
				}

				"do" -> {
					tokens.removeFirst()
					parseDo()
				}

				"continue" -> {
					tokens.removeFirst()
					Continue()
				}

				"break" -> {
					tokens.removeFirst()
					Break()
				}

				"switch" -> {
					tokens.removeFirst()
					parseSwitch()
				}

				"return" -> {
					tokens.removeFirst()
					parseReturn()
				}

				"goto" -> {
					tokens.removeFirst()
					parseGoto()
				}

				"try" -> {
					tokens.removeFirst()
					parseTry()
				}

				"throw" -> {
					tokens.removeFirst()
					parseThrow()
				}

				else -> {
					if (tokens[1] is Assign || tokens[1] is AddAssign || tokens[1] is SubAssign ||
						tokens[1] is MulAssign || tokens[1] is DivAssign || tokens[1] is ModAssign ||
						tokens[1] is PowAssign || tokens[1] is AndAssign || tokens[1] is OrAssign ||
						tokens[1] is LShiftAssign || tokens[1] is RShiftAssign || tokens[1] is XorAssign
					) {
						parseAssignment()
					} else if (tokens[1] is Colon) {
						parseLabel()
					} else {
						parseExpressionStatement()
					}
				}
			}
		} else if (tokens[0] is LBrace) {
			parseBlock()
		} else {
			parseExpressionStatement()
		}

		if (tokens[0] !is Semicolon) {
			throw StatementParserException("Line ${tokens[0].line} - Error, all statements must end with semicolons. Got '${(tokens[0])}', expecting semicolon ';'.")
		}
		tokens.removeFirst()
		return statement
	}


	private fun parseIf(): Statement {
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, while loops must be followed by control structure. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		val conditional: Expression = parseExpression()
		if (tokens[0] !is RParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, while loops must be followed by control structure. Got '${(tokens[0])}', expecting right paren ')'.")
		}
		tokens.removeFirst()
		val consequent = parseBlock()
		var alternate: Statement? = null
		if ((tokens[0] is Ident && (tokens[0] as Ident).ident == "else")) {
			tokens.removeFirst()
			alternate = parseBlock()
		}
		return If(conditional, consequent, alternate)
	}

	private fun parseBlock(): Statement {
		if (tokens[0] !is LBrace) {
			throw StatementParserException("Line ${tokens[0].line} - Error, block expected.")
		}
		tokens.removeFirst()
		val block = mutableListOf<Declaration>()
		while (tokens[0] !is RBrace) {
			block.add(parseDeclaration())
			if (tokens[0] is EndOfFile) {
				throw StatementParserException("Line ${tokens[0].line} - Error, unterminated block.")
			}
		}
		tokens.removeFirst()
		return Block(block)
	}

	private fun parseForLoop(): Statement {
		var initializer: Declaration? = null
		var conditional: Expression? = null
		var iteration: Statement? = null
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, for loops must be followed by control structure. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		if (tokens[0] !is Semicolon) {
			initializer = parseDeclaration()
			if (tokens[0] is Semicolon || tokens[0] is Comma) {
				tokens.removeFirst()
			} else if (tokens[0] is RParen) {
				tokens.removeFirst()
				val block = parseBlock()
				return For(initializer, null, null, block)
			} else {
				throw StatementParserException("Line ${tokens[0].line} - Error, unexpected token. Got '${(tokens[0])}', expecting right paren ')' or semicolon ';' or comma ','.")
			}
		}
		if (tokens[0] !is Semicolon) {
			conditional = parseExpression()
			if (tokens[0] is Semicolon || tokens[0] is Comma) {
				tokens.removeFirst()
			} else if (tokens[0] is RParen) {
				tokens.removeFirst()
				val block = parseBlock()
				return For(initializer, conditional, null, block)
			} else {
				throw StatementParserException("Line ${tokens[0].line} - Error, unexpected token. Got '${(tokens[0])}', expecting right paren ')' or semicolon ';' or comma ','.")
			}
		}
		if (tokens[0] !is Semicolon) {
			iteration = parseStatement()
			if (tokens[0] is Semicolon || tokens[0] is Comma) {
				tokens.removeFirst()
			} else if (tokens[0] is RParen) {
				tokens.removeFirst()
				val block = parseBlock()
				return For(initializer, conditional, iteration, block)
			} else {
				throw StatementParserException("Line ${tokens[0].line} - Error, unexpected token. Got '${(tokens[0])}', expecting right paren ')' or semicolon ';' or comma ','.")
			}
		}
		return For(initializer, conditional, iteration, parseBlock())
	}

	private fun parseForEachLoop(): Statement {
		val itervar: String
		val collectionvar: String
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, foreach loops must be followed by control structure. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		if (tokens[0] is Ident) {
			itervar = (tokens[0] as Ident).ident
			tokens.removeFirst()
		} else {
			throw StatementParserException("Line ${tokens[0].line} - Error, foreach loops consist of a iteration var, a colon, and a collection var. Got '${(tokens[0])}', expecting identifier.")
		}
		if (tokens[0] !is Colon) {
			throw StatementParserException("Line ${tokens[0].line} - Error, foreach loops must be followed by control structure. Got '${(tokens[0])}', expecting colon ':'.")
		}
		tokens.removeFirst()
		if (tokens[0] is Ident) {
			collectionvar = (tokens[0] as Ident).ident
			tokens.removeFirst()
		} else {
			throw StatementParserException("Line ${tokens[0].line} - Error, foreach loops consist of a iteration var, a colon, and a collection var. Got '${(tokens[0])}', expecting identifier.")
		}
		if (tokens[0] !is RParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, foreach loop control structures must be terminated. Got '${(tokens[0])}', expecting left paren ')'.")
		}
		tokens.removeFirst()
		return ForEach(itervar, collectionvar, parseBlock())
	}

	private fun parseWhile(): Statement {
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, while loops must be followed by control structure. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		val conditional: Expression = parseExpression()
		if (tokens[0] !is RParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, while loops must be followed by control structure. Got '${(tokens[0])}', expecting right paren ')'.")
		}
		tokens.removeFirst()
		return While(conditional, parseBlock())
	}

	private fun parseDo(): Statement {
		val body = parseBlock()
		if (!(tokens[0] is Ident && (tokens[0] as Ident).ident == "while")) {
			throw StatementParserException("Line ${tokens[0].line} - Error, do loops must be followed by while block and control structure. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, do loops must be followed by control structure. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		val conditional: Expression = parseExpression()
		if (tokens[0] !is RParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, do loops must be followed by control structure. Got '${(tokens[0])}', expecting right paren ')'.")
		}
		tokens.removeFirst()
		return DoWhile(conditional, body)
	}

	private fun parseSwitch(): Statement {
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, switch statements must be followed by the expression to switch on. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		val conditional: Expression = parseExpression()
		if (tokens[0] !is RParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, switch statements must be followed by the expression to switch on. Got '${(tokens[0])}', expecting right paren ')'.")
		}
		tokens.removeFirst()
		if (tokens[0] !is LBrace) {
			throw StatementParserException("Line ${tokens[0].line} - Error, switch statements must be followed by a switch block. Got '${(tokens[0])}', expecting right paren '{'.")
		}
		tokens.removeFirst()
		val cases = mutableListOf<Pair<Expression, Statement>>()
		while (true) {
			if (!(tokens[0] is Ident && (tokens[0] as Ident).ident == "case")) {
				throw StatementParserException("Line ${tokens[0].line} - Error, case blocks consist of the keyword 'case' followed by an expression, a colon, and a code block. Got '${(tokens[0])}', expecting 'case'.")
			}
			tokens.removeFirst()
			val exp: Expression = parseExpression()
			if (tokens[0] !is Colon) {
				throw StatementParserException("Line ${tokens[0].line} - Error, case expression must be followed by a colon. Got '${(tokens[0])}', expecting colon ':'.")
			}
			tokens.removeFirst()
			val block: Statement = parseBlock()
			cases.add(Pair(exp, block))
			if (tokens[0] is RBrace) {
				tokens.removeFirst()
				break
			}
			if (tokens[0] is EndOfFile) {
				throw StatementParserException("Line ${tokens[0].line} - Error, unterminated switch block. Did you forget a closing brace?")
			}
		}
		return Switch(conditional, cases)
	}

	private fun parseLabel(): Statement {
		if (tokens[0] !is Ident) {
			throw StatementParserException("Line ${tokens[0].line} - Error, invalid label. Got '${(tokens[0])}', expecting an identifier.")
		}
		val label = (tokens[0] as Ident).ident
		tokens.removeFirst()
		if (tokens[0] !is Colon) {
			throw StatementParserException("Line ${tokens[0].line} - Error, invalid label. Got '${(tokens[0])}' after '${label}', expecting a colon to end the label.")
		}
		tokens.removeFirst()
		return Label(label)
	}

	private fun parseAssignment(): Statement {
		if (tokens[0] !is Ident) {
			throw StatementParserException("Line ${tokens[0].line} - Error, invalid assignment identifier. Got '${(tokens[0])}', expecting an identifier.")
		}
		val variable = (tokens[0] as Ident).ident
		tokens.removeFirst()
		if (tokens[0] !is Assign) {
			throw StatementParserException("Line ${tokens[0].line} - Error, assignments require equals. Got '${(tokens[0])}', expecting an assignment sign '='.")

		}
		tokens.removeFirst()
		return Assignment(variable, parseExpression())
	}

	private fun parseReturn(): Statement {
		return if (tokens[0] is Semicolon) {
			Return(null)
		} else {
			Return(parseExpression())
		}
	}

	private fun parseGoto(): Statement {
		if (tokens[0] !is Ident) {
			throw StatementParserException("Line ${tokens[0].line} - Error, invalid label. Got '${(tokens[0])}', expecting an identifier.")
		}
		return Goto((tokens[0] as Ident).ident)
	}

	private fun parseTry(): Statement {
		val block = parseBlock()
		if (!(tokens[0] is Ident && (tokens[0] as Ident).ident == "catch")) {
			throw StatementParserException("Line ${tokens[0].line} - Error, try blocks consist of a code block followed by a 'catch' statement and a parenthetical. Got '${(tokens[0])}', expecting 'catch'.")
		}
		tokens.removeFirst()
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, catch statements must be followed by a parenthetical. Got '${(tokens[0])}', expecting left paren '('.")
		}
		tokens.removeFirst()
		if (tokens[0] !is Ident) {
			throw StatementParserException("Line ${tokens[0].line} - Error, catch expression parenthetical must have variable name. Got '${(tokens[0])}', expecting variable name.")
		}
		val catches = (tokens[0] as Ident).ident
		tokens.removeFirst()
		if (tokens[0] !is Colon) {
			throw StatementParserException("Line ${tokens[0].line} - Error, catch variable must be typed. Got '${(tokens[0])}', expecting colon ':'.")
		}
		tokens.removeFirst()
		if (tokens[0] !is Ident) {
			throw StatementParserException("Line ${tokens[0].line} - Error, catch expression parenthetical must have variable type. Got '${(tokens[0])}', expecting variable type.")
		}
		val catchesAs = (tokens[0] as Ident).ident
		tokens.removeFirst()
		if (tokens[0] !is RParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, catch statements must be terminated. Got '${(tokens[0])}', expecting left paren ')'.")
		}
		val catch = parseBlock()
		return Try(block, catches, catchesAs, catch)
	}

	private fun parseThrow(): Statement {
		if (tokens[0] !is Ident) {
			throw StatementParserException("Line ${tokens[0].line} - Error, throw expression must have exception name. Got '${(tokens[0])}', expecting exception name.")
		}
		val throws = (tokens[0] as Ident).ident
		tokens.removeFirst()
		if (tokens[0] !is LParen) {
			throw StatementParserException("Line ${tokens[0].line} - Error, throw expression must have arguments in parenthesies. Got '${(tokens[0])}', expecting left paren.")
		}
		tokens.removeFirst()
		val params = mutableListOf<Expression>()

		while (true) {
			when {
				tokens[0] is RParen -> {
					tokens.removeFirst()
					break
				}

				tokens[0] is Comma -> tokens.removeFirst()
				tokens[0] is EndOfFile || tokens[0] is Semicolon -> {
					throw ExpressionParserException("Line ${tokens[0].line} - Invalid exception. Parameter list is not terminated.")
				}

				else -> params.add(parseExpression())
			}
		}
		return Throw(throws, params)
	}

	private fun parseExpressionStatement(): Statement {
		return ExprStatement(parseExpression())
	}
	/*
		DECLARATIONS
	 */

	fun parseDeclaration(): Declaration {
		return parseStatement()
	}

}