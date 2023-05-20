import AST.*
import ParserExcptions.DeclarationParserException
import ParserExcptions.ExpressionParserException
import ParserExcptions.StatementParserException

class Parser(private val tokens: MutableList<Token>) {
	private fun isAssignment(token: Token): Boolean {
		return token is Assign || token is AddAssign || token is SubAssign ||
				token is MulAssign || token is DivAssign || token is ModAssign ||
				token is PowAssign || token is AndAssign || token is OrAssign ||
				token is LShiftAssign || token is RShiftAssign || token is XorAssign
	}

	private fun expectToken(index: Int, expectedToken: List<Token>, errorMessage: String, parserStage: Int): Token {
		if (expectedToken.none { it::class == tokens[index]::class }) {
			val line = tokens[index].line.toString()
			val tokenString = tokToString(tokens[index])
			val expectedTokensString = expectedToken.joinToString(" or ") { tokToString(it) }
			val error = "Line $line - $errorMessage Got $tokenString, expected $expectedTokensString."

			when (parserStage) {
				0 -> throw ExpressionParserException(error)
				1 -> throw StatementParserException(error)
				2 -> throw DeclarationParserException(error)
			}
		}

		val result = tokens[index]
		tokens.removeAt(index)
		return result
	}

	private fun expectIdent(
		ident: String,
		expectedIdent: List<String>,
		error: String,
		parserStage: Int,
		line: Int
	) {
		if (expectedIdent.none { ident == it }) {
			val expectedIdentString = expectedIdent.joinToString(" or ") { it }
			val error = "Line $line - $error Got '$ident', expected $expectedIdentString."

			when (parserStage) {
				0 -> throw ExpressionParserException(error)
				1 -> throw StatementParserException(error)
				2 -> throw DeclarationParserException(error)
			}
		}
	}

	private fun expectNode(
		recievedNode: ASTRoot,
		expectedNode: List<ASTRoot>,
		errorMessage: String,
		parserStage: Int
	): ASTRoot {
		if (expectedNode.none { it::class == recievedNode::class }) {
			val line = recievedNode.line.toString()
			val tokenString = nodeToString(recievedNode)
			val expectedTokensString = expectedNode.joinToString(" or ") { nodeToString(it) }
			val error = "Line $line - $errorMessage Got $tokenString, expected $expectedTokensString."

			when (parserStage) {
				0 -> throw ExpressionParserException(error)
				1 -> throw StatementParserException(error)
				2 -> throw DeclarationParserException(error)
			}
		}
		return recievedNode
	}

	private fun rejectToken(index: Int, rejectedToken: List<Token>, errorMessage: String, parserStage: Int) {
		if (rejectedToken.any { it::class == tokens[index]::class }) {
			val line = tokens[index].line.toString()
			val tokenString = tokToString(tokens[index])
			val error = "Line $line - $errorMessage Unexpected $tokenString."

			when (parserStage) {
				0 -> throw ExpressionParserException(error)
				1 -> throw StatementParserException(error)
				2 -> throw DeclarationParserException(error)
			}
		}
	}

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
		val line = tokens[0].line!!
		val params = parseParams()
		val block = parseBlock()
		return Lambda(params, block, line)
	}

	private fun parseParams(): List<String> {
		expectToken(
			0,
			listOf<Token>(LParen()),
			"A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses.",
			0
		)
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

				else -> expectToken(
					0,
					listOf<Token>(Ident(), RParen(), Comma()),
					"Unexpected token in parameter list.",
					0
				)
			}
		}
		return params
	}

	private fun parseLogicalOrExpression(): Expression {
		var lhs = parseLogicalAndExpression()
		while (tokens[0] is Or) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			val rhs = parseLogicalAndExpression()
			lhs = BinaryOp(lhs, BinaryOps.Or, rhs, line)
		}
		return lhs
	}


	private fun parseLogicalAndExpression(): Expression {
		var lhs = parseBitwiseAnd()
		while (tokens[0] is And) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			val rhs = parseBitwiseAnd()
			lhs = BinaryOp(lhs, BinaryOps.And, rhs, line)
		}
		return lhs
	}

	private fun parseBitwiseAnd(): Expression {
		var lhs = parseBitwiseXor()
		while (tokens[0] is Bitwise_And) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			val rhs = parseBitwiseXor()
			lhs = BinaryOp(lhs, BinaryOps.Bitwise_And, rhs, line)
		}
		return lhs
	}

	private fun parseBitwiseXor(): Expression {
		var lhs = parseBitwiseOr()
		while (tokens[0] is Bitwise_Xor) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			val rhs = parseBitwiseOr()
			lhs = BinaryOp(lhs, BinaryOps.Bitwise_Xor, rhs, line)
		}
		return lhs
	}

	private fun parseBitwiseOr(): Expression {
		var lhs = parseEquality()
		while (tokens[0] is Bitwise_Or) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			val rhs = parseEquality()
			lhs = BinaryOp(lhs, BinaryOps.Bitwise_Or, rhs, line)
		}
		return lhs
	}

	private fun parseEquality(): Expression {
		var lhs = parseRelationalExpression()
		while (true) {
			lhs = when (tokens[0]) {
				is EqualTo -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseRelationalExpression()
					BinaryOp(lhs, BinaryOps.EqualTo, rhs, line)
				}

				is NotEqualTo -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseRelationalExpression()
					BinaryOp(lhs, BinaryOps.NotEqualTo, rhs, line)
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
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.GreaterThan, rhs, line)
				}

				is GreaterEqual -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.GreaterEqual, rhs, line)
				}

				is LessEqual -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.LessEqual, rhs, line)
				}

				is LessThan -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.LessThan, rhs, line)
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
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseTerm()
					BinaryOp(lhs, BinaryOps.Bitwise_LShift, rhs, line)
				}

				is Bitwise_RShift -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseTerm()
					BinaryOp(lhs, BinaryOps.Bitwise_RShift, rhs, line)
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
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseFactor()
					BinaryOp(lhs, BinaryOps.Add, rhs, line)
				}

				is Sub -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseFactor()
					BinaryOp(lhs, BinaryOps.Sub, rhs, line)
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
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Mul, rhs, line)
				}

				is Div -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Div, rhs, line)
				}

				is Mod -> {
					val line = tokens[0].line!!
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Mod, rhs, line)
				}

				else -> return lhs
			}
		}
	}

	private fun parseTightlyBindingFactor(): Expression {
		var lhs = parseUnary()
		while (tokens[0] is Pow) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			val rhs = parseUnary()
			lhs = BinaryOp(lhs, BinaryOps.Pow, rhs, line)
		}
		return lhs
	}


	private fun parseUnary(): Expression {
		return if (tokens[0] is Bitwise_Not) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			UnaryOp(UnaryOps.BNot, parseUnary(), line)
		} else if (tokens[0] is Sub) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			UnaryOp(UnaryOps.Invert, parseUnary(), line)
		} else {
			parseLogicalNotExpression()
		}
	}

	private fun parseLogicalNotExpression(): Expression {
		return if (tokens[0] is Not) {
			val line = tokens[0].line!!
			tokens.removeFirst()
			val notExpr = parseLogicalNotExpression()
			UnaryOp(UnaryOps.Not, notExpr, line)
		} else {
			parseIncDec()
		}
	}

	private fun parseIncDec(): Expression {
		return when (tokens[0]) {
			is Inc -> {
				val line = tokens[0].line!!
				tokens.removeFirst()
				val incExpr =
					(expectToken(0, listOf(Ident()), "Expected identifier after increment op.", 0) as Ident).ident
				Modify(incExpr, false, IntegerNode(1, line), line)
			}

			is Dec -> {
				val line = tokens[0].line!!
				tokens.removeFirst()
				val decExpr =
					(expectToken(0, listOf(Ident()), "Expected identifier after decrement op.", 0) as Ident).ident
				Modify(decExpr, false, IntegerNode(-1, line), line)
			}

			is Ident -> {
				when (tokens[1]) {
					is Inc -> {
						val line = tokens[0].line!!
						val incExpr = (tokens[0] as Ident).ident
						tokens.removeFirst()
						tokens.removeFirst()
						Modify(incExpr, true, IntegerNode(1, line), line)
					}

					is Dec -> {
						val line = tokens[0].line!!
						val incExpr = (tokens[0] as Ident).ident
						tokens.removeFirst()
						tokens.removeFirst()
						Modify(incExpr, true, IntegerNode(-1, line), line)
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
		val line = tokens[0].line!!
		if (tokens[0] is Ident) {
			val identifier = (tokens[0] as Ident).ident
			tokens.removeFirst()
			return when {
				tokens[0] is LBracket -> {
					val line = tokens[0].line!!
					val idx = parseExpression()
					expectToken(0, listOf(RBracket()), "Expected closing brace on list access.", 0)
					ListAccess(identifier, idx, line)
				}

				tokens[0] is Dot -> {
					val line = tokens[0].line!!
					val rhs = parseExpression()
					expectNode(
						rhs,
						listOf(VariableAccess(line = line), ListAccess(line = line), Call(line = line)),
						"Invalid ScopeOf expression, expected variable, list, or function call.",
						0
					)
					ScopeOf(identifier, rhs, line)
				}

				tokens[0] is LParen -> {
					val line = tokens[0].line!!
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

							else -> {
								rejectToken(
									0,
									listOf(EndOfFile(), Semicolon()),
									"Invalid function call. Parameter list is not terminated.",
									0
								)
								params.add(parseExpression())
							}
						}
					}
					// Return the Call expression with identifier and parameters
					Call(identifier, params, line)
				}

				else -> {
					VariableAccess(identifier, line)
				}
			}
		}

		return parsePrimary()
	}

	private fun parsePrimary(): Expression {
		val tok =
			expectToken(0, listOf(Num(), StringTok(), CharTok(), Ident(), LParen()), "Error: Unexpected token.", 0)
		val line = tokens[0].line!!
		when (tok) {
			is Num -> {
				val num = tok
				return if (num.is_int) {
					IntegerNode(num.int, line)
				} else {
					Floating(num.dbl, line)
				}
			}

			is StringTok -> {
				return StringLit(tok.str, line)
			}

			is CharTok -> {
				return CharNode(tok.c, line)
			}

			is Ident -> {
				expectIdent(
					tok.ident,
					listOf("true", "false", "enumtype"),
					"Error, bad token provided.",
					0,
					line
				)
				// Todo: Implement types from enums.
				when (tok.ident) {
					"true" -> {
						return Bool(true, line)
					}

					"false" -> {
						return Bool(false, line)
					}
				}
			}

			is LParen -> {
				tokens.removeFirst()
				val parenthetical = parseExpression()
				expectToken(0, listOf(RParen()), "Error, unterminated parenthetical.", 0)
				return parenthetical
			}

			else -> {
				throw ExpressionParserException("Failure in expectToken")
			}
		}
		throw ExpressionParserException("Failure in expectToken")
	}

	/*

		STATEMENTS

	 */

	private fun parseStatement(): Statement {
		val statement = if (tokens[0] is Ident) {
			val line = tokens[0].line!!
			when ((tokens[0] as Ident).ident) {
				"if" -> {
					tokens.removeFirst()
					return parseIf()
				}

				"for" -> {
					tokens.removeFirst()
					return parseForLoop()
				}

				"foreach" -> {
					tokens.removeFirst()
					return parseForEachLoop()
				}

				"while" -> {
					tokens.removeFirst()
					return parseWhile()
				}

				"do" -> {
					tokens.removeFirst()
					return parseDo()
				}

				"continue" -> {
					tokens.removeFirst()
					Continue(line)
				}

				"break" -> {
					tokens.removeFirst()
					Break(line)
				}

				"switch" -> {
					tokens.removeFirst()
					return parseSwitch()
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
					return parseTry()
				}

				"throw" -> {
					tokens.removeFirst()
					parseThrow()
				}

				else -> {
					if (isAssignment(tokens[1])) {
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

		expectToken(
			0,
			listOf(Semicolon()),
			"Error, all statements must end with semicolons.",
			1
		)
		return statement
	}

	private fun parseIf(): Statement {
		var line = tokens[0].line!!
		expectToken(
			0,
			listOf(LParen()),
			"Error, each if statement must contain a conditional enclosed in parentheses.",
			1
		)
		val conditional: Expression = parseExpression()
		expectToken(
			0,
			listOf(RParen()),
			"Error, each if statement must contain a conditional enclosed in parentheses.",
			1
		)
		tokens.removeFirst()
		val consequent = parseBlock()
		var alternate: Statement? = null
		if ((tokens[0] is Ident && (tokens[0] as Ident).ident == "else")) {
			tokens.removeFirst()
			alternate = parseBlock()
		}
		return If(conditional, consequent, alternate, line)
	}

	private fun parseBlock(): Statement {
		var line = tokens[0].line!!
		expectToken(
			0,
			listOf(LBrace()),
			"Error, block expected.",
			1
		)
		tokens.removeFirst()
		val block = mutableListOf<Declaration>()
		while (tokens[0] !is RBrace) {
			block.add(parseDeclaration())
			rejectToken(0, listOf(EndOfFile()), "Error, unterminated block.", 1)
		}
		expectToken(
			0,
			listOf(RBrace()),
			"Error, block expected.",
			1
		)
		tokens.removeFirst()
		return Block(block, line)
	}


	private fun parseForLoop(): Statement {
		val line = tokens[0].line!!
		var initializer: Declaration? = null
		var conditional: Expression? = null
		var iteration: Statement? = null
		expectToken(
			0,
			listOf(LParen()),
			"Error, for loops must be followed by control structure.",
			1
		)
		if (tokens[0] !is Semicolon) {
			initializer = parseDeclaration()
			expectToken(
				0,
				listOf(Semicolon(), Comma(), RParen()),
				"Error, unexpected token.",
				1
			)
			if (tokens[0] is Semicolon || tokens[0] is Comma) {
				tokens.removeFirst()
			} else if (tokens[0] is RParen) {
				tokens.removeFirst()
				val block = parseBlock()
				return For(initializer, null, null, block, line)
			}
		}
		if (tokens[0] !is Semicolon) {
			conditional = parseExpression()
			expectToken(
				0,
				listOf(Semicolon(), Comma(), RParen()),
				"Error, unexpected token.",
				1
			)
			if (tokens[0] is Semicolon || tokens[0] is Comma) {
				tokens.removeFirst()
			} else if (tokens[0] is RParen) {
				tokens.removeFirst()
				val block = parseBlock()
				return For(initializer, conditional, null, block, line)
			}
		}
		if (tokens[0] !is Semicolon) {
			iteration = parseStatement()
			expectToken(
				0,
				listOf(Semicolon(), Comma(), RParen()),
				"Error, unexpected token.",
				1
			)
			if (tokens[0] is Semicolon || tokens[0] is Comma) {
				tokens.removeFirst()
			} else if (tokens[0] is RParen) {
				tokens.removeFirst()
				val block = parseBlock()
				return For(initializer, conditional, iteration, block, line)
			}
		}
		return For(initializer, conditional, iteration, parseBlock(), line)
	}

	private fun parseForEachLoop(): Statement {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LParen()),
			"Error, foreach loops must be followed by control structure.",
			1
		)
		val itervar = expectIdentifierToken("iteration")
		expectToken(
			0,
			listOf(Colon()),
			"Error, foreach loops consist of an iteration variable, a colon ':', and a collection variable.",
			1
		)

		val collectionvar = expectIdentifierToken("collection")
		expectToken(
			0,
			listOf(RParen()),
			"Error, foreach loop control structures must be terminated.",
			1
		)

		return ForEach(itervar.ident, collectionvar.ident, parseBlock(), line)
	}

	private fun expectIdentifierToken(variableType: String): Ident {
		if (tokens[0] !is Ident) {
			throw StatementParserException("Line ${tokens[0].line} - Error, foreach loops consist of an iteration variable, a colon ':', and a collection variable. Got '${(tokens[0])}', expecting identifier for $variableType.")
		}
		val identifier = tokens[0] as Ident
		tokens.removeFirst()
		return identifier
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