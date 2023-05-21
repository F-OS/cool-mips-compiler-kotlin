import AST.*
import ParserExcptions.DeclarationParserException
import ParserExcptions.ExpressionParserException
import ParserExcptions.ParserException
import ParserExcptions.StatementParserException

class Parser(private val tokens: MutableList<Token>) {
	private var semicolon_exempt: Boolean = false

	private inline fun isAssignment(token: Token): Boolean {
		return token is Assign || token is AddAssign || token is SubAssign ||
				token is MulAssign || token is DivAssign || token is ModAssign ||
				token is PowAssign || token is AndAssign || token is OrAssign ||
				token is LShiftAssign || token is RShiftAssign || token is XorAssign
	}

	private fun throwError(parserStage: Int, errorMessage: String) {
		when (parserStage) {
			0 -> throw ExpressionParserException(errorMessage)
			1 -> throw StatementParserException(errorMessage)
			2 -> throw DeclarationParserException(errorMessage)
		}
	}

	private fun expectToken(
		index: Int,
		expectedToken: List<Token>,
		errorMessage: String,
		parserStage: Int
	): Token {
		if (expectedToken.none { it::class == tokens[index]::class }) {
			if (expectedToken.all { it is Semicolon } && semicolon_exempt) {
				semicolon_exempt = false;
				return Unimplemented('\u0000');
			}
			val line = tokens[index].line.toString()
			val tokenString = tokToString(tokens[index])
			val expectedTokensString = expectedToken.joinToString(" or ") { tokToString(it) }
			val error = "Line $line - $errorMessage Got $tokenString, expected $expectedTokensString."
			throwError(parserStage, error)
		}
		if (tokens[index] is Semicolon) {
			semicolon_exempt = false;
		}
		val result = tokens[index]
		tokens.removeAt(index)
		return result
	}

	private fun expectIdent(
		ident: String,
		expectedIdent: List<String>,
		errorMessage: String,
		parserStage: Int,
		line: Int
	) {
		if (expectedIdent.none { it == ident }) {
			val expectedIdentString = expectedIdent.joinToString(" or ") { it }
			val error = "Line $line - $errorMessage Got '$ident', expected $expectedIdentString."
			throwError(parserStage, error)
		}
	}

	private fun expectNode(
		receivedNode: ASTRoot,
		expectedNode: List<ASTRoot>,
		errorMessage: String,
		parserStage: Int
	): ASTRoot {
		if (expectedNode.none { it::class == receivedNode::class }) {
			val line = receivedNode.line.toString()
			val tokenString = nodeToString(receivedNode)
			val expectedTokensString = expectedNode.joinToString(" or ") { nodeToString(it) }
			val error = "Line $line - $errorMessage Got $tokenString, expected $expectedTokensString."
			throwError(parserStage, error)
		}
		return receivedNode
	}

	private fun rejectTokens(
		index: Int,
		rejectedToken: List<Token>,
		errorMessage: String,
		parserStage: Int
	) {
		if (rejectedToken.any { it::class == tokens[index]::class }) {
			val line = tokens[index].line.toString()
			val tokenString = tokToString(tokens[index])
			val error = "Line $line - $errorMessage Unexpected $tokenString."
			throwError(parserStage, error)
		}
	}


	private fun expectIdentifierToken(variableType: String, statementType: String): String {
		if (tokens[0] !is Ident) {
			throw ParserException("Line ${tokens[0].line} - Error, missing an identifier in a $statementType. Got '${(tokens[0])}', expecting identifier for $variableType.")
		}
		val identifier = tokens[0] as Ident
		tokens.removeFirst()
		return identifier.ident
	}

	/*

			EXPRESSIONS

	 */
	/**
	 * The entry point of the recursive descent parser.
	 * Starts parsing of the expression at the root level.
	 *
	 * @return An object of type Expression representing the parsed expression.
	 * @grammar Expression -> Lambda
	 */
	private fun parseExpression(): Expression {
		return parseLambda()
	}

	/**
	 * Parses a lambda expression.
	 * Lambda expressions should start with the keyword "lambda".
	 * If the first token is not "lambda", it continues parsing.
	 * @return An object of type Expression representing the parsed lambda expression.
	 * @grammar Lambda -> LogicalOr | "lambda" LambdaBody
	 */
	private fun parseLambda(): Expression {
		return if (tokens[0] is Ident && (tokens[0] as Ident).ident == "lambda") {
			tokens.removeFirst()
			parseLambdaBody()
		} else {
			parseLogicalOrExpression()
		}
	}

	/**
	 * Parses a lambda expression
	 * A lambda expression consists of parameters and a block of code.
	 * @return An object of type Lambda containing parameters and block of code.
	 * @grammar LambdaBody -> Parameters Block
	 */
	private fun parseLambdaBody(): Expression {
		val line = tokens[0].line!!
		val params = parseParams()
		val block = parseBlock()
		return Lambda(params, block, line)
	}

	/**
	 * Parses the parameters of a lambda expression.
	 * A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses.
	 * Each parameter is a typed identifier.
	 * @return A list of strings representing the parsed parameters.
	 * @grammar Parameters -> "(" ParameterList ")"
	 * @grammar ParameterList -> TypedIdentifier
	 * @grammar                | ParameterList "," TypedIdentifier
	 */
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

	/**
	 * Parses a logical OR expression.
	 * @return An expression representing the parsed logical OR expression.
	 * @grammar LogicalOrExpression -> LogicalAndExpression
	 *          | LogicalOrExpression "||" LogicalAndExpression
	 */
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

	/**
	 * Parses a logical AND expression.
	 * @return An expression representing the parsed logical AND expression.
	 * @grammar LogicalAndExpression -> BitwiseAnd
	 *          | LogicalAndExpression "&&" BitwiseAnd
	 */
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

	/**
	 * Parses a bitwise AND expression.
	 * @return An expression representing the parsed bitwise AND expression.
	 * @grammar BitwiseAnd -> BitwiseXor
	 *          | BitwiseAnd "&" BitwiseXor
	 */
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

	/**
	 * Parses a bitwise XOR expression.
	 * @return An expression representing the parsed bitwise XOR expression.
	 * @grammar BitwiseXor -> BitwiseOr
	 *          | BitwiseXor "^" BitwiseOr
	 */
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

	/**
	 * Parses a bitwise OR expression.
	 * @return An expression representing the parsed bitwise OR expression.
	 * @grammar BitwiseOr -> Equality
	 *          | BitwiseOr "|" Equality
	 */
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

	/**
	 * Parses an equality expression.
	 * @return An expression representing the parsed equality expression.
	 * @grammar Equality -> RelationalExpression
	 *          | Equality "==" RelationalExpression
	 *          | Equality "!=" RelationalExpression
	 */
	private fun parseEquality(): Expression {
		var lhs = parseRelationalExpression()
		while (true) {
			val line = tokens[0].line!!
			lhs = when (tokens[0]) {
				is EqualTo -> {
					tokens.removeFirst()
					val rhs = parseRelationalExpression()
					BinaryOp(lhs, BinaryOps.EqualTo, rhs, line)
				}

				is NotEqualTo -> {
					tokens.removeFirst()
					val rhs = parseRelationalExpression()
					BinaryOp(lhs, BinaryOps.NotEqualTo, rhs, line)
				}

				else -> return lhs
			}
		}
	}


	/**
	 * Parses a relational expression.
	 * @return An expression representing the parsed relational expression.
	 * @grammar RelationalExpression -> BitwiseShift
	 * 			| RelationalExpression ">" BitwiseShift
	 *			| RelationalExpression ">=" BitwiseShift
	 * 			| RelationalExpression "<=" BitwiseShift
	 * 			| RelationalExpression "<" BitwiseShift
	 */
	private fun parseRelationalExpression(): Expression {
		var lhs = parseBitwiseShift()
		while (true) {
			val line = tokens[0].line!!
			lhs = when (tokens[0]) {
				is GreaterThan -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.GreaterThan, rhs, line)
				}

				is GreaterEqual -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.GreaterEqual, rhs, line)
				}

				is LessEqual -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.LessEqual, rhs, line)
				}

				is LessThan -> {
					tokens.removeFirst()
					val rhs = parseBitwiseShift()
					BinaryOp(lhs, BinaryOps.LessThan, rhs, line)
				}

				else -> return lhs
			}
		}
	}

	/**
	 * Parses a bitwise shift expression.
	 * @return An expression representing the parsed bitwise shift expression.
	 * @grammar BitwiseShift -> Term
	 *           | BitwiseShift "<<" Term
	 *           | BitwiseShift ">>" Term
	 */
	private fun parseBitwiseShift(): Expression {
		var lhs = parseTerm()
		while (tokens[0] is Bitwise_LShift || tokens[0] is Bitwise_RShift) {
			val line = tokens[0].line!!
			lhs = when (tokens[0]) {
				is Bitwise_LShift -> {
					tokens.removeFirst()
					val rhs = parseTerm()
					BinaryOp(lhs, BinaryOps.Bitwise_LShift, rhs, line)
				}

				is Bitwise_RShift -> {
					tokens.removeFirst()
					val rhs = parseTerm()
					BinaryOp(lhs, BinaryOps.Bitwise_RShift, rhs, line)
				}

				else -> return lhs
			}
		}
		return lhs
	}

	/**
	 * Parses a term expression.
	 * @return An expression representing the parsed term expression.
	 * @grammar Term -> Factor
	 *         | Term "+" Factor
	 *         | Term "-" Factor
	 */
	private fun parseTerm(): Expression {
		var lhs = parseFactor()
		while (true) {
			val line = tokens[0].line!!
			lhs = when (tokens[0]) {
				is Add -> {
					tokens.removeFirst()
					val rhs = parseFactor()
					BinaryOp(lhs, BinaryOps.Add, rhs, line)
				}

				is Sub -> {
					tokens.removeFirst()
					val rhs = parseFactor()
					BinaryOp(lhs, BinaryOps.Sub, rhs, line)
				}

				else -> return lhs
			}
		}
	}

	/**
	 * Parses a factor expression.
	 * @return An expression representing the parsed factor expression.
	 * @grammar Factor -> TightlyBindingFactor
	 *         | Factor "*" TightlyBindingFactor
	 *         | Factor "/" TightlyBindingFactor
	 *         | Factor "%" TightlyBindingFactor
	 */
	private fun parseFactor(): Expression {
		var lhs = parseTightlyBindingFactor()
		while (true) {
			val line = tokens[0].line!!
			lhs = when (tokens[0]) {
				is Mul -> {
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Mul, rhs, line)
				}

				is Div -> {
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Div, rhs, line)
				}

				is Mod -> {
					tokens.removeFirst()
					val rhs = parseTightlyBindingFactor()
					BinaryOp(lhs, BinaryOps.Mod, rhs, line)
				}

				else -> return lhs
			}
		}
	}

	/**
	 * Parses a tightly binding factor expression.
	 * @return An expression representing the parsed tightly binding factor expression.
	 * @grammar TightlyBindingFactor -> Unary
	 *              | TightlyBindingFactor "**" Unary
	 */
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

	/**
	 * Parses a unary expression.
	 * @return An expression representing the parsed unary expression.
	 * @grammar Unary -> "~" Unary
	 *         | "-" Unary
	 *         | LogicalNotExpression
	 */
	private fun parseUnary(): Expression {
		val line = tokens[0].line!!
		return if (tokens[0] is Bitwise_Not) {
			tokens.removeFirst()
			UnaryOp(UnaryOps.BNot, parseUnary(), line)
		} else if (tokens[0] is Sub) {
			tokens.removeFirst()
			UnaryOp(UnaryOps.Invert, parseUnary(), line)
		} else {
			parseLogicalNotExpression()
		}
	}

	/**
	 * Parses a logical not expression.
	 * @return An expression representing the parsed logical not expression.
	 * @grammar LogicalNotExpression -> "!" LogicalNotExpression
	 *             | IncDec
	 */
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

	/**
	 * Parses an increment or decrement expression.
	 * @return An expression representing the parsed increment or decrement expression.
	 * @grammar IncDec -> Inc Identifier
	 *          | Dec Identifier
	 *          | Identifier Inc
	 *          | Identifier Dec
	 *          | Call
	 */
	/**
	 * Parses an increment or decrement expression.
	 * @return An expression representing the parsed increment or decrement expression.
	 * @grammar IncDec -> "++" Call
	 *          | "--" Call
	 *          | Call "++"
	 *          | Call "--"
	 *          | Call
	 */
	private fun parseIncDec(): Expression {
		val line = tokens[0].line!!
		return when (tokens[0]) {
			is Inc -> {
				tokens.removeFirst()
				val result = parseCall()
				Modify(result, false, IntegerNode(1, line), line)
			}

			is Dec -> {
				tokens.removeFirst()
				val result = parseCall()
				Modify(result, false, IntegerNode(-1, line), line)
			}

			else -> {
				val result = parseCall()
				when (tokens[0]) {
					is Inc -> {
						tokens.removeFirst()
						Modify(result, true, IntegerNode(1, line), line)
					}

					is Dec -> {
						tokens.removeFirst()
						Modify(result, true, IntegerNode(-1, line), line)
					}

					else -> {
						result
					}
				}
			}
		}
	}

	/**
	 * Parses a function call, array access, variable access, or field access.
	 * @return An expression representing the parsed function call.
	 * @grammar CallExpression -> PrimaryExpression
	 *             | Ident "[" Expression "]"
	 *             | Ident "." Expression
	 *             | Ident "(" Parameters ")"
	 * @grammar Parameters -> "(" ParameterList ")"
	 * @grammar ParameterList -> Identifier
	 * 		                   | ParameterList "," Identifier
	 */
	private fun parseCall(): Expression {
		val line = tokens[0].line!!
		if (tokens[0] is Ident) {
			val identifier = (tokens[0] as Ident).ident
			tokens.removeFirst()
			return when {
				tokens[0] is LBracket -> {
					val idx = parseExpression()
					expectToken(0, listOf(RBracket()), "Expected closing brace on list access.", 0)
					ListAccess(identifier, idx, line)
				}

				tokens[0] is Dot -> {
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
								rejectTokens(
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

	/**
	 * Parses a primary expression.
	 * @return An expression representing the parsed primary expression.
	 * @grammar PrimaryExpression -> Num
	 *             | StringTok
	 *             | CharTok
	 *             | Ident
	 *             | "(" Expression ")"
	 */
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

	/**
	 * Parses a statement.
	 * @return The parsed statement.
	 * @grammar Statement -> IfStatement
	 *             | ForLoop
	 *             | ForEachLoop
	 *             | WhileLoop
	 *             | DoWhileLoop
	 *             | Continue ";"
	 *             | Break ";"
	 *             | SwitchStatement
	 *             | ReturnStatement ";"
	 *             | GotoStatement ";"
	 *             | TryStatement
	 *             | ThrowStatement ";"
	 *             | LabelStatement
	 *             | AssignmentStatement ";"
	 *             | ExpressionStatement ";"
	 *             | Block
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
					if (tokens[1] is Colon) {
						return parseLabel()
					}
					val lhs = parseExpression()
					if (isAssignment(tokens[0])) {
						parseAssignment(lhs)
					} else {
						ExprStatement(lhs, tokens[0].line!!)
					}
				}
			}
		} else if (tokens[0] is LBrace) {
			return parseBlock()
		} else {
			ExprStatement(parseExpression(), tokens[0].line!!)
		}

		expectToken(
			0,
			listOf(Semicolon()),
			"Error, all statements must end with semicolons.",
			1
		)
		return statement
	}

	/**
	 * Parses an if statement.
	 * @return The parsed if statement.
	 * @grammar IfStatement -> "if" "(" Expression ")" Block [ "else" Block ]
	 */
	private fun parseIf(): Statement {
		val line = tokens[0].line!!
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
		val consequent = parseBlock()
		var alternate: Statement? = null
		if ((tokens[0] is Ident && (tokens[0] as Ident).ident == "else")) {
			tokens.removeFirst()
			alternate = parseBlock()
		}
		return If(conditional, consequent, alternate, line)
	}

	/**
	 * Parses a block statement.
	 * @return The parsed block statement.
	 * @grammar Block -> "{" (Declaration)* "}"
	 */
	private fun parseBlock(): Statement {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LBrace()),
			"Error, block expected.",
			1
		)
		val block = mutableListOf<Declaration>()
		while (tokens[0] !is RBrace) {
			block.add(parseDeclaration())
			rejectTokens(0, listOf(EndOfFile()), "Error, unterminated block.", 1)
		}
		expectToken(
			0,
			listOf(RBrace()),
			"Error, block expected.",
			1
		)
		return Block(block, line)
	}

	/**
	 * Parses a for loop statement.
	 * @return The parsed for loop statement.
	 * @grammar ForLoop -> "for" "(" [ Initializer ] [ Conditional ] [ Iteration ] ")" Block
	 * @grammar Initializer -> Declaration | ";"
	 * @grammar Conditional -> ExprStatement | ";"
	 * @grammar Iteration -> Declaration | ";"
	 */
	private fun parseForLoop(): Statement {
		val line = tokens[0].line!!
		var initializer: Declaration? = null
		var conditional: Declaration? = null
		var iteration: Declaration? = null

		expectToken(0, listOf(LParen()), "Error, opening parenthesis expected after 'for'.", 1)

		if (tokens[0] !is RParen) {
			if (tokens[0] is Semicolon) {
				tokens.removeFirst()
			} else {
				semicolon_exempt = true
				initializer = parseDeclaration()
			}
		}
		if (tokens[0] !is RParen) {
			if (tokens[0] is Semicolon) {
				tokens.removeFirst()
			} else {
				semicolon_exempt = true
				conditional = parseDeclaration()
				expectNode(conditional, listOf(ExprStatement(line = line)), "Invalid conditional", 0)
			}
		}
		if (tokens[0] !is RParen) {
			if (tokens[0] is Semicolon) {
				tokens.removeFirst()
			} else {
				semicolon_exempt = true
				iteration = parseDeclaration()
			}
		}
		expectToken(0, listOf(RParen()), "Error, closing parenthesis expected after 'for'.", 1)
		val block = parseBlock()
		return For(initializer, conditional, iteration, block, line)
	}

	/**
	 * Parses a foreach loop statement.
	 * @return The parsed foreach loop statement.
	 * @grammar ForEachLoop -> "for" "(" Identifier ":" Identifier  ")" Block
	 */
	private fun parseForEachLoop(): Statement {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LParen()),
			"Error, foreach loops must be followed by control structure.",
			1
		)
		val itervar = expectIdentifierToken("an iteration variable", "foreach")
		expectToken(
			0,
			listOf(Colon()),
			"Error, foreach loops consist of an iteration variable, a colon ':', and a collection variable.",
			1
		)

		val collectionvar = expectIdentifierToken("a collection variable", "foreach")
		expectToken(
			0,
			listOf(RParen()),
			"Error, foreach loop control structures must be terminated.",
			1
		)

		return ForEach(itervar, collectionvar, parseBlock(), line)
	}

	/**
	 * Parses a while loop statement.
	 * @return The parsed while loop statement.
	 * @grammar WhileLoop -> "while" "(" Expression ")" Block
	 */
	private fun parseWhile(): Statement {
		val line = tokens[0].line!!
		expectToken(0, listOf(LParen()), "Error, while loops must be followed by a conditional in parenthesis.", 1)

		val conditional: Expression = parseExpression()

		expectToken(0, listOf(RParen()), "Error, unclosed parenthesis in while loop conditional.", 1)

		return While(conditional, parseBlock(), line)
	}

	/**
	 * Parses a do-while loop statement.
	 * @return The parsed do-while loop statement.
	 * @grammar WhileLoop -> "do" Block "while" (" Expression ")" ";"
	 */
	private fun parseDo(): Statement {
		val line = tokens[0].line!!

		val body = parseBlock()

		expectIdent(
			(tokens[0] as Ident).ident,
			listOf("while"),
			"Error, do loops must be followed by a 'while' block and control structure.",
			1,
			line
		)

		expectToken(0, listOf(LParen()), "Error, while blocks must have their condition in parenthesis.", 1)

		val conditional: Expression = parseExpression()

		expectToken(0, listOf(RParen()), "Error, unclosed parenthesis in while loop conditional.", 1)

		expectToken(0, listOf(Semicolon()), "Error, do-while loops end with a semicolon after the while statement.", 1)

		return DoWhile(conditional, body, line)
	}

	/**
	 * Parses a switch statement
	 * @return The parsed switch statement.
	 * @grammar SwitchStatement -> "switch" "(" Expression ")" CasesBlock
	 * @grammar CasesBlock -> "{" (CaseBlock)* "}"
	 * @grammar CaseBlock
	 */
	private fun parseSwitch(): Statement {
		val line = tokens[0].line!!
		expectToken(0, listOf(LParen()), "Error, switch statements must be followed by the expression to switch on.", 1)

		val conditional: Expression = parseExpression()

		expectToken(0, listOf(RParen()), "Error, unclosed expression in switch block.", 1)

		expectToken(0, listOf(LBrace()), "Error, switch statements must be followed by a switch block.", 1)

		val cases = mutableListOf<Pair<Expression, Statement>>()
		while (true) {
			expectIdent(
				(tokens[0] as Ident).ident,
				listOf("case", "default"),
				"Error, case blocks consist of the keyword 'case' or 'default' followed by an expression, a colon, and a code block.",
				1,
				tokens[0].line!!
			)
			var tok = tokens[0]
			tokens.removeFirst()
			val exp: Expression
			if (tok is Ident && (tok as Ident).ident == "default") {
				exp = Bool(true, line)
			} else {
				exp = parsePrimary()
			}
			expectToken(0, listOf(Colon()), "Error, case expression must be followed by a colon.", 1)

			val block: Statement = parseBlock()
			cases.add(Pair(exp, block))
			if (tokens[0] is RBrace) {
				break
			}
			rejectTokens(0, listOf(EndOfFile()), "Error, unterminated switch block. Did you forget a closing brace?", 1)
		}
		expectToken(0, listOf(RBrace()), "Error, switch blocks end with right braces.", 1)

		return Switch(conditional, cases, line)
	}

	/**
	 * Parses a label
	 * @return The parsed label.
	 * @grammar LabelStatement -> Identifier ":"
	 */
	private fun parseLabel(): Statement {
		val line = tokens[0].line!!
		val label =
			(expectToken(0, listOf(Ident()), "Error, invalid label. Expecting an identifier.", 1) as Ident).ident
		expectToken(0, listOf(Colon()), "Error, invalid label. Expecting a colon to end the label.", 1)
		return Label(label, line)
	}

	/**
	 * Parses an assignment
	 * @return The parsed assignment.
	 * @grammar AssignmentStatement -> Expression ("+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "&=" | "|=" | "^=" | "<<=" | ">>=") Expression
	 */
	private fun parseAssignment(lhs: Expression): Statement {
		val line = tokens[0].line!!
		val tok = tokens[0];
		tokens.removeFirst()
		if (isAssignment(tok) && tok !is Assign) {
			when (tok) {
				is AddAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Add, parseExpression(), line),
					line
				)

				is SubAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Sub, parseExpression(), line),
					line
				)

				is MulAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Mul, parseExpression(), line),
					line
				)

				is DivAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Div, parseExpression(), line),
					line
				)

				is ModAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Mod, parseExpression(), line),
					line
				)

				is PowAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Pow, parseExpression(), line),
					line
				)

				is AndAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_And, parseExpression(), line),
					line
				)

				is OrAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_Or, parseExpression(), line),
					line
				)

				is XorAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_Xor, parseExpression(), line),
					line
				)

				is LShiftAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_LShift, parseExpression(), line),
					line
				)

				is RShiftAssign -> return Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_RShift, parseExpression(), line),
					line
				)

				else -> {
					throw StatementParserException("isAssignment is malformed")
				}
			}
		}
		return Assignment(lhs, parseExpression(), line)
	}

	/**
	 * Parses a return statement
	 * @return The parsed return statement.
	 * @grammar ReturnStatement -> "return" [ ExpressionStatement ];
	 */
	private fun parseReturn(): Statement {
		val line = tokens[0].line!!
		return if (tokens[0] is Semicolon) {
			Return(null, line)
		} else {
			Return(parseExpression(), line)
		}
	}
	
	/**
	 * Parses a return statement
	 * @return The parsed return statement.
	 * @grammar ReturnStatement -> "return" [ ExpressionStatement ];
	 */
	private fun parseGoto(): Statement {
		val line = tokens[0].line!!
		val label = (expectToken(
			0,
			listOf(Ident()),
			"Error, invalid label for goto. Expecting an identifier.",
			1
		) as Ident).ident
		return Goto(label, line)
	}

	private fun parseTry(): Statement {
		val block = parseBlock()
		expectIdent(
			(tokens[0] as Ident).ident,
			listOf("catch"),
			"Error, try blocks consist of a code block followed by a 'catch' statement and a parenthetical.",
			1,
			tokens[0].line!!
		)
		tokens.removeFirst()
		expectToken(
			0,
			listOf(LParen()),
			"Error, catch statements must be followed by a parenthetical.",
			1
		)
		val line = tokens[0].line!!
		val catches = expectIdentifierToken("an exception variable", "catch block")
		expectToken(
			0,
			listOf(Colon()),
			"Error, catch variable must be typed.",
			1
		)
		val catchesAs = expectIdentifierToken("a type", "catch block")
		expectToken(
			0,
			listOf(RParen()),
			"Error, catch statements must be terminated.",
			1
		)
		val catch = parseBlock()
		return Try(block, catches, catchesAs, catch, line)
	}

	private fun parseThrow(): Statement {
		val throws = expectIdentifierToken("an exception", "throw expression")
		expectToken(
			0,
			listOf(LParen()),
			"Error, throw expression must have arguments in parentheses.",
			1
		)
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
		return Throw(throws, params, tokens[0].line!!)
	}
	/*
		DECLARATIONS
	 */

	fun parseDeclaration(): Declaration {
		return parseStatement()
	}

}