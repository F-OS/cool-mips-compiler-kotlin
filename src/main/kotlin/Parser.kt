import AST.*
import ParserExceptions.DeclarationParserException
import ParserExceptions.ExpressionParserException
import ParserExceptions.ParserException
import ParserExceptions.StatementParserException


class Parser(private val tokens: MutableList<Token>) {
	private var semicolon_exempt: Boolean = false
	private var partialDeclaration: MutableList<Declaration> = mutableListOf()

	enum class parserStates {
		PARSER_EXPRESSION_STAGE,
		PARSER_STATEMENT_STAGE,
		PARSER_DECLARATION_STAGE
	}

	private var reserved: List<String> = listOf(
		"true",
		"false",
		"nil"
	)

	fun parseProgram(): List<Declaration> {
		val list = mutableListOf<Declaration>()
		while (tokens[0] !is EndOfFile) {
			list.add(parseDeclaration())
			partialDeclaration = list
		}
		return list
	}

	private fun isAssignment(token: Token): Boolean {
		return token is Assign || token is AddAssign || token is SubAssign ||
				token is MulAssign || token is DivAssign || token is ModAssign ||
				token is PowAssign || token is AndAssign || token is OrAssign ||
				token is LShiftAssign || token is RShiftAssign || token is XorAssign
	}

	fun synchronize(): List<Declaration> {
		val declsSoFar = partialDeclaration
		partialDeclaration = mutableListOf()
		while (tokens[0] !is EndOfFile) {
			when (tokens[0]) {
				is RBrace, is Semicolon -> {
					tokens.removeFirst()
					return declsSoFar
				}

				is Ident -> {
					val ident = (tokens[0] as Ident).ident
					val synchronizeTokens = listOf(
						"if",
						"while",
						"for",
						"foreach",
						"do",
						"class",
						"var",
						"fun",
						"enum",
						"switch",
						"else",
						"throw",
						"try",
						"catch"
					)
					if (synchronizeTokens.any { it == ident }) {
						return declsSoFar
					}
				}

				else -> {
					tokens.removeFirst()
				}
			}
		}
		return declsSoFar
	}

	private fun throwError(parserStage: parserStates? = null, errorMessage: String, line_or_not: Int? = null): Nothing {
		val line: Int = if (line_or_not == null) tokens[0].line!! else line_or_not
		if (parserStage == null) {
			throw ParserException("Line $line - $errorMessage")
		}
		when (parserStage) {
			parserStates.PARSER_EXPRESSION_STAGE -> throw ExpressionParserException("Line $line - " + errorMessage)
			parserStates.PARSER_STATEMENT_STAGE -> throw StatementParserException("Line $line - " + errorMessage)
			parserStates.PARSER_DECLARATION_STAGE -> throw DeclarationParserException("Line $line - " + errorMessage)
		}
	}

	private fun expectToken(
		index: Int,
		expectedToken: List<Token>,
		errorMessage: String,
		parserStage: parserStates
	): Token {
		if (expectedToken.none { it::class == tokens[index]::class }) {
			if (expectedToken.all { it is Semicolon } && semicolon_exempt) {
				semicolon_exempt = false
				return Unimplemented('\u0000')
			}
			val line = tokens[index].line
			val tokenString = tokToString(tokens[index])
			val expectedTokensString = expectedToken.joinToString(" or ") { tokToString(it) }
			val error = "Line $line - $errorMessage Got $tokenString, expected $expectedTokensString."
			throwError(parserStage, error, line)
		}
		if (tokens[index] is Semicolon) {
			semicolon_exempt = false
		}
		val result = tokens[index]
		tokens.removeAt(index)
		return result
	}

	private fun expectIdent(
		ident: String,
		expectedIdent: List<String>,
		errorMessage: String,
		parserStage: parserStates,
		line: Int
	) {
		if (expectedIdent.none { it == ident }) {
			val expectedIdentString = expectedIdent.joinToString(" or ") { it }
			val error = "$errorMessage Got '$ident', expected $expectedIdentString."
			throwError(parserStage, error, line)
		}
	}

	private fun expectNode(
		receivedNode: ASTRoot,
		expectedNode: List<ASTRoot>,
		errorMessage: String,
		parserStage: parserStates
	): ASTRoot {
		if (expectedNode.none { it::class == receivedNode::class }) {
			val line = receivedNode.line
			val tokenString = nodeToString(receivedNode)
			val expectedTokensString = expectedNode.joinToString(" or ") { nodeToString(it) }
			val error = "$errorMessage Got $tokenString, expected $expectedTokensString."
			throwError(parserStage, error, line)
		}
		return receivedNode
	}

	private fun rejectTokens(
		index: Int,
		rejectedToken: List<Token>,
		errorMessage: String,
		parserStage: parserStates
	) {
		if (rejectedToken.any { it::class == tokens[index]::class }) {
			val line = tokens[index].line
			val tokenString = tokToString(tokens[index])
			val error = "$errorMessage Unexpected $tokenString."
			throwError(parserStage, error, line)
		}
	}


	private fun expectIdentifierToken(variableType: String, statementType: String): String {
		if (tokens[0] !is Ident) {
			throwError(
				errorMessage = "Error, missing an identifier in a $statementType. Got '${(tokens[0])}', expecting identifier for $variableType.",
				line_or_not = tokens[0].line
			)
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
	 * @return An object of type [Expression] representing the parsed expression.
	 *
	 * ***Grammar:***
	 * * Expression ->
	 * * * Lambda
	 */
	private fun parseExpression(): Expression {
		return parseLambda()
	}

	/**
	 * Parses a [Lambda] expression.
	 * Lambda expressions should start with the keyword "lambda".
	 * If the first token is not "lambda", it continues parsing.
	 * @return An object of type [Expression] representing the parsed Lambda expression or a passthrough to Logical Or.
	 *
	 * ***Grammar:***
	 * * Lambda ->
	 * * * LogicalOr  |
	 * * * "lambda" LambdaBody
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
	 * @return An object of type Lambda containing parameters and a block of code.
	 *
	 * ***Grammar:***
	 * * LambdaBody ->
	 * * * Parameters Block
	 */
	private fun parseLambdaBody(): Expression {
		val line = tokens[0].line!!
		val params = parseParams()
		expectToken(0, listOf(Colon()), "Lambda needs type annotations.", parserStates.PARSER_EXPRESSION_STAGE)
		val type = expectIdentifierToken("a return type", "lambda statement")
		val block = parseBlock()
		return Lambda(params, block, type, line)
	}

	/**
	 * Parses the parameters of a lambda expression.
	 * A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses.
	 * Each parameter is a typed Identifier.
	 * @return A list of strings representing the parsed parameters.
	 *
	 * ***Grammar:***
	 * * Parameters -> "(" ParameterList ")"
	 * * ParameterList ->
	 * * * TypedIdentifier |
	 * * * ParameterList "," TypedIdentifier
	 */
	private fun parseParams(): List<Pair<String, String>> {
		expectToken(
			0,
			listOf(LParen()),
			"A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses.",
			parserStates.PARSER_EXPRESSION_STAGE
		)
		val params = mutableListOf<Pair<String, String>>()
		var expectComma = false
		while (true) {
			when (tokens[0]) {
				is Ident -> {
					if (expectComma) {
						println("WARNING: Expected a comma at identifier ${(tokens[0] as Ident).ident} in parameter definition on line ${tokens[0].line}")
					}
					expectComma = true
					val name = expectIdentifierToken("a parameter name", "lambda")

					expectToken(
						0,
						listOf(Colon()),
						"A parameter definition must contain an identifier followed by a colon and then a typename.",
						parserStates.PARSER_EXPRESSION_STAGE
					)

					val type = expectIdentifierToken("a parameter type", "lambda")

					params.add(Pair(name, type))
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
					listOf(Ident(), RParen(), Comma()),
					"Unexpected token in parameter list.",
					parserStates.PARSER_EXPRESSION_STAGE
				)
			}
		}
		return params
	}

	/**
	 * Parses a logical OR expression.
	 * @return A [BinaryOp] expression representing the parsed expression or a passthrough to Logical And.
	 *
	 * ***Grammar:***
	 * * LogicalOrExpression ->
	 * * * LogicalAndExpression |
	 * * * LogicalOrExpression "||" LogicalAndExpression
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
	 * @return A [BinaryOp] expression representing the parsed expression or a passthrough to Bitwise And.
	 *
	 * ***Grammar:***
	 * * LogicalAndExpression ->
	 * * * BitwiseAnd |
	 * * * LogicalAndExpression "||" BitwiseAnd
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
	 * @return A [BinaryOp] expression representing the parsed expression or a passthrough to Bitwise Xor.
	 *
	 * ***Grammar:***
	 * * BitwiseAnd ->
	 * * * BitwiseXor |
	 * * * BitwiseAnd "&" BitwiseXor
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
	 * @return A [BinaryOp] expression representing the parsed expression or a passthrough to Bitwise Or.
	 *
	 * ***Grammar:***
	 * * BitwiseXor ->
	 * * * BitwiseOr |
	 * * * BitwiseXor "^" BitwiseOr
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
	 * @return A [BinaryOp] expression representing the parsed expression or a passthrough to Equality.
	 *
	 * ***Grammar:***
	 * * BitwiseOr ->
	 * * * Equality |
	 * * * BitwiseOr "|" Equality
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
	 * @return A [BinaryOp] expression representing the parsed inequality/equality or a passthrough to Relational.
	 *
	 * ***Grammar:***
	 * * Equality ->
	 * * * RelationalExpression |
	 * * * Equality "==" RelationalExpression |
	 * * * Equality "!=" RelationalExpression
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
	 * @return A [BinaryOp] expression representing the parsed relational expression or a passthrough to Bitwise Shift.
	 *
	 * ***Grammar:***
	 * * RelationalExpression ->
	 * * * BitwiseShift |
	 * * * RelationalExpression ">" BitwiseShift |
	 * * * RelationalExpression ">=" BitwiseShift |
	 * * * RelationalExpression "<=" BitwiseShift |
	 * * * RelationalExpression "<" BitwiseShift
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
	 * @return A [BinaryOp] expression representing the parsed bitwise shift expression, or a passthrough to Term.
	 *
	 * ***Grammar:***
	 * * BitwiseShift ->
	 * * * Term |
	 * * * BitwiseShift "<<" Term |
	 * * * BitwiseShift ">>" Term
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
	 * @return A [BinaryOp] expression representing the parsed term expression or a passthrough to Factor.
	 *
	 * ***Grammar:***
	 * * Term ->
	 * * * Factor |
	 * * * Term "+" Factor |
	 * * * Term "-" Factor
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
	 * @return A [BinaryOp] expression representing the parsed factor expression or a passthrough to Pow.
	 *
	 * ***Grammar:***
	 * * Factor ->
	 * * * TightlyBindingFactor |
	 * * * Factor "*" TightlyBindingFactor |
	 * * * Factor "/" TightlyBindingFactor |
	 * * * Factor "%" TightlyBindingFactor
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
	 * @return A [BinaryOp] expression representing the parsed tightly binding factor expression or a passthrough to Unary.
	 *
	 * ***Grammar:***
	 * * TightlyBindingFactor ->
	 * * * Unary |
	 * * * TightlyBindingFactor "**" Unary
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
	 * @return A [UnaryOp] expression representing the parsed unary expression or a passthrough to Logical Not.
	 *
	 * ***Grammar:***
	 * * Unary ->
	 * * * "~" Unary |
	 * * * "-" Unary |
	 * * * LogicalNotExpression
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
	 * @return A [UnaryOp] expression representing the parsed logical not expression or a passthrough to Modify.
	 *
	 * ***Grammar:***
	 * * LogicalNotExpression ->
	 * * * "!" LogicalNotExpression |
	 * * * IncDec
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
	 * @return A [Modify] expression representing the parsed increment or decrement expression or a passthrough to Call.
	 *
	 * ***Grammar:***
	 * * IncDec ->
	 * * * "++" Call |
	 * * * "--" Call |
	 * * * Call "++" |
	 * * * Call "--" |
	 * * *  Call
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
	 * Parses a function call [Call], array access [ListAccess], variable access [VariableAccess], or field access [ScopeOf].
	 * @return An expression representing the parsed function call or a passthrough to Primary.
	 *
	 * ***Grammar:***
	 * * CallExpression ->
	 * * * PrimaryExpression |
	 * * *  Ident "[" Expression "]" |
	 * * *  Ident "." Expression |
	 * * *  Ident "(" Parameters ")"
	 * * Parameters -> "(" ParameterList ")"
	 * * ParameterList ->
	 * * * Identifier |
	 * * *  ParameterList "," Identifier
	 */
	private fun parseCall(): Expression {
		val line = tokens[0].line!!
		if (tokens[0] is Ident) {
			val identifier = (tokens[0] as Ident).ident
			if (reserved.any { it == identifier }) {
				return parsePrimary()
			}
			tokens.removeFirst()
			return when {
				tokens[0] is LBracket -> {
					val idx = parseExpression()
					expectToken(
						0,
						listOf(RBracket()),
						"Expected closing brace on list access.",
						parserStates.PARSER_EXPRESSION_STAGE
					)
					ListAccess(identifier, idx, line)
				}

				tokens[0] is Dot -> {
					val rhs = parseExpression()
					expectNode(
						rhs,
						listOf(VariableAccess(line = line), ListAccess(line = line), Call(line = line)),
						"Invalid ScopeOf expression, expected variable, list, or function call.",
						parserStates.PARSER_EXPRESSION_STAGE
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
									parserStates.PARSER_EXPRESSION_STAGE
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
	 *
	 * ***Grammar:***
	 * * PrimaryExpression ->
	 * * * [IntegerNode] |
	 * * * [Floating] |
	 * * * [Bool] |
	 * * * [StringTok] |
	 * * * [CharTok] |
	 * * * [reserved]Ident |
	 * * * "(" Expression ")"
	 */
	private fun parsePrimary(): Expression {
		val tok =
			expectToken(
				0,
				listOf(Num(), StringTok(), CharTok(), Ident(), LParen()),
				"Error: Unexpected token.",
				parserStates.PARSER_EXPRESSION_STAGE
			)
		val line = tokens[0].line!!
		when (tok) {
			is Num -> {
				return if (tok.is_int) {
					IntegerNode(tok.int, line)
				} else {
					Floating(tok.dbl, line)
				}
			}

			is StringTok -> {
				return StringLit(tok.str, line)
			}

			is CharTok -> {
				return CharNode(tok.c, line)
			}

			is Ident -> {
				when (tok.ident) {
					"true" -> {
						return Bool(true, line)
					}

					"false" -> {
						return Bool(false, line)
					}

					"nil" -> {
						TODO("This will be useful for objects at some point")
					}
				}
			}

			is LParen -> {
				val parenthetical = parseExpression()
				expectToken(
					0,
					listOf(RParen()),
					"Error, unterminated parenthetical.",
					parserStates.PARSER_EXPRESSION_STAGE
				)
				return parenthetical
			}

			else -> {
				throwError(parserStates.PARSER_EXPRESSION_STAGE, "Failure in expectToken")
			}
		}
		throwError(parserStates.PARSER_EXPRESSION_STAGE, "Failure in expectToken")
	}

	/*

		STATEMENTS

	 */

	/**
	 * Parses a statement.
	 * @return The parsed statement.
	 *
	 * ***Grammar:***
	 * * [Statement] ->
	 * * * [If]
	 * * * [For]
	 * * * [ForEach]
	 * * * [While]
	 * * * [DoWhile]
	 * * * [Continue] ";"
	 * * * [Break] ";"
	 * * * [Switch]
	 * * * [Return] ";"
	 * * * [Goto] ";"
	 * * * [Try]
	 * * * [Throw] ";"
	 * * * [Label]
	 * * * [Assignment] ";"
	 * * * [Expression] ";"
	 * * * [Block]
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
			parserStates.PARSER_STATEMENT_STAGE
		)
		return statement
	}

	/**
	 * Parses an if statement.
	 * @return The parsed [If] statement.
	 *
	 * ***Grammar:***
	 * * IfStatement ->
	 * * * "if" "(" Expression ")" Block
	 * * * "if" "(" Expression ")" Block "else" Block
	 */
	private fun parseIf(): Statement {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LParen()),
			"Error: Each if statement must contain a conditional enclosed in parentheses.",
			parserStates.PARSER_STATEMENT_STAGE
		)
		val conditional: Expression = parseExpression()
		expectToken(
			0,
			listOf(RParen()),
			"Error: Each if statement must contain a conditional enclosed in parentheses.",
			parserStates.PARSER_STATEMENT_STAGE
		)
		val consequent = parseBlock()
		var alternate: Statement? = null
		if (tokens[0] is Ident && (tokens[0] as Ident).ident == "else") {
			tokens.removeFirst()
			alternate = parseBlock()
		}
		return If(conditional, consequent, alternate, line)
	}

	/**
	 * Parses a block statement.
	 * @return The parsed [Block] statement.
	 *
	 * ***Grammar:***
	 * * Block ->
	 * * * "{" (Declaration)* "}"
	 */

	private fun parseBlock(): Block {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LBrace()),
			"Error: Block expected.",
			parserStates.PARSER_STATEMENT_STAGE
		)
		val block = mutableListOf<Declaration>()
		while (tokens[0] !is RBrace) {
			block.add(parseDeclaration())
			rejectTokens(
				0,
				listOf(EndOfFile()),
				"Error: Unterminated block.",
				parserStates.PARSER_STATEMENT_STAGE
			)
		}
		expectToken(
			0,
			listOf(RBrace()),
			"Error: Block expected.",
			parserStates.PARSER_STATEMENT_STAGE
		)
		return Block(block, line)
	}


	/**
	 * Parses a for loop statement.
	 * @return The parsed [For] loop statement.
	 *
	 * ***Grammar:***
	 * * ForLoop ->
	 * * * "for" "(" [ Initializer ] [ Conditional ] [ Iteration ] ")" Block
	 * * Initializer ->
	 * * * e | Declaration | ";"
	 * * Conditional ->
	 * * * e | ExprStatement | ";"
	 * * Iteration ->
	 * * * e | Declaration | ";"
	 */

	private fun parseForLoop(): Statement {
		val line = tokens[0].line!!
		var initializer: Declaration? = null
		var conditional: Declaration? = null
		var iteration: Declaration? = null

		expectToken(
			0,
			listOf(LParen()),
			"Error: Opening parenthesis expected after 'for'.",
			parserStates.PARSER_STATEMENT_STAGE
		)

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
				expectNode(
					conditional,
					listOf(ExprStatement(line = line)),
					"Invalid conditional",
					parserStates.PARSER_STATEMENT_STAGE
				)
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
		expectToken(
			0,
			listOf(RParen()),
			"Error: Closing parenthesis expected after 'for'.",
			parserStates.PARSER_STATEMENT_STAGE
		)
		val block = parseBlock()
		return For(initializer, conditional, iteration, block, line)
	}

	/**
	 * Parses a foreach loop statement.
	 * @return The parsed [ForEach] loop statement.
	 *
	 * ***Grammar:***
	 * * ForEachLoop ->
	 * * * "for" "(" Identifier ":" Identifier ")" Block
	 */
	private fun parseForEachLoop(): Statement {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LParen()),
			"Error: Foreach loops must be followed by control structure.",
			parserStates.PARSER_STATEMENT_STAGE
		)
		val itervar = expectIdentifierToken("an iteration variable", "foreach")
		expectToken(
			0,
			listOf(Colon()),
			"Error: Foreach loops consist of an iteration variable, a colon ':', and a collection variable.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val collectionvar = expectIdentifierToken("a collection variable", "foreach")
		expectToken(
			0,
			listOf(RParen()),
			"Error: Foreach loop control structures must be terminated.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		return ForEach(itervar, collectionvar, parseBlock(), line)
	}


	/**
	 * Parses a while loop statement.
	 * @return The parsed [While] loop statement.
	 *
	 * ***Grammar:***
	 * * WhileLoop ->
	 * * * "while" "(" Expression ")" Block
	 */
	private fun parseWhile(): Statement {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LParen()),
			"Error: While loops must be followed by a conditional in parentheses.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val conditional: Expression = parseExpression()

		expectToken(
			0,
			listOf(RParen()),
			"Error: Unclosed parenthesis in while loop conditional.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		return While(conditional, parseBlock(), line)
	}

	/**
	 * Parses a do-while loop statement.
	 * @return The parsed [DoWhile] loop statement.
	 *
	 * ***Grammar:***
	 * * WhileLoop ->
	 * * * "do" Block "while" (" Expression ")" ";"
	 */
	private fun parseDo(): Statement {
		val line = tokens[0].line!!

		val body = parseBlock()

		expectIdent(
			(tokens[0] as Ident).ident,
			listOf("while"),
			"Error: Do loops must be followed by a 'while' block and control structure.",
			parserStates.PARSER_STATEMENT_STAGE,
			line
		)

		expectToken(
			0,
			listOf(LParen()),
			"Error: While blocks must have their condition in parentheses.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val conditional: Expression = parseExpression()

		expectToken(
			0,
			listOf(RParen()),
			"Error: Unclosed parenthesis in while loop conditional.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		expectToken(
			0,
			listOf(Semicolon()),
			"Error: Do-while loops end with a semicolon after the while statement.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		return DoWhile(conditional, body, line)
	}

	/**
	 * Parses a switch statement
	 * @return The parsed [Switch] statement.
	 *
	 * ***Grammar:***
	 * * SwitchStatement ->
	 * * * "switch" "(" Expression ")" CasesBlock
	 * * CasesBlock ->
	 * * * "{" (CaseBlock)* "}"
	 * * CaseBlock ->
	 * * * "case" Primary ":" Block |
	 * * * "default" ":" Block
	 */
	private fun parseSwitch(): Statement {
		val line = tokens[0].line!!
		expectToken(
			0,
			listOf(LParen()),
			"Error: Switch statements must be followed by the expression to switch on.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val conditional: Expression = parseExpression()

		expectToken(
			0,
			listOf(RParen()),
			"Error: Unclosed expression in switch block.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		expectToken(
			0,
			listOf(LBrace()),
			"Error: Switch statements must be followed by a switch block.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val cases = mutableListOf<Pair<Expression, Statement>>()
		while (true) {
			expectIdent(
				(tokens[0] as Ident).ident,
				listOf("case", "default"),
				"Error: Case blocks consist of the keyword 'case' or 'default' followed by an expression, a colon, and a code block.",
				parserStates.PARSER_STATEMENT_STAGE,
				tokens[0].line!!
			)
			val tok = tokens.removeFirst()
			val exp: Expression = if (tok is Ident && tok.ident == "default") {
				Bool(true, line)
			} else {
				parsePrimary()
			}
			expectToken(
				0,
				listOf(Colon()),
				"Error: Case expression must be followed by a colon.",
				parserStates.PARSER_STATEMENT_STAGE
			)

			val block: Statement = parseBlock()
			cases.add(Pair(exp, block))
			if (tokens[0] is RBrace) {
				break
			}
			rejectTokens(
				0,
				listOf(EndOfFile()),
				"Error: Unterminated switch block. Did you forget a closing brace?",
				parserStates.PARSER_STATEMENT_STAGE
			)
		}
		expectToken(
			0,
			listOf(RBrace()),
			"Error: Switch blocks end with right braces.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		return Switch(conditional, cases, line)
	}

	/**
	 * Parses an assignment
	 * @return The parsed [Assignment].
	 *
	 * ***Grammar:***
	 * * AssignmentStatement ->
	 * * * Expression ("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "&=" | "|=" | "^=" | "<<=" | ">>=") Expression ";"
	 */
	private fun parseAssignment(lhs: Expression): Statement {
		val line = tokens[0].line!!
		val tok = tokens.removeFirst()
		if (isAssignment(tok) && tok !is Assign) {
			return when (tok) {
				is AddAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Add, parseExpression(), line),
					line
				)

				is SubAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Sub, parseExpression(), line),
					line
				)

				is MulAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Mul, parseExpression(), line),
					line
				)

				is DivAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Div, parseExpression(), line),
					line
				)

				is ModAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Mod, parseExpression(), line),
					line
				)

				is PowAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Pow, parseExpression(), line),
					line
				)

				is AndAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_And, parseExpression(), line),
					line
				)

				is OrAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_Or, parseExpression(), line),
					line
				)

				is XorAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_Xor, parseExpression(), line),
					line
				)

				is LShiftAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_LShift, parseExpression(), line),
					line
				)

				is RShiftAssign -> Assignment(
					lhs,
					BinaryOp(lhs, BinaryOps.Bitwise_RShift, parseExpression(), line),
					line
				)

				else -> throwError(parserStates.PARSER_EXPRESSION_STAGE, "isAssignment is malformed")
			}
		}
		return Assignment(lhs, parseExpression(), line)
	}

	/**
	 * Parses a label
	 * @return The parsed [Label].
	 *
	 * ***Grammar:***
	 * * LabelStatement ->
	 * * * Identifier ":"
	 */
	private fun parseLabel(): Statement {
		val line = tokens[0].line!!
		val label = expectIdentifierToken("a label", "label definition")
		expectToken(
			0,
			listOf(Colon()),
			"Error: Invalid label. Expecting a colon to end the label.",
			parserStates.PARSER_STATEMENT_STAGE
		)
		return Label(label, line)
	}

	/**
	 * Parses a return statement
	 * @return The parsed [Return] statement.
	 *
	 * ***Grammar:***
	 * * ReturnStatement ->
	 * * * "return" [ ExpressionStatement ] ";"
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
	 * Parses a goto statement
	 * @return The parsed [Goto] statement.
	 *
	 * ***Grammar:***
	 * * GotoStatement ->
	 * * * "goto" Identifier ";"
	 */
	private fun parseGoto(): Statement {
		val line = tokens[0].line!!
		val label = expectIdentifierToken("a label", "goto")
		return Goto(label, line)
	}

	/**
	 * Parses a try statement
	 * @return The parsed [Try] statement.
	 *
	 * ***Grammar:***
	 * * TryStatement ->
	 * * * "try" Block "catch" "(" Exception ")" Block
	 * * Exception ->
	 * * * ExceptionName ":" ExceptionType
	 * * ExceptionName ->
	 * * * Identifier
	 * * ExceptionType ->
	 * * * Identifier
	 */
	private fun parseTry(): Statement {
		val block = parseBlock()

		expectIdent(
			(tokens[0] as Ident).ident,
			listOf("catch"),
			"Error: try blocks consist of a code block followed by a 'catch' statement and a parenthetical.",
			parserStates.PARSER_STATEMENT_STAGE,
			tokens[0].line!!
		)

		tokens.removeFirst()

		expectToken(
			0,
			listOf(LParen()),
			"Error: catch statements must be followed by a parenthetical.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val line = tokens[0].line!!

		val catches = expectIdentifierToken("an exception variable", "catch block")

		expectToken(
			0,
			listOf(Colon()),
			"Error: catch variable must be typed.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val catchesAs = expectIdentifierToken("a type", "catch block")

		expectToken(
			0,
			listOf(RParen()),
			"Error: catch statements must be terminated.",
			parserStates.PARSER_STATEMENT_STAGE
		)

		val catchBlock = parseBlock()

		return Try(block, catches, catchesAs, catchBlock, line)
	}

	/**
	 * Parses a throw statement
	 * @return The parsed [Throw] statement.
	 *
	 * ***Grammar***
	 * * Throw ->
	 * * * "throw" ExceptionName Parameters ";"
	 * * ExceptionName ->
	 * * * Identifier
	 * * Parameters ->
	 * * * "(" ParameterList ")"
	 * * ParameterList ->
	 * * * Identifier |
	 * * * ParameterList "," Identifier
	 */
	private fun parseThrow(): Statement {
		val throws = expectIdentifierToken("an exception", "throw statement")

		expectToken(
			0,
			listOf(LParen()),
			"Error: a throw expression must have its arguments in parentheses.",
			parserStates.PARSER_STATEMENT_STAGE
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
					throwError(
						parserStates.PARSER_EXPRESSION_STAGE,
						"Error: Invalid exception. Parameter list is not terminated."
					)
				}

				else -> params.add(parseExpression())
			}
		}

		return Throw(throws, params, tokens[0].line!!)
	}

	/*
		DECLARATIONS
	 */

	/**
	 * Parses a declaration.
	 * @return The parsed declaration.
	 *
	 * ***Grammar:***
	 *
	 * * [Declaration] -> [SimpleVarDeclaration]
	 * * | [ArrayDeclaration]
	 * * | [EnumDeclaration]
	 * * | [ClassDeclaration]
	 * * | [FunctionDeclaration]
	 * * | [Statement]
	 */
	private fun parseDeclaration(): Declaration {
		return if (tokens[0] is Ident) {
			when ((tokens[0] as Ident).ident) {
				"var" -> {
					tokens.removeFirst()
					parseVarDeclaration()
				}

				"array" -> {
					tokens.removeFirst()
					parseArrayDeclaration()
				}

				"enum" -> {
					tokens.removeFirst()
					parseEnumDeclaration()
				}

				"class" -> {
					tokens.removeFirst()
					parseClassDeclaration()
				}

				"fun" -> {
					tokens.removeFirst()
					parseFunDeclaration()
				}

				"struct" -> {
					tokens.removeFirst()
					parseStructDeclaration()
				}

				else -> {
					parseStatement()
				}
			}
		} else {
			parseStatement()
		}
	}

	/**
	 * Parses a var.
	 * @return The parsed declaration.
	 *
	 * ***Grammar:***
	 * * [SimpleVarDeclaration] -> "var" Name ":" Type ["=" [Expression]] ";"
	 *
	 * * Name -> [Ident]ifier
	 *
	 * * Type -> [Ident]ifier
	 */
	private fun parseVarDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a variable name", "variable definition")

		expectToken(
			0,
			listOf(Colon()),
			"Error: A variable definition must contain an identifier followed by a colon and then a typename.",
			parserStates.PARSER_DECLARATION_STAGE
		)

		val type = expectIdentifierToken("a variable type", "variable definition")

		val varDecl = Pair(name, type)

		val nextToken = expectToken(
			0,
			listOf(Assign(), Semicolon()),
			"Error: A variable definition must be followed by either an assignment or a semicolon.",
			parserStates.PARSER_DECLARATION_STAGE
		)

		when (nextToken) {
			is Assign -> {
				val expr = parseExpression()
				expectToken(
					0,
					listOf(Semicolon()),
					"Error: An expression in a definition must be followed by a semicolon.",
					parserStates.PARSER_DECLARATION_STAGE
				)
				return SimpleVarDeclaration(varDecl, expr, line)
			}

			is Semicolon -> {
				return SimpleVarDeclaration(varDecl, null, line)
			}

			else -> throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Failed to parse a variable declaration.")
		}
	}


	private fun parseArrayDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a variable name", "array definition")

		expectToken(
			0,
			listOf(Colon()),
			"Error: An array definition must contain an identifier followed by a colon and then a typename.",
			parserStates.PARSER_DECLARATION_STAGE
		)

		val type = expectIdentifierToken("a variable type", "array definition")

		val varDecl = Pair(name, type)

		val nextToken = expectToken(
			0,
			listOf(LBracket(), Assign(), Semicolon()),
			"Error: An array definition must be followed by either an assignment, a length in braces, or a semicolon.",
			parserStates.PARSER_DECLARATION_STAGE
		)

		when (nextToken) {
			is Assign -> {
				val initializerList = parseArrayInitializers()
				expectToken(
					0,
					listOf(Semicolon()),
					"Error: An array initializer must be followed by a semicolon.",
					parserStates.PARSER_DECLARATION_STAGE
				)
				return ArrayDeclaration(varDecl, -1, initializerList, line)
			}

			is LBracket -> {
				val lenNum = expectToken(
					0,
					listOf(Num()),
					"Error: Array lengths must be whole numbers.",
					parserStates.PARSER_DECLARATION_STAGE
				) as Num

				if (!lenNum.is_int) {
					throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Array lengths must be whole numbers.")
				}

				val len = lenNum.int

				expectToken(
					0,
					listOf(RBracket()),
					"Error: An array length expression must be followed by a right square bracket.",
					parserStates.PARSER_DECLARATION_STAGE
				)

				val tokenAfterLength = expectToken(
					0,
					listOf(Assign(), Semicolon()),
					"Error: An array length expression must be followed by an assignment operator and initializer list or a semicolon.",
					parserStates.PARSER_DECLARATION_STAGE
				)

				return when (tokenAfterLength) {
					is Assign -> {
						val initializerList = parseArrayInitializers()
						expectToken(
							0,
							listOf(Semicolon()),
							"Error: An array initializer must be followed by a semicolon.",
							parserStates.PARSER_DECLARATION_STAGE
						)
						ArrayDeclaration(varDecl, len, initializerList, line)
					}

					is Semicolon -> {
						ArrayDeclaration(varDecl, len, listOf(), line)
					}

					else -> {
						throwError(
							parserStates.PARSER_DECLARATION_STAGE,
							"Error: Failed to parse an array declaration."
						)
					}
				}
			}

			is Semicolon -> {
				return ArrayDeclaration(varDecl, 0, listOf(), line)
			}

			else -> {
				throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Failed to parse an array declaration.")
			}
		}
	}

	private fun parseArrayInitializers(): List<Expression> {
		val initializerList: MutableList<Expression> = mutableListOf()

		if (tokens[0] is LBrace) {
			tokens.removeFirst()
			while (tokens[0] !is RBrace) {
				initializerList.add(parseExpression())
				if (tokens[0] is Comma) {
					tokens.removeFirst()
				} else if (tokens[0] is RBrace) {
					break
				} else {
					throwError(
						parserStates.PARSER_DECLARATION_STAGE,
						"Error: Unexpected token in array initializer list."
					)
				}
			}
		}

		expectToken(
			0,
			listOf(RBrace()),
			"Error: An array initializer must be followed by a right brace.",
			parserStates.PARSER_DECLARATION_STAGE
		)

		return initializerList
	}

	private fun parseEnumDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("an enum name", "enum declaration")

		expectToken(
			0,
			listOf(LBrace()),
			"Error: An enum declaration must contain an identifier followed by a series of mappings.",
			parserStates.PARSER_DECLARATION_STAGE
		)

		val enumEntries = mutableListOf<Pair<String, Long>>()
		var enumNumber = 0L

		while (true) {
			val nextToken = expectToken(
				0,
				listOf(Ident(), RBrace()),
				"Error: An enum declaration must contain a braced block of definitions.",
				parserStates.PARSER_DECLARATION_STAGE
			)

			when (nextToken) {
				is Ident -> {
					val paramName = expectIdentifierToken("an enum parameter name", "enum declaration")

					val tokenAfterName = expectToken(
						0,
						listOf(Colon(), Comma()),
						"Error: An enum definition is either an identifier followed by a comma or an identifier, a colon, a number, and then a comma.",
						parserStates.PARSER_DECLARATION_STAGE
					)

					when (tokenAfterName) {
						is Colon -> {
							val numVal = expectToken(
								0,
								listOf(Num()),
								"Error: Must define an enum entry as a number.",
								parserStates.PARSER_DECLARATION_STAGE
							) as Num

							if (!numVal.is_int) {
								throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Enums must be integers.")
							}

							val num = numVal.int
							enumEntries.add(Pair(paramName, num))
							enumNumber = num
						}

						is Comma -> {
							enumEntries.add(Pair(paramName, enumNumber))
							enumNumber++
						}

						else -> throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Error in enum parsing")
					}
				}

				is RBrace -> {
					if (enumEntries.isEmpty()) {
						println("WARNING: Empty enum definition at line ${tokens[0].line}.")
					}
					tokens.removeFirst()
					break
				}

				else -> throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Error in enum parsing")
			}
		}

		return EnumDeclaration(name, enumEntries, line)
	}

	private fun parseStructDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a struct name", "struct definition")
		val body = parseDeclarationBlock("struct")
		return StructDeclaration(name, body, line)
	}

	private fun parseClassDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a class name", "class definition")
		val inheritsFrom = mutableListOf<String>()
		if (tokens[0] is Colon) {
			tokens.removeFirst()
			val nextToken = expectToken(
				0,
				listOf(LParen(), Ident()),
				"Error: Expected either a single class or a list of classes in parentheses to inherit from.",
				parserStates.PARSER_DECLARATION_STAGE
			)
			if (nextToken is LParen) {
				while (true) {
					val token = expectToken(
						0,
						listOf(Ident(), RParen(), Comma()),
						"Error: Unexpected token in inheritance list.",
						parserStates.PARSER_DECLARATION_STAGE
					)
					when (token) {
						is Ident -> {
							val className = token.ident
							inheritsFrom.add(className)
						}

						is RParen -> {
							break
						}

						is Comma -> {
							// Commas are allowed between inherited classes
						}

						else -> {
							throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Failed to parse")
						}
					}
				}
			} else {
				inheritsFrom.add((nextToken as Ident).ident)
			}
		}
		val body = parseDeclarationBlock("class")
		return ClassDeclaration(name, body, inheritsFrom, line)
	}

	private fun parseDeclarationBlock(blockType: String): List<Declaration> {
		val list = mutableListOf<Declaration>()
		expectToken(
			0,
			listOf(LBrace()),
			"The body of $blockType must be braced.",
			parserStates.PARSER_DECLARATION_STAGE
		)
		while (tokens[0] !is RBrace && tokens[0] !is EndOfFile) {
			list.add(parseDeclaration())
		}
		expectToken(
			0,
			listOf(RBrace()),
			"The body of $blockType must be braced.",
			parserStates.PARSER_DECLARATION_STAGE
		)
		return list
	}

	private fun parseFunDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a function name", "function definition")
		val params = parseFunctionParams()
		expectToken(
			0,
			listOf(Colon()),
			"Error: Function needs type annotations after parameters",
			parserStates.PARSER_DECLARATION_STAGE
		)
		val type = expectIdentifierToken("a return type", "function definition")
		val body = parseBlock()
		return FunctionDeclaration(name, params, type, body, line)
	}

	private fun parseFunctionParams(): List<Pair<String, String>> {
		expectToken(
			0,
			listOf(LParen()),
			"Error: A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses.",
			parserStates.PARSER_EXPRESSION_STAGE
		)
		val params = mutableListOf<Pair<String, String>>()
		var expectComma = false
		while (true) {
			when (val token = tokens[0]) {
				is Ident -> {
					if (expectComma) {
						println("WARNING: Expected a comma at identifier ${token.ident} in parameter definition on line ${token.line}")
					}
					expectComma = true
					val name = expectIdentifierToken("a parameter name", "lambda")

					expectToken(
						0,
						listOf(Colon()),
						"Error: A parameter definition must contain an identifier followed by a colon and then a typename.",
						parserStates.PARSER_EXPRESSION_STAGE
					)

					val type = expectIdentifierToken("a parameter type", "lambda")

					params.add(name to type)
				}

				is RParen -> {
					if (params.isEmpty()) {
						println("WARNING: Empty parameter definition at line ${token.line}.")
					}
					tokens.removeFirst()
					break
				}

				is Comma -> {
					if (expectComma) {
						expectComma = false
					} else {
						println("WARNING: Unexpected comma in parameter definition at line ${token.line}")
					}
					tokens.removeFirst()
				}

				else -> expectToken(
					0,
					listOf(Ident(), RParen(), Comma()),
					"Error: Unexpected token in parameter list.",
					parserStates.PARSER_EXPRESSION_STAGE
				)
			}
		}
		return params
	}
}