package Parser

import AST.*
import Tokenizer.*

class CodeParser(private val tokens: MutableList<Token>) {

	private val reservedSyntax = listOf(
		"if",
		"while",
		"for",
		"foreach",
		"do",
		"class",
		"struct",
		"var",
		"fun",
		"enum",
		"switch",
		"else",
		"throw",
		"try",
		"catch"
	)
	// Error messages
	private val ERROR_IF_CONDITIONAL = "Error: Each if statement must contain a conditional enclosed in parentheses."

	private val ERROR_EOF = "Unexpected EOF"

	private val ERROR_BLOCK_EXPECTED = "Error: Block expected."

	private val ERROR_UNTERMINATED_BLOCK = "Error: Unterminated block."

	private val ERROR_FOR_OPENING_PARENTHESIS = "Error: Opening parenthesis expected after 'for'."

	private val ERROR_INVALID_CONDITIONAL = "Error: Invalid Conditional"

	private val ERROR_FOR_CLOSING_PARENTHESIS = "Error: Closing parenthesis expected after 'for'."

	private val ERROR_FOREACH_CONTROL_STRUCTURE = "Error: Foreach loops must be followed by control structure."

	private val ERROR_FOREACH_LOOP_FORMAT =
		"Error: Foreach loops consist of an iteration variable, a colon ':', and a collection variable."

	private val ERROR_FOREACH_LOOP_TERMINATION = "Error: Foreach loop control structures must be terminated."

	private val ERROR_WHILE_CONDITIONAL = "Error: While loops must be followed by a conditional in parentheses."

	private val ERROR_WHILE_UNCLOSED_PARENTHESIS = "Error: Unclosed parenthesis in while loop conditional."

	private val ERROR_DO_LOOP_TERMINATION = "Error: Do loops must be followed by a 'while' block and control structure."

	private val ERROR_DO_LOOP_END = "Error: Do-while loops end with a semicolon after the while statement."

	private val ERROR_SWITCH_EXPRESSION = "Error: Switch statements must be followed by the expression to switch on."

	private val ERROR_UNCLOSED_EXPRESSION = "Error: Unclosed expression in switch block."

	private val ERROR_SWITCH_BLOCK = "Error: Switch statements must be followed by a switch block."

	private val ERROR_CASE_BLOCK_FORMAT =
		"Error: Case blocks consist of the keyword 'case' or 'default' followed by an expression, a colon, and a code block."

	private val ERROR_CASE_EXPRESSION_COLON = "Error: Case expression must be followed by a colon."

	private val ERROR_UNTERMINATED_SWITCH_BLOCK = "Error: Unterminated switch block. Did you forget a closing brace?"

	private val ERROR_SWITCH_BLOCK_END = "Error: Switch blocks end with right braces."

	private val ERROR_MISSING_SEMICOLON = "Error: All statements must end with semicolons."

	private val ERROR_ASSIGNMENT_MALFORMED = "Error: Assignment malformed."

	private val ERROR_INVALID_LABEL = "Error: Labels consist of an identifier followed by a colon."

	private val ERROR_TRY_BLOCK_FORMAT =
		"Error: try blocks consist of a code block followed by a 'catch' statement and a parenthetical."

	private val ERROR_CATCH_PARENTHESIS = "Error: catch statements must be followed by a parenthetical."

	private val ERROR_INVALID_CATCH_VARIABLE = "Error: catch variable must be typed."

	private val ERROR_CATCH_TERMINATION = "Error: catch statements must be terminated."

	private val ERROR_THROW_PARENTHESIS = "Error: a throw expression must have its arguments in parentheses."

	private val ERROR_INVALID_EXCEPTION_PARAMETERS = "Error: Invalid exception. Parameter list is not terminated."

	private val ERROR_VAR_DEFINITION =
		"Error: A variable definition must contain an identifier followed by a colon and then a typename."

	private val ERROR_VAR_ASSIGNMENT =
		"Error: A variable definition must be followed by either an assignment or a semicolon."

	private val ERROR_EXPR_SEMICOLON = "Error: An expression in a definition must be followed by a semicolon."

	private val ERROR_ARRAY_DEFINITION =
		"Error: An array definition must contain an identifier followed by a colon and then a typename."


	private val ERROR_ARRAY_LENGTH = "Error: Array lengths must be whole numbers."

	private val ERROR_ARRAY_ASSIGNMENT =
		"Error: An array length expression must be followed by an assignment operator and initializer list or a semicolon."

	private val ERROR_ARRAY_BRACES =
		"Error: An array definition must be followed by either an assignment, a length in braces, or a semicolon."

	private val ERROR_ARRAY_INITIALIZER = "Error: Unexpected token in array initializer list."

	private val ERROR_ARRAY_RSQUARE = "Error: An array length expression must be followed by a right square bracket."

	private val ERROR_ARRAY_FAILED = "Error: Failed to parse an array declaration."

	private val ERROR_ARRAY_BRACKET = "Error: An array initializer must be followed by a right brace."

	private val ERROR_ENUM_DECLARATION = "Error: An enum declaration must contain a braced block of definitions."

	private val ERROR_ENUM_ENTRY =
		"Error: An enum definition is either an identifier followed by a comma or an identifier, a colon, a number, and then a comma."

	private val ERROR_UNEXPECTED_TOKEN = "Error: Unexpected token in %s."

	private val ERROR_FAILED_PARSE = "Error: Failed to parse."

	private val ERROR_EMPTY_PARAMETER_DEFINITION = "WARNING: Empty parameter definition."

	private val ERROR_UNEXPECTED_COMMA = "WARNING: Unexpected comma in parameter definition."

	private val ERROR_FUNCTION_PARAMS = "Error: A function parameter list must be enclosed in parentheses."

	private val ERROR_MISSING_LAMBDA = "Lambda needs type annotations."

	private val ERROR_PARAM_DEFINITION_MISSING_LEFT_PAREN =
		"A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses."

	private val ERROR_PARAM_DEFINITION_MISSING_COLON =
		"A parameter definition must contain an identifier followed by a colon and then a typename."


	// Used by parse-statement to allow for statements which end in blocks (I.E control flow statements). Hacky but works so vOv
	private var semicolon_exempt: Boolean = false

	// Used to store successfully parsed declarations for error recovery.
	private var partialDeclaration: MutableList<Declaration> = mutableListOf()

	// Used to scope error messages.
	private enum class parserStates {
		PARSER_EXPRESSION_STAGE,
		PARSER_STATEMENT_STAGE,
		PARSER_DECLARATION_STAGE
	}

	// When enums are defined their members are appended to this list.
	private var reserved: MutableList<String> = mutableListOf(
		"true",
		"false",
		"nil"
	)

	/**
	 * Parses a program. Contains logic to store the current program state in case of an error.
	 * @return A list of declarations representing a parsed program.
	 * ***Grammar***
	 * * program -> (declarations)* EoF
	 */
	fun parseProgram(): List<Declaration> {
		val list = mutableListOf<Declaration>()
		while (tokens[0] !is EndOfFile) {
			list.add(parseDeclaration())
			partialDeclaration = list
		}
		return list
	}

	/**
	 * A wrapper for assignment booleans.
	 * @return If the token is an assignment operator.
	 */
	private fun isAssignment(token: Token): Boolean {
		return token is Assign || token is AddAssign || token is SubAssign ||
				token is MulAssign || token is DivAssign || token is ModAssign ||
				token is PowAssign || token is AndAssign || token is OrAssign ||
				token is LShiftAssign || token is RShiftAssign || token is XorAssign
	}

	/**
	 * When parser errors occur, this function consumes all tokens up to the nearest synchronizeable token.
	 * Synchronizeable tokens are those which are likely to be followed by valid syntax, I.E control flow structures or semicolons.
	 * This allows the parser to continue without throwing out a bunch of errors.
	 * General logic from: https://www.rose-hulman.edu/class/cs/csse404/schedule/day28/28-ErrorRecoveryI.pdf
	 * @return the current list of partial declarations.
	 */
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
						"struct",
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
					else
					{
						tokens.removeFirst()
					}
				}

				else -> {
					tokens.removeFirst()
				}
			}
		}
		return declsSoFar
	}

	/**
	 * This function throws a line-numbered error code.
	 * parserStates is used to provide the current parser state, I.E expression/statement.
	 * Can help to distinguish errors, for example, the user should be able to tell -
	 * between an error in a for loop like a missing semicolon and an error in the loop conditional itself.
	 */
	private fun throwError(parserStage: parserStates? = null, message: String, line_or_not: Int? = null): Nothing {
		val line: Int = line_or_not ?: tokens[0].line!!
		var errorMessage = message
		if(tokens[0] is Unimplemented)
		{
			errorMessage = "Unimplemented token - Unable to recognize ${(tokens[0] as Unimplemented).c}"
		}
		if (parserStage == null) {
			throw ParserException("Line $line - $errorMessage")
		}
		when (parserStage) {
			parserStates.PARSER_EXPRESSION_STAGE -> throw ParserException("Line $line while parsing expression - $errorMessage")
			parserStates.PARSER_STATEMENT_STAGE -> throw ParserException("Line $line while parsing statement - $errorMessage")
			parserStates.PARSER_DECLARATION_STAGE -> throw ParserException("Line $line while parsing declaration - $errorMessage")
		}
	}

	/**
	 * Throws an error if the wrong token is passed.
	 * This function allows you to get a token from the stream at the given index.
	 * Pretty straightforward.
	 */
	private fun expectToken(
		expectedToken: List<Token>,
		errorMessage: String,
		parserStage: parserStates
	): Token {
		if (tokens.isEmpty()) {
			throwError(parserStage, ERROR_EOF)
		}
		if (expectedToken.none { it::class == tokens.getOrNull(0)!!::class }) {
			if (expectedToken.all { it is Semicolon } && semicolon_exempt) {
				semicolon_exempt = false
				return Unimplemented('\u0000')
			}
			val line = tokens.getOrNull(0)!!.line
			val tokenString = tokToString(tokens.getOrNull(0)!!)
			val expectedTokensString = expectedToken.joinToString(" or ") { tokToString(it) }
			val error = "$errorMessage Got $tokenString, expected $expectedTokensString."
			throwError(parserStage, error, line)
		}
		if (tokens.getOrNull(0)!! is Semicolon) {
			semicolon_exempt = false
		}
		val result = tokens.getOrNull(0)!!
		tokens.removeFirst()
		return result
	}

	/**
	 * Throws an error if the wrong identifier is passed.
	 * It does not remove the token if it gets a match.
	 * Again, pretty straightforward.
	 */
	private fun expectIdent(
		ident: String,
		expectedIdent: List<String>,
		errorMessage: String,
		line: Int
	) {
		if (tokens.isEmpty()) {
			throwError(message = ERROR_EOF)
		}
		if (expectedIdent.none { it == ident }) {
			val expectedIdentString = expectedIdent.joinToString(" or ") { it }
			val error = "$errorMessage Got '$ident', expected $expectedIdentString."
			throwError(message = error, line_or_not = line)
		}
	}

	/**
	 * Throws an error if the wrong node is passed.
	 */
	private fun expectNode(
		receivedNode: ASTRoot,
		expectedNode: List<ASTRoot>,
		errorMessage: String,
		parserStage: parserStates
	): ASTRoot {
		if (tokens.isEmpty()) {
			throwError(parserStage, ERROR_EOF)
		}
		if (expectedNode.none { it::class == receivedNode::class }) {
			val line = receivedNode.line
			val tokenString = nodeToString(receivedNode)
			val expectedTokensString = expectedNode.joinToString(" or ") { nodeToString(it) }
			val error = "$errorMessage Got $tokenString, expected $expectedTokensString."
			throwError(parserStage, error, line)
		}
		return receivedNode
	}

	/**
	 * Opposite of expectToken. This function throws an error if a token in the rejectedToken list is passed.
	 */
	private fun rejectTokens(
		rejectedToken: List<Token>,
		errorMessage: String,
		parserStage: parserStates
	) {
		if (tokens.isEmpty()) {
			throwError(parserStage, ERROR_EOF)
		}
		if (rejectedToken.any { it::class == tokens.getOrNull(0)!!::class }) {
			val line = tokens.getOrNull(0)?.line
			val tokenString = tokToString(tokens.getOrNull(0)!!)
			val error = "$errorMessage Unexpected $tokenString."
			throwError(parserStage, error, line)
		}
	}

	/**
	 * Throws an error if an identifier is not passed.
	 * This function allows you to get a user-defined identifier.
	 * It removes and returns the identifier if it gets a match.
	 */
	private fun expectIdentifierToken(variableType: String, statementType: String): String {
		if (tokens[0] !is Ident) {
			throwError(
				message = "Error: missing an identifier in a $statementType. Got '${(tokToString(tokens[0]))}', expecting identifier for $variableType.",
				line_or_not = tokens[0].line
			)
		}
		val identifier = tokens[0] as Ident
		if(reservedSyntax.any {(tokens[0] as Ident).ident == it})
		{
			throwError(
				message = "Error: reserved token in $statementType. Got '${(tokens[0] as Ident).ident}', which cannot be used for $variableType.",
				line_or_not = tokens[0].line
			)
		}
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
		if (tokens.firstOrNull() is Ident && (tokens.firstOrNull() as Ident).ident == "lambda") {
			tokens.removeFirst()
			val lambda = parseLambdaBody()
			semicolon_exempt = true
			return lambda
		} else {
			return parseTernary()
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
		val line = tokens.firstOrNull()?.line ?: throwError(
			parserStates.PARSER_EXPRESSION_STAGE,
			ERROR_UNEXPECTED_TOKEN.format("lambda")
		)
		val params = parseParams()
		expectToken(listOf(Colon()), ERROR_MISSING_LAMBDA, parserStates.PARSER_EXPRESSION_STAGE)
		val type = expectIdentifierToken("a return type", "lambda statement")
		val block = parseBlock()
		return Lambda(params, block, type, line)
	}

	private fun parseTernary(): Expression {
		val line = tokens[0].line!!
		val lhs = parseBinaryOpExpression(0)
		if(tokens[0] is QMark)
		{
			tokens.removeFirst()
			val consequent = parseBinaryOpExpression(0)
			expectToken(listOf(Colon()), "Expected colon in ternary expression.", parserStates.PARSER_EXPRESSION_STAGE)
			val alternate = parseBinaryOpExpression(0)
			return Ternary(lhs, consequent, alternate, line)
		}
		return lhs
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
			listOf(LParen()),
			ERROR_PARAM_DEFINITION_MISSING_LEFT_PAREN,
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
						listOf(Colon()),
						ERROR_PARAM_DEFINITION_MISSING_COLON,
						parserStates.PARSER_EXPRESSION_STAGE
					)

					val type = expectIdentifierToken("a parameter type", "lambda")

					params.add(name to type)
				}

				is RParen -> {
					if (params.isEmpty()) {
						println("$ERROR_EMPTY_PARAMETER_DEFINITION at line ${token.line}.")
					}
					tokens.removeFirst()
					break
				}

				is Comma -> {
					if (expectComma) {
						expectComma = false
					} else {
						println("$ERROR_UNEXPECTED_COMMA at line ${token.line}")
					}
					tokens.removeFirst()
				}

				else -> expectToken(
					listOf(Ident(), RParen(), Comma()),
					ERROR_UNEXPECTED_TOKEN.format("lambda params"),
					parserStates.PARSER_EXPRESSION_STAGE
				)
			}
		}
		return params
	}

	private val precedenceTable = listOf(
		listOf(Or()),
		listOf(And()),
		listOf(Bitwise_Or()),
		listOf(Bitwise_Xor()),
		listOf(Bitwise_And()),
		listOf(EqualTo(), NotEqualTo()),
		listOf(GreaterThan(), GreaterEqual(), LessThan(), LessEqual()),
		listOf(Bitwise_LShift(), Bitwise_RShift()),
		listOf(Add(), Sub()),
		listOf(Mul(), Div(), Mod()),
		listOf(Pow())
	)

	private fun parseBinaryOpExpression(precedence: Int): Expression {
		if (precedence >= precedenceTable.size) {
			return parseUnary()
		}

		var lhs = parseBinaryOpExpression(precedence + 1)

		while (precedenceTable[precedence].any { it::class == tokens[0]::class }) {
			val line = tokens[0].line!!
			val op = when (val token = tokens.removeFirst()) {
				is Or -> BinaryOps.Or
				is And -> BinaryOps.And
				is Bitwise_Or -> BinaryOps.Bitwise_Or
				is Bitwise_Xor -> BinaryOps.Bitwise_Xor
				is Bitwise_And -> BinaryOps.Bitwise_And
				is EqualTo -> BinaryOps.EqualTo
				is NotEqualTo -> BinaryOps.NotEqualTo
				is GreaterThan -> BinaryOps.GreaterThan
				is GreaterEqual -> BinaryOps.GreaterEqual
				is LessThan -> BinaryOps.LessThan
				is LessEqual -> BinaryOps.LessEqual
				is Bitwise_LShift -> BinaryOps.Bitwise_LS
				is Bitwise_RShift -> BinaryOps.Bitwise_RS
				is Add -> BinaryOps.Add
				is Sub -> BinaryOps.Sub
				is Mul -> BinaryOps.Mul
				is Div -> BinaryOps.Div
				is Mod -> BinaryOps.Mod
				is Pow -> BinaryOps.Pow
				else -> throw IllegalArgumentException("Unknown token: $token")
			}
			val rhs = parseBinaryOpExpression(precedence + 1)
			lhs = BinaryOp(lhs, op, rhs, line)
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
					tokens.removeFirst()
					val idx = parseExpression()
					expectToken(
						listOf(RBracket()),
						"Expected closing brace on list access.",
						parserStates.PARSER_EXPRESSION_STAGE
					)
					ListAccess(identifier, idx, line)
				}

				tokens[0] is Dot -> {
					tokens.removeFirst()
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
					semicolon_exempt = true
					return parseIf()
				}

				"for" -> {
					tokens.removeFirst()
					semicolon_exempt = true
					return parseForLoop()
				}

				"foreach" -> {
					tokens.removeFirst()
					semicolon_exempt = true
					return parseForEachLoop()
				}

				"while" -> {
					tokens.removeFirst()
					semicolon_exempt = true
					return parseWhile()
				}

				"do" -> {
					tokens.removeFirst()
					semicolon_exempt = true
					return parseDo()
				}

				"continue" -> {
					tokens.removeFirst()
					semicolon_exempt = false
					Continue(line)
				}

				"break" -> {
					tokens.removeFirst()
					semicolon_exempt = false
					Break(line)
				}

				"switch" -> {
					tokens.removeFirst()
					semicolon_exempt = true
					return parseSwitch()
				}

				"return" -> {
					tokens.removeFirst()
					semicolon_exempt = false
					parseReturn()
				}

				"goto" -> {
					tokens.removeFirst()
					semicolon_exempt = false
					parseGoto()
				}

				"try" -> {
					tokens.removeFirst()
					semicolon_exempt = true
					return parseTry()
				}

				"throw" -> {
					tokens.removeFirst()
					semicolon_exempt = false
					parseThrow()
				}

				else -> {
					if (tokens[1] is Colon) {
						semicolon_exempt = true
						return parseLabel()
					} else {
						val lhs = parseExpression()
						if (isAssignment(tokens[0])) {
							parseAssignment(lhs)
						} else {
							ExprStatement(lhs, tokens[0].line!!)
						}
					}
				}
			}
		} else if (tokens[0] is LBrace) {
			parseBlock()
		} else {
			ExprStatement(parseExpression(), tokens[0].line!!)
		}

		expectToken(listOf(Semicolon()), ERROR_MISSING_SEMICOLON, parserStates.PARSER_STATEMENT_STAGE)
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
		val line = expectToken(listOf(LParen()), ERROR_IF_CONDITIONAL, parserStates.PARSER_STATEMENT_STAGE).line!!

		val conditional: Expression = parseExpression()
		if(conditional::class == BinaryOp::class)
		{
			val op = (conditional as BinaryOp).op
			when(op)
			{
				BinaryOps.LessThan, BinaryOps.LessEqual, BinaryOps.EqualTo, BinaryOps.GreaterEqual, BinaryOps.NotEqualTo, BinaryOps.GreaterThan, BinaryOps.And, BinaryOps.Or ->
				{
					// Good.
				}
				else ->
				{
					throwError(parserStates.PARSER_STATEMENT_STAGE,"Invalid conditional.")
				}
			}
		}
		else if(conditional::class == Bool::class)
		{
			// Good.
		}
		else {
			throwError(parserStates.PARSER_STATEMENT_STAGE, "Invalid conditional.")
		}

		expectToken(listOf(RParen()), ERROR_IF_CONDITIONAL, parserStates.PARSER_STATEMENT_STAGE)

		val consequent = parseBlock()

		val alternate = if (tokens.getOrNull(0) is Ident && (tokens[0] as Ident).ident == "else") {
			tokens.removeFirst()
			parseBlock()
		} else {
			null
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
		val line = expectToken(listOf(LBrace()), ERROR_BLOCK_EXPECTED, parserStates.PARSER_STATEMENT_STAGE).line!!

		val block = mutableListOf<Declaration>()
		while (tokens.getOrNull(0) !is RBrace) {
			block += parseDeclaration()
			rejectTokens(listOf(EndOfFile()), ERROR_UNTERMINATED_BLOCK, parserStates.PARSER_STATEMENT_STAGE)
		}

		expectToken(listOf(RBrace()), ERROR_BLOCK_EXPECTED, parserStates.PARSER_STATEMENT_STAGE)

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
		val line =
			expectToken(listOf(LParen()), ERROR_FOR_OPENING_PARENTHESIS, parserStates.PARSER_STATEMENT_STAGE).line!!

		var initializer: Declaration? = null
		var conditional: Declaration? = null
		var iteration: Declaration? = null

		if (tokens.getOrNull(0) !is RParen) {
			if (tokens.getOrNull(0) is Semicolon) {
				tokens.removeFirst()
			} else {
				semicolon_exempt = true
				initializer = parseDeclaration()
			}
		}
		if (tokens.getOrNull(0) !is RParen) {
			if (tokens.getOrNull(0) is Semicolon) {
				tokens.removeFirst()
			} else {
				semicolon_exempt = true
				conditional = parseDeclaration()
				val expr = (expectNode(
					conditional,
					listOf(ExprStatement(line = line)),
					ERROR_INVALID_CONDITIONAL,
					parserStates.PARSER_STATEMENT_STAGE
				) as ExprStatement).expr!!
				if(expr::class == BinaryOp::class)
				{
					val op = (expr as BinaryOp).op
					when(op)
					{
						BinaryOps.LessThan, BinaryOps.LessEqual, BinaryOps.EqualTo, BinaryOps.GreaterEqual, BinaryOps.NotEqualTo, BinaryOps.GreaterThan, BinaryOps.And, BinaryOps.Or->
						{
							// Good.
						}
						else ->
						{
							throwError(parserStates.PARSER_STATEMENT_STAGE,"Invalid conditional.")
						}
					}
				}
				else if(expr::class == Bool::class)
				{
					// Good.
				}
				else {
					throwError(parserStates.PARSER_STATEMENT_STAGE, "Invalid conditional.")
				}
			}
		}
		if (tokens.getOrNull(0) !is RParen) {
			if (tokens.getOrNull(0) is Semicolon) {
				tokens.removeFirst()
			} else {
				semicolon_exempt = true
				iteration = parseDeclaration()
			}
		}

		expectToken(listOf(RParen()), ERROR_FOR_CLOSING_PARENTHESIS, parserStates.PARSER_STATEMENT_STAGE)

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
		val line = expectToken(
			listOf(LParen()),
			ERROR_FOREACH_CONTROL_STRUCTURE,
			parserStates.PARSER_STATEMENT_STAGE
		).line!!

		val itervar = expectIdentifierToken("an iteration variable", "foreach")

		expectToken(listOf(Colon()), ERROR_FOREACH_LOOP_FORMAT, parserStates.PARSER_STATEMENT_STAGE)

		val collectionvar = expectIdentifierToken("a collection variable", "foreach")

		expectToken(listOf(RParen()), ERROR_FOREACH_LOOP_TERMINATION, parserStates.PARSER_STATEMENT_STAGE)

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
		val line = expectToken(listOf(LParen()), ERROR_WHILE_CONDITIONAL, parserStates.PARSER_STATEMENT_STAGE).line!!

		val conditional: Expression = parseExpression()
		if(conditional::class == BinaryOp::class)
		{
			val op = (conditional as BinaryOp).op
			when(op)
			{
				BinaryOps.LessThan, BinaryOps.LessEqual, BinaryOps.EqualTo, BinaryOps.GreaterEqual, BinaryOps.NotEqualTo, BinaryOps.GreaterThan, BinaryOps.And, BinaryOps.Or ->
				{
					// Good.
				}
				else ->
				{
					throwError(parserStates.PARSER_STATEMENT_STAGE,"Invalid conditional.")
				}
			}
		}
		else if(conditional::class == Bool::class)
		{
			// Good.
		}
		else {
			throwError(parserStates.PARSER_STATEMENT_STAGE, "Invalid conditional.")
		}
		expectToken(listOf(RParen()), ERROR_WHILE_UNCLOSED_PARENTHESIS, parserStates.PARSER_STATEMENT_STAGE)

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
		val line = tokens.getOrNull(0)?.line!!
		val body = parseBlock()

		expectIdent(
			(tokens[0] as Ident).ident,
			listOf("while"),
			ERROR_DO_LOOP_TERMINATION,
			line
		)
		tokens.removeFirst()
		expectToken(listOf(LParen()), ERROR_WHILE_CONDITIONAL, parserStates.PARSER_STATEMENT_STAGE)

		val conditional: Expression = parseExpression()
		if(conditional::class == BinaryOp::class)
		{
			val op = (conditional as BinaryOp).op
			when(op)
			{
				BinaryOps.LessThan, BinaryOps.LessEqual, BinaryOps.EqualTo, BinaryOps.GreaterEqual, BinaryOps.NotEqualTo, BinaryOps.GreaterThan, BinaryOps.And, BinaryOps.Or ->
				{
					// Good.
				}
				else ->
				{
					throwError(parserStates.PARSER_STATEMENT_STAGE,"Invalid conditional.")
				}
			}
		}
		else if(conditional::class == Bool::class)
		{
			// Good.
		}
		else {
			throwError(parserStates.PARSER_STATEMENT_STAGE, "Invalid conditional.")
		}

		expectToken(listOf(RParen()), ERROR_WHILE_UNCLOSED_PARENTHESIS, parserStates.PARSER_STATEMENT_STAGE)

		expectToken(listOf(Semicolon()), ERROR_DO_LOOP_END, parserStates.PARSER_STATEMENT_STAGE)

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
		val line = expectToken(listOf(LParen()), ERROR_SWITCH_EXPRESSION, parserStates.PARSER_STATEMENT_STAGE).line!!

		val conditional: Expression = parseExpression()

		expectToken(listOf(RParen()), ERROR_UNCLOSED_EXPRESSION, parserStates.PARSER_STATEMENT_STAGE)

		expectToken(listOf(LBrace()), ERROR_SWITCH_BLOCK, parserStates.PARSER_STATEMENT_STAGE)

		val cases = mutableListOf<Pair<Expression, Statement>>()
		while (tokens.getOrNull(0) !is RBrace) {
			expectIdent(
				(tokens[0] as Ident).ident,
				listOf("case", "default"),
				ERROR_CASE_BLOCK_FORMAT,
				tokens[0].line!!
			)
			val (exp, block) = if ((tokens.getOrNull(0) as Ident).ident == "default") {
				expectToken(listOf(Ident()), "How", parserStates.PARSER_STATEMENT_STAGE)
				expectToken(listOf(Colon()), ERROR_CASE_EXPRESSION_COLON, parserStates.PARSER_STATEMENT_STAGE)
				Bool(true, line) to parseBlock()
			} else {
				expectToken(listOf(Ident()), "How", parserStates.PARSER_STATEMENT_STAGE)
				val primary = parsePrimary()
				expectToken(listOf(Colon()), ERROR_CASE_EXPRESSION_COLON, parserStates.PARSER_STATEMENT_STAGE)
				primary to parseBlock()
			}

			cases += exp to block

			rejectTokens(listOf(EndOfFile()), ERROR_UNTERMINATED_SWITCH_BLOCK, parserStates.PARSER_STATEMENT_STAGE)
		}

		expectToken(listOf(RBrace()), ERROR_SWITCH_BLOCK_END, parserStates.PARSER_STATEMENT_STAGE)

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
				is AddAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Add, parseExpression(), line), line)
				is SubAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Sub, parseExpression(), line), line)
				is MulAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Mul, parseExpression(), line), line)
				is DivAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Div, parseExpression(), line), line)
				is ModAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Mod, parseExpression(), line), line)
				is PowAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Pow, parseExpression(), line), line)
				is AndAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Bitwise_And, parseExpression(), line), line)
				is OrAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Bitwise_Or, parseExpression(), line), line)
				is XorAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Bitwise_Xor, parseExpression(), line), line)
				is LShiftAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Bitwise_LS, parseExpression(), line), line)
				is RShiftAssign -> Assignment(lhs, BinaryOp(lhs, BinaryOps.Bitwise_RS, parseExpression(), line), line)
				else -> throwError(parserStates.PARSER_STATEMENT_STAGE, ERROR_ASSIGNMENT_MALFORMED)
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
		val label = expectIdentifierToken("a label", "label definition")
		val line = expectToken(listOf(Colon()), ERROR_INVALID_LABEL, parserStates.PARSER_EXPRESSION_STAGE).line!!
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
			ERROR_TRY_BLOCK_FORMAT,
			tokens[0].line!!
		)

		tokens.removeFirst()

		expectToken(listOf(LParen()), ERROR_CATCH_PARENTHESIS, parserStates.PARSER_STATEMENT_STAGE)

		val line = tokens[0].line!!

		val catches = expectIdentifierToken("an exception variable", "catch block")

		expectToken(listOf(Colon()), ERROR_INVALID_CATCH_VARIABLE, parserStates.PARSER_STATEMENT_STAGE)

		val catchesAs = expectIdentifierToken("a type", "catch block")

		expectToken(listOf(RParen()), ERROR_CATCH_TERMINATION, parserStates.PARSER_STATEMENT_STAGE)

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

		expectToken(listOf(LParen()), ERROR_THROW_PARENTHESIS, parserStates.PARSER_STATEMENT_STAGE)

		val params = mutableListOf<Expression>()

		while (true) {
			when {
				tokens[0] is RParen -> {
					tokens.removeFirst()
					break
				}

				tokens[0] is Comma -> tokens.removeFirst()
				tokens[0] is EndOfFile || tokens[0] is Semicolon -> {
					throwError(parserStates.PARSER_EXPRESSION_STAGE, ERROR_INVALID_EXCEPTION_PARAMETERS)
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

				else -> parseStatement()
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
	 * * * Name -> [Ident]ifier
	 *
	 * * * Type -> [Ident]ifier
	 */
	private fun parseVarDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a variable name", "variable definition")

		expectToken(listOf(Colon()), ERROR_VAR_DEFINITION, parserStates.PARSER_DECLARATION_STAGE)

		val type = expectIdentifierToken("a variable type", "variable definition")

		val varDecl = name to type

		val nextToken = expectToken(
			listOf(Assign(), Semicolon()),
			ERROR_VAR_ASSIGNMENT,
			parserStates.PARSER_DECLARATION_STAGE
		)

		return when (nextToken) {
			is Assign -> {
				val expr = parseExpression()
				expectToken(listOf(Semicolon()), ERROR_EXPR_SEMICOLON, parserStates.PARSER_DECLARATION_STAGE)
				SimpleVarDeclaration(varDecl, expr, line)
			}

			is Semicolon -> {
				SimpleVarDeclaration(varDecl, null, line)
			}

			else -> throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Failed to parse a variable declaration.")
		}
	}

	/**
	 * Parses an array.
	 * @return The parsed declaration.
	 *
	 * ***Grammar:***
	 * * ArrayDeclaration -> "array" Name ":" Type ["[" Number "]"] ["=" ExpressionList] ";"
	 *
	 * * Name -> [Ident]ifier
	 *
	 * * Type -> [Ident]ifier
	 */
	private fun parseArrayDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a variable name", "array definition")

		expectToken(listOf(Colon()), ERROR_ARRAY_DEFINITION, parserStates.PARSER_DECLARATION_STAGE)

		val type = expectIdentifierToken("a variable type", "array definition")

		val varDecl = name to type

		val nextToken = expectToken(
			listOf(LBracket(), Assign(), Semicolon()),
			ERROR_ARRAY_BRACES,
			parserStates.PARSER_DECLARATION_STAGE
		)

		return when (nextToken) {
			is Assign -> {
				val initializerList = parseArrayInitializers()
				expectToken(
					listOf(Semicolon()),
					ERROR_ARRAY_INITIALIZER,
					parserStates.PARSER_DECLARATION_STAGE
				)
				ArrayDeclaration(varDecl, -1, initializerList, line)
			}

			is LBracket -> {
				val lenNum = expectToken(
					listOf(Num()),
					ERROR_ARRAY_LENGTH,
					parserStates.PARSER_DECLARATION_STAGE
				) as Num

				if (!lenNum.is_int) {
					throwError(parserStates.PARSER_DECLARATION_STAGE, ERROR_ARRAY_LENGTH)
				}

				val len = lenNum.int

				expectToken(
					listOf(RBracket()),
					ERROR_ARRAY_RSQUARE,
					parserStates.PARSER_DECLARATION_STAGE
				)

				val tokenAfterLength = expectToken(
					listOf(Assign(), Semicolon()),
					ERROR_ARRAY_ASSIGNMENT,
					parserStates.PARSER_DECLARATION_STAGE
				)

				return when (tokenAfterLength) {
					is Assign -> {
						val initializerList = parseArrayInitializers()
						expectToken(
							listOf(Semicolon()),
							ERROR_ARRAY_INITIALIZER,
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
							ERROR_ARRAY_FAILED
						)
					}
				}
			}

			is Semicolon -> {
				ArrayDeclaration(varDecl, 0, listOf(), line)
			}

			else -> {
				throwError(parserStates.PARSER_DECLARATION_STAGE, ERROR_ARRAY_FAILED)
			}
		}
	}

	/**
	 * Parses an array initalizer.
	 * @return The parsed expression.
	 *
	 * ***Grammar:***
	 * * ExpressionList ->  "{" (Expression ",")+ "}"
	 * * * Expression
	 */
	private fun parseArrayInitializers(): List<Expression> {
		val initializerList = mutableListOf<Expression>()

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
						ERROR_ARRAY_INITIALIZER
					)
				}
			}
		}

		expectToken(
			listOf(RBrace()),
			ERROR_ARRAY_BRACKET,
			parserStates.PARSER_DECLARATION_STAGE
		)

		return initializerList
	}

	/**
	 * Parses an enum.
	 * @return The parsed declaration.
	 *
	 * ***Grammar:***
	 * * EnumDeclaration -> "enum" Name "{" EnumPairs "}"
	 *
	 * * Name -> [Ident]ifier
	 * * EnumPairs ->  Name ":" Primary ","
	 */
	private fun parseEnumDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("an enum name", "enum declaration")

		expectToken(
			listOf(LBrace()),
			"Error: An enum declaration must contain an identifier followed by a series of mappings.",
			parserStates.PARSER_DECLARATION_STAGE
		)

		val enumEntries = mutableListOf<Pair<String, Long>>()
		var enumNumber = 0L

		while (true) {
			val nextToken = expectToken(
				listOf(Ident(), RBrace()),
				ERROR_ENUM_DECLARATION,
				parserStates.PARSER_DECLARATION_STAGE
			)

			when (nextToken) {
				is Ident -> {
					val paramName = (nextToken as Ident).ident

					val tokenAfterName = expectToken(
						listOf(Colon(), Comma()),
						ERROR_ENUM_ENTRY,
						parserStates.PARSER_DECLARATION_STAGE
					)

					when (tokenAfterName) {
						is Colon -> {
							val numVal = expectToken(
								listOf(Num()),
								"Error: Must define an enum entry as a number.",
								parserStates.PARSER_DECLARATION_STAGE
							) as Num

							if (!numVal.is_int) {
								throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Enums must be integers.")
							}

							val num = numVal.int
							reserved += paramName
							enumEntries.add(Pair(paramName, num))
							enumNumber = num
						}

						is Comma -> {
							reserved += paramName
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
					break
				}

				else -> throwError(parserStates.PARSER_DECLARATION_STAGE, "Error: Error in enum parsing")
			}
		}

		return EnumDeclaration(name, enumEntries, line)
	}

	/**
	 * Parses a struct.
	 * @return The parsed declaration.
	 *
	 * ***Grammar:***
	 * * StructDeclaration -> "struct" Name "{" DeclarationList "}"
	 *
	 * * Name -> [Ident]ifier
	 */
	private fun parseStructDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a struct name", "struct definition")
		val body = parseDeclarationBlock("struct")
		return StructDeclaration(name, body, line)
	}

	/**
	 * Parses a class.
	 * @return The parsed declaration.
	 *
	 * ***Grammar:***
	 * * ClassDeclaration -> "struct" Name [":" InheritsFrom] "{" DeclarationList "}"
	 *
	 * * Name -> [Ident]ifier
	 *
	 * * InheritsFrom -> Name |
	 * * * "(" Name ("," Name)* ")"
	 */
	private fun parseClassDeclaration(): Declaration {
		val line = tokens[0].line!!
		val name = expectIdentifierToken("a class name", "class definition")
		val inheritsFrom = mutableListOf<String>()
		if (tokens[0] is Colon) {
			tokens.removeFirst()
			val nextToken = expectToken(
				listOf(LParen(), Ident()),
				"Error: Expected either a single class or a list of classes in parentheses to inherit from.",
				parserStates.PARSER_DECLARATION_STAGE
			)
			if (nextToken is LParen) {
				while (true) {
					val token = expectToken(
						listOf(Ident(), RParen(), Comma()),
						ERROR_UNEXPECTED_TOKEN.format("inheritance list"),
						parserStates.PARSER_STATEMENT_STAGE
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
							throwError(parserStates.PARSER_DECLARATION_STAGE, ERROR_FAILED_PARSE)
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

	/**
	 * Parses a declaration list.
	 * @return The parsed declaration list.
	 *
	 * ***Grammar:***
	 * * DeclarationList -> "{" (Declarations)* "}"
	 */
	private fun parseDeclarationBlock(blockType: String): List<Declaration> {
		expectToken(
			listOf(LBrace()),
			"The body of $blockType must be braced.",
			parserStates.PARSER_DECLARATION_STAGE
		)
		val list = mutableListOf<Declaration>()
		while (tokens[0] !is RBrace && tokens[0] !is EndOfFile) {
			list.add(parseDeclaration())
		}
		expectToken(
			listOf(RBrace()),
			"The body of $blockType must be braced.",
			parserStates.PARSER_DECLARATION_STAGE
		)
		return list
	}

	/**
	 * Parses a function.
	 * @return A function node.
	 *
	 * ***Grammar:***
	 * * Function -> "fun" funName Parameters ":" returnType Block
	 * * * funName -> Identifier
	 * * * returnType -> Identifier
	 */
	private fun parseFunDeclaration(): Declaration {
		val line = tokens[0].line!!
		var name = expectIdentifierToken("a function name", "function definition")
		// Hacky fix.
		if(name == "fun")
		{
			println("WARNING: either you are trying to define a function named fun or the parser was unable to synchronize properly. In either case stop it.")
			var name = expectIdentifierToken("a function name", "function definition")
		}
		val params = parseFunctionParams()
		expectToken(
			listOf(Colon()),
			"Error: Function needs type annotations after parameters",
			parserStates.PARSER_DECLARATION_STAGE
		)
		val type = expectIdentifierToken("a return type", "function definition")
		val body = parseBlock()
		return FunctionDeclaration(name, params, type, body, line)
	}


	/**
	 * Parses the parameters of a function.
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
	private fun parseFunctionParams(): List<Pair<String, String>> {
		expectToken(
			listOf(LParen()),
			ERROR_FUNCTION_PARAMS,
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
						listOf(Colon()),
						"Error: A parameter definition must contain an identifier followed by a colon and then a typename.",
						parserStates.PARSER_EXPRESSION_STAGE
					)

					val type = expectIdentifierToken("a parameter type", "lambda")

					params.add(name to type)
				}

				is RParen -> {
					if (params.isEmpty()) {
						println(ERROR_EMPTY_PARAMETER_DEFINITION)
					}
					tokens.removeFirst()
					break
				}

				is Comma -> {
					if (expectComma) {
						expectComma = false
					} else {
						println(ERROR_UNEXPECTED_COMMA)
					}
					tokens.removeFirst()
				}

				else -> expectToken(
					listOf(Ident(), RParen(), Comma()),
					ERROR_UNEXPECTED_TOKEN.format("parameter list"),
					parserStates.PARSER_DECLARATION_STAGE
				)
			}
		}
		return params
	}

	fun hasTokens(): Boolean {
		return tokens.isNotEmpty() && tokens[0] !is EndOfFile
	}
}