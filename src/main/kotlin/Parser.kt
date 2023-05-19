import AST.*
import ParserExcptions.ExpressionParserException

class Parser(private val tokens: MutableList<Token>) {
    /*

            EXPRESSIONS

     */
    fun parseExpression(): Expression {
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
        var lhs = parseLogicalNotExpression()
        while (tokens[0] is And) {
            tokens.removeFirst()
            val rhs = parseLogicalNotExpression()
            lhs = BinaryOp(lhs, BinaryOps.And, rhs)
        }
        return lhs
    }

    private fun parseLogicalNotExpression(): Expression {
        return if (tokens[0] is Not) {
            tokens.removeFirst()
            val notExpr = parseLogicalNotExpression()
            UnaryOp(UnaryOps.Not, notExpr)
        } else {
            parseRelationalExpression()
        }
    }


    private fun parseRelationalExpression(): Expression {
        var lhs = parseTerm()
        while (true) {
            lhs = when (tokens[0]) {
                is GreaterThan -> {
                    tokens.removeFirst()
                    val rhs = parseTerm()
                    BinaryOp(lhs, BinaryOps.GreaterThan, rhs)
                }

                is GreaterEqual -> {
                    tokens.removeFirst()
                    val rhs = parseTerm()
                    BinaryOp(lhs, BinaryOps.GreaterEqual, rhs)
                }

                is EqualTo -> {
                    tokens.removeFirst()
                    val rhs = parseTerm()
                    BinaryOp(lhs, BinaryOps.EqualTo, rhs)
                }

                is NotEqualTo -> {
                    tokens.removeFirst()
                    val rhs = parseTerm()
                    BinaryOp(lhs, BinaryOps.NotEqualTo, rhs)
                }

                is LessEqual -> {
                    tokens.removeFirst()
                    val rhs = parseTerm()
                    BinaryOp(lhs, BinaryOps.LessEqual, rhs)
                }

                is LessThan -> {
                    tokens.removeFirst()
                    val rhs = parseTerm()
                    BinaryOp(lhs, BinaryOps.LessThan, rhs)
                }

                else -> return lhs
            }
        }
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
        var lhs = parseBitwiseShift()
        while (tokens[0] is Pow) {
            tokens.removeFirst()
            val rhs = parseBitwiseShift()
            lhs = BinaryOp(lhs, BinaryOps.Pow, rhs)
        }
        return lhs
    }


    private fun parseBitwiseShift(): Expression {
        var lhs = parseBitwiseAnd()
        while (tokens[0] is Bitwise_LShift || tokens[0] is Bitwise_RShift) {
            lhs = when (tokens[0]) {
                is Bitwise_LShift -> {
                    tokens.removeFirst()
                    val rhs = parseBitwiseAnd()
                    BinaryOp(lhs, BinaryOps.Bitwise_LShift, rhs)
                }

                is Bitwise_RShift -> {
                    tokens.removeFirst()
                    val rhs = parseBitwiseAnd()
                    BinaryOp(lhs, BinaryOps.Bitwise_RShift, rhs)
                }

                else -> return lhs
            }
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
        var lhs = parseUnary()
        while (tokens[0] is Bitwise_Or) {
            tokens.removeFirst()
            val rhs = parseUnary()
            lhs = BinaryOp(lhs, BinaryOps.Bitwise_Or, rhs)
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
            parseCall()
        }
    }

    private fun parseCall(): Expression {
        if (tokens[0] is Ident) {
            val identifier = (tokens[0] as Ident).ident

            if (tokens[0] is LBracket) {
                val idx = parseExpression()

                if (tokens[0] !is RBracket) {
                    throw ExpressionParserException("Line ${tokens[0].line} - Expected closing brace on list access.")
                }

                tokens.removeFirst()
                return ListAccess(identifier, idx)
            } else if (tokens[0] is Dot) {
                val rhs = parseExpression()

                if (rhs !is VariableAccess && rhs !is ListAccess && rhs !is Call) {
                    throw ExpressionParserException("Line ${tokens[0].line} - Invalid ScopeOf expression, expected variable, list, or function call.")
                }

                return ScopeOf(identifier, rhs)
            } else if (tokens[0] is LParen) {
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
                            throw ExpressionParserException("Line ${tokens[0].line} - Invalid function call. Parameter list is not terminated.")
                        }

                        else -> params.add(parseExpression())
                    }
                }
                // Return the Call expression with identifier and parameters
                return Call(identifier, params)
            } else {
                return VariableAccess(identifier)
            }
        }

        return parsePrimary()
    }

    private fun parsePrimary(): Expression {
        when {
            tokens[0] is EndOfFile -> {
                throw ExpressionParserException("Line ${tokens[0].line} - Error in expression parsing code. Parser called parse_primary on empty list of tokens.")
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

            tokens[0] is String_ -> {
                tokens.removeFirst()
                return StringLit((tokens[0] as String_).str)
            }

            tokens[0] is Char_ -> {
                tokens.removeFirst()
                return Char_((tokens[0] as Char_).c)
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

            else -> throw ExpressionParserException("Line ${tokens[0].line} - Error, unexpected token. Got '${tokens[0]::class}', expecting integer or float or double or bool or enum type or parenthetical.")
        }
    }
    /*

        STATEMENTS

     */

    private fun parseBlock(): Block {
        TODO()
    }

    /*

        DECLARATIONS

     */
    // TODO: Implement declarations
}