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
        if (tokens[0]::class != LParen::class) {
            throw ExpressionParserException("Line ${tokens[0].line} - A parameter definition must contain a left paren '(' followed by a list of parameters enclosed in parentheses.")
        }
        tokens.removeFirst()
        val params = mutableListOf<String>()
        var expectComma = false
        while (true) {
            if (tokens[0]::class == Ident::class) {
                if (expectComma) {
                    println("WARNING: Expected a comma at identifier ${(tokens[0] as Ident).ident} in parameter definition on line ${tokens[0].line}")
                }
                expectComma = true
                params.add((tokens[0] as Ident).ident)
                tokens.removeFirst()
            } else if (tokens[0]::class == RParen::class) {
                if (params.isEmpty()) {
                    println("WARNING: Empty parameter definition at line ${tokens[0].line}.")
                }
                tokens.removeFirst()
                break
            } else if (tokens[0]::class == Comma::class) {
                if (expectComma) {
                    expectComma = false
                } else {
                    println("WARNING: Unexpected comma in parameter definition at line ${tokens[0].line}")
                }
                tokens.removeFirst()
            } else {
                throw ExpressionParserException("Line ${tokens[0].line} - Unexpected token in lambda parameter list.")

            }
        }
        return params
    }

    private fun parseLogicalOrExpression(): Expression {
        var lhs = parseLogicalAndExpression()
        while (true) {
            if (tokens[0]::class == Or::class) {
                tokens.removeFirst()
                val rhs = parseLogicalAndExpression()
                lhs = BinaryOp(lhs, BinaryOps.Or, rhs)
            } else {
                break
            }
        }
        return lhs
    }

    private fun parseLogicalAndExpression(): Expression {
        var lhs = parseLogicalNotExpression()
        while (true) {
            if (tokens[0]::class == And::class) {
                tokens.removeFirst()
                val rhs = parseLogicalNotExpression()
                lhs = BinaryOp(lhs, BinaryOps.And, rhs)
            } else {
                break
            }
        }
        return lhs
    }

    private fun parseLogicalNotExpression(): Expression {
        return if (tokens[0]::class == Not::class) {
            tokens.removeFirst()
            val notExpr = parseLogicalNotExpression()
            UnaryOp(UnaryOps.Not, notExpr)
        } else {
            val rel = parseRelationalExpression()
            rel
        }
    }

    private fun parseRelationalExpression(): Expression {
        var lhs = parseTerm()
        while (true) {
            if (tokens[0]::class == GreaterThan::class) {
                tokens.removeFirst()
                val rhs = parseTerm()
                lhs = BinaryOp(lhs, BinaryOps.GreaterThan, rhs)
            } else if (tokens[0]::class == GreaterEqual::class) {
                tokens.removeFirst()
                val rhs = parseTerm()
                lhs = BinaryOp(lhs, BinaryOps.GreaterEqual, rhs)
            } else if (tokens[0]::class == EqualTo::class) {
                tokens.removeFirst()
                val rhs = parseTerm()
                lhs = BinaryOp(lhs, BinaryOps.EqualTo, rhs)
            } else if (tokens[0]::class == NotEqualTo::class) {
                tokens.removeFirst()
                val rhs = parseTerm()
                lhs = BinaryOp(lhs, BinaryOps.NotEqualTo, rhs)
            } else if (tokens[0]::class == LessEqual::class) {
                tokens.removeFirst()
                val rhs = parseTerm()
                lhs = BinaryOp(lhs, BinaryOps.LessEqual, rhs)
            } else if (tokens[0]::class == LessThan::class) {
                tokens.removeFirst()
                val rhs = parseTerm()
                lhs = BinaryOp(lhs, BinaryOps.LessThan, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseTerm(): Expression {
        var lhs = parseFactor()
        while (true) {
            lhs = if (tokens[0]::class == Add::class) {
                tokens.removeFirst()
                val rhs = parseFactor()
                BinaryOp(lhs, BinaryOps.Add, rhs)
            } else if (tokens[0]::class == Sub::class) {
                tokens.removeFirst()
                val rhs = parseFactor()
                BinaryOp(lhs, BinaryOps.Sub, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseFactor(): Expression {
        var lhs = parseTightlyBindingFactor()
        while (true) {
            lhs = if (tokens[0]::class == Mul::class) {
                tokens.removeFirst()
                val rhs = parseTightlyBindingFactor()
                BinaryOp(lhs, BinaryOps.Mul, rhs)
            } else if (tokens[0]::class == Div::class) {
                tokens.removeFirst()
                val rhs = parseTightlyBindingFactor()
                BinaryOp(lhs, BinaryOps.Div, rhs)
            } else if (tokens[0]::class == Mod::class) {
                tokens.removeFirst()
                val rhs = parseTightlyBindingFactor()
                BinaryOp(lhs, BinaryOps.Mod, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseTightlyBindingFactor(): Expression {
        var lhs = parseBitwiseShift()
        while (true) {
            if (tokens[0]::class == Pow::class) {
                tokens.removeFirst()
                val rhs = parseBitwiseShift()
                lhs = BinaryOp(lhs, BinaryOps.Pow, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseBitwiseShift(): Expression {
        var lhs = parseBitwiseAnd()
        while (true) {
            lhs = if (tokens[0]::class == Bitwise_LShift::class) {
                tokens.removeFirst()
                val rhs = parseBitwiseAnd()
                BinaryOp(lhs, BinaryOps.Bitwise_LShift, rhs)
            } else if (tokens[0]::class == Bitwise_RShift::class) {
                tokens.removeFirst()
                val rhs = parseBitwiseAnd()
                BinaryOp(lhs, BinaryOps.Bitwise_RShift, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseBitwiseAnd(): Expression {
        var lhs = parseBitwiseXor()
        while (true) {
            if (tokens[0]::class == Bitwise_And::class) {
                tokens.removeFirst()
                val rhs = parseBitwiseXor()
                lhs = BinaryOp(lhs, BinaryOps.Bitwise_And, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseBitwiseXor(): Expression {
        var lhs = parseBitwiseOr()
        while (true) {
            if (tokens[0]::class == Bitwise_Xor::class) {
                tokens.removeFirst()
                val rhs = parseBitwiseOr()
                lhs = BinaryOp(lhs, BinaryOps.Bitwise_Xor, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseBitwiseOr(): Expression {
        var lhs = parseUnary()
        while (true) {
            if (tokens[0]::class == Bitwise_Or::class) {
                tokens.removeFirst()
                val rhs = parseUnary()
                lhs = BinaryOp(lhs, BinaryOps.Bitwise_Or, rhs)
            } else {
                return lhs
            }
        }
    }

    private fun parseUnary(): Expression {
        return if (tokens[0]::class == Bitwise_Not::class) {
            tokens.removeFirst()
            UnaryOp(UnaryOps.BNot, parseUnary())
        } else if (tokens[0]::class == Sub::class) {
            tokens.removeFirst()
            UnaryOp(UnaryOps.Invert, parseUnary())
        } else {
            parseCall()
        }
    }

    private fun parseCall(): Expression {
        if (tokens[0]::class == Ident::class) {
            val identifier = (tokens[0] as Ident).ident
            if (tokens[0]::class == LBracket::class) {
                val idx = parseExpression()
                if (tokens[0]::class != RBracket::class) {
                    throw ExpressionParserException("Line ${tokens[0].line} - Expected closing brace on list access.")
                } else {
                    tokens.removeFirst()
                    return ListAccess(identifier, idx)
                }
            } else if (tokens[0]::class == Dot::class) {
                val rhs = parseExpression()
                if (rhs::class != VariableAccess::class && rhs::class != ListAccess::class && rhs::class != Call::class) {
                    throw ExpressionParserException("Line ${tokens[0].line} - Invalid ScopeOf expression, expected variable, list, or function call.")
                }
                return ScopeOf(identifier, rhs)
            } else if (tokens[0]::class == LParen::class) {
                tokens.removeFirst()
                val params = mutableListOf<Expression>()
                while (true) {
                    if (tokens[0]::class == RParen::class) {
                        tokens.removeFirst()
                        break
                    } else if (tokens[0]::class == Comma::class) {
                        tokens.removeFirst()
                    } else if (tokens[0]::class == EndOfFile::class || tokens[0]::class == Semicolon::class) {
                        throw ExpressionParserException("Line ${tokens[0].line} - Invalid function call. Parameter list is not terminated.")
                    } else {
                        params.add(parseExpression())
                    }
                }
            } else {
                return VariableAccess(identifier)
            }
        }
        return parsePrimary()
    }

    private fun parsePrimary(): Expression {
        if (tokens[0]::class == EndOfFile::class) {
            throw ExpressionParserException("Line ${tokens[0].line} - Error in expression parsing code. Parser called parse_primary on empty list of tokens.")
        } else if (tokens[0]::class == Unimplemented::class) {
            throw ExpressionParserException("Line ${tokens[0].line} - Error, bad token provided. Got '${(tokens[0] as Unimplemented).c}'")
        } else if (tokens[0]::class == Num::class) {
            val num = tokens[0] as Num
            if (num.is_int) {
                tokens.removeFirst()
                return Integer(num.int)
            }
            tokens.removeFirst()
            return Floating(num.dbl)
        } else if (tokens[0]::class == String_::class) {
            tokens.removeFirst()
            return StringLit((tokens[0] as String_).str)
        } else if (tokens[0]::class == Char_::class) {
            tokens.removeFirst()
            return Char_((tokens[0] as Char_).c)
        } else if (tokens[0]::class == Ident::class) {
            val idt = tokens[0] as Ident
            when (idt.ident) {
                "true" -> {
                    tokens.removeFirst()
                    return Bool(true)
                }

                "false" -> {
                    tokens.removeFirst()
                    return Bool(false)
                }

                else -> {
                    throw ExpressionParserException("Line ${tokens[0].line} - Error, bad token provided. Got '${(tokens[0] as Ident).ident}, expecting bool.")
                    // Todo: Implement types from enums.
                }
            }
        } else if (tokens[0]::class == LParen::class) {
            tokens.removeFirst()
            val parenthetical = parseExpression()
            if (tokens[0]::class != RParen::class) {
                throw ExpressionParserException("Line ${tokens[0].line} - Error, unterminated parenthetical. Got '${(tokens[0] as Ident).ident}, expecting right paren ')'.")
            }
            tokens.removeFirst()
            return parenthetical
        } else {
            throw ExpressionParserException("Line ${tokens[0].line} - Error, unexpected token. Got '${tokens[0]::class}, expecting integer or float or double or bool or enum type or parenthetical.")
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