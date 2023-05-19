val escapeSequences = mapOf(
    'n' to '\n',
    'r' to '\r',
    't' to '\t',
    'b' to '\b',
    '\\' to '\\',
    '\"' to '\"'
)
val StringLitRegex = "\"(\\\\.|[^\"])*\"".toRegex()
fun scanToken(str: String): List<Token> {
    val tokens = mutableListOf<Token>()
    var stridx = 0
    var line = 0;
    val strlen = str.length
    while (stridx < strlen) {
        if (str.startsWith("<<=", stridx, ignoreCase = true)) {
            stridx += 3
            tokens.add(LShiftAssign(line))
        } else if (str.startsWith(">>=", stridx, ignoreCase = true)) {
            stridx += 3
            tokens.add(RShiftAssign(line))
        } else if (str.startsWith("**=", stridx, ignoreCase = true)) {
            stridx += 3
            tokens.add(PowAssign(line))
        } else if (str.startsWith("**", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(Pow(line))
        } else if (str.startsWith("+=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(AddAssign(line))
        } else if (str.startsWith("-=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(SubAssign(line))
        } else if (str.startsWith("*=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(MulAssign(line))
        } else if (str.startsWith("/=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(DivAssign(line))
        } else if (str.startsWith("%=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(ModAssign(line))
        } else if (str.startsWith("&=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(AndAssign(line))
        } else if (str.startsWith("|=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(OrAssign(line))
        } else if (str.startsWith("^=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(XorAssign(line))
        } else if (str.startsWith("++", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(Inc(line))
        } else if (str.startsWith("--", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(Dec(line))
        } else if (str.startsWith(">>", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(Bitwise_RShift(line))
        } else if (str.startsWith("<<", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(Bitwise_LShift(line))
        } else if (str.startsWith("||", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(Or(line))
        } else if (str.startsWith("&&", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(And(line))
        } else if (str.startsWith("<", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(LessThan(line))
        } else if (str.startsWith("<=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(LessEqual(line))
        } else if (str.startsWith(">", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(GreaterThan(line))
        } else if (str.startsWith(">=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(GreaterEqual(line))
        } else if (str.startsWith("==", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(EqualTo(line))
        } else if (str.startsWith("!=", stridx, ignoreCase = true)) {
            stridx += 2
            tokens.add(NotEqualTo(line))
        } else if (str.startsWith("^", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Bitwise_Xor(line))
        } else if (str.startsWith("~", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Bitwise_Not(line))
        } else if (str.startsWith("|", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Bitwise_Or(line))
        } else if (str.startsWith("&", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Bitwise_And(line))
        } else if (str.startsWith("!", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Not(line))
        } else if (str.startsWith("+", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Add(line))
        } else if (str.startsWith("-", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Sub(line))
        } else if (str.startsWith("*", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Mul(line))
        } else if (str.startsWith("?", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(QMark(line))
        } else if (str.startsWith(":", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Colon(line))
        } else if (str.startsWith("/", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Div(line))
        } else if (str.startsWith("%", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Mod(line))
        } else if (str.startsWith(".", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Dot(line))
        } else if (str.startsWith("=", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Assign(line))
        } else if (str.startsWith(",", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Comma(line))
        } else if (str.startsWith(";", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(Semicolon(line))
        } else if (str.startsWith("}", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(RBrace(line))
        } else if (str.startsWith("{", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(LBrace(line))
        } else if (str.startsWith("]", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(RBracket(line))
        } else if (str.startsWith("[", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(LBracket(line))
        } else if (str.startsWith("(", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(LParen(line))
        } else if (str.startsWith(")", stridx, ignoreCase = true)) {
            stridx += 1
            tokens.add(RParen(line))
        } else if (str.startsWith("\'", stridx, ignoreCase = true)) {
            stridx += 1
            if (str[stridx] != '\\') {
                stridx += 1
                val char = escapeSequences[str[stridx]] ?: str[stridx]
                tokens.add(Char_(char, line))
            } else {
                tokens.add(Char_(str[stridx], line))
                stridx += 1
            }
        } else if (str[stridx].isDigit()) {
            var span = 0
            for (c in str.substring(stridx)) {
                if (c.isDigit() || c == '.') {
                    span++
                } else {
                    break
                }
            }
            val num = str.substring(stridx, stridx + span)
            stridx += span
            val isint = !num.contains(".")
            if (isint) {
                tokens.add(Num(true, num.toLong(), 0.0, line))
            } else {
                tokens.add(Num(false, 0, num.toDouble(), line))
            }
        } else if (str[stridx].isLetter() || str[stridx] == '_') {
            var span = 0
            for (c in str.substring(stridx)) {
                if (c.isLetter() || c == '_') {
                    span++
                } else {
                    break
                }
            }
            val ident = str.substring(stridx, stridx + span).lowercase()
            stridx += span
            tokens.add(Ident(ident, line))
        } else if (str.startsWith("\"", stridx, ignoreCase = true)) {
            val matchResult: String = StringLitRegex.find(str)?.value ?: ""
            stridx += matchResult.length

            val escapedLiteral = StringBuilder()

            var i = 0
            while (i < matchResult.length) {
                val currentChar = matchResult[i]
                if(currentChar == '\"')
                {
                    i++
                    continue
                }
                if (currentChar == '\\') {
                    escapedLiteral.append(escapeSequences[matchResult[i + 1]] ?: matchResult[i + 1])
                    i += 2
                    continue
                }
                escapedLiteral.append(currentChar)
                i++
            }
            tokens.add(String_(escapedLiteral.toString(), line))
        } else if (str[stridx].isWhitespace()) {
            line += if (str[stridx] == '\n' || str[stridx] == '\r') 1 else 0
            stridx++
        } else {
            tokens.add(Unimplemented(str[stridx++], line))
        }
    }
    tokens.add(EndOfFile(line))
    return tokens
}