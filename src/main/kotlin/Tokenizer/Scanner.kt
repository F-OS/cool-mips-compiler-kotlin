package Tokenizer

val escapeSequences = mapOf(
    'n' to '\n',
    'r' to '\r',
    't' to '\t',
    'b' to '\b',
    '\\' to '\\',
    '\"' to '\"'
)

val stringLitRegex = "\"(\\\\.|[^\"])*\"".toRegex()
val prefixes: Array<String> = arrayOf(
    "<<=", ">>=", "**=", "**", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
    "++", "--", ">>", "<<", "||", "&&", "<=", ">=", "==", "!=", "^", "~", "|", "&", "!", "+", "-",
    "*", "?", ":", "/", "%", ".", "<", ">", "=", ",", ";", "}", "{", "]", "[", "(", ")"
)

fun scanToken(str: String): List<Token> {
    val tokens: ArrayList<Token> = ArrayList(10000)
    var strIdx = 0
    var line = 1
    val strLen = str.length
    while (strIdx < strLen) {
        while (str[strIdx].isWhitespace() || str[strIdx] == '\n' || str[strIdx] == '\r') {
            line += if (str[strIdx] == '\n' || str[strIdx] == '\r') 1 else 0
            strIdx++
            if (strIdx >= strLen) {
                tokens.add(EndOfFile(line))
                return tokens
            }
        }

        if (str.startsWith("//", strIdx)) {
            strIdx += 2
            while (strIdx < strLen && str[strIdx] != '\n' && str[strIdx] != '\r') {
                strIdx++
            }
            continue
        }


        val matchedPrefix = prefixes.find { str.startsWith(it, strIdx, ignoreCase = true) }
        if (matchedPrefix != null) {
            strIdx += matchedPrefix.length
            tokens.add(
                when (matchedPrefix) {
                    "<<=" -> LShiftAssign(line)
                    ">>=" -> RShiftAssign(line)
                    "**=" -> PowAssign(line)
                    "<<" -> Bitwise_LShift(line)
                    ">>" -> Bitwise_RShift(line)
                    "**" -> Pow(line)
                    "+=" -> AddAssign(line)
                    "-=" -> SubAssign(line)
                    "*=" -> MulAssign(line)
                    "/=" -> DivAssign(line)
                    "%=" -> ModAssign(line)
                    "&=" -> AndAssign(line)
                    "|=" -> OrAssign(line)
                    "^=" -> XorAssign(line)
                    "++" -> Inc(line)
                    "--" -> Dec(line)
                    "||" -> Or(line)
                    "&&" -> And(line)
                    "<=" -> LessEqual(line)
                    ">=" -> GreaterEqual(line)
                    "==" -> EqualTo(line)
                    "!=" -> NotEqualTo(line)
                    "<" -> LessThan(line)
                    ">" -> GreaterThan(line)
                    "^" -> Bitwise_Xor(line)
                    "~" -> Bitwise_Not(line)
                    "|" -> Bitwise_Or(line)
                    "&" -> Bitwise_And(line)
                    "!" -> Not(line)
                    "+" -> Add(line)
                    "-" -> Sub(line)
                    "*" -> Mul(line)
                    "?" -> QMark(line)
                    ":" -> Colon(line)
                    "/" -> Div(line)
                    "%" -> Mod(line)
                    "." -> Dot(line)
                    "=" -> Assign(line)
                    "," -> Comma(line)
                    ";" -> Semicolon(line)
                    "}" -> RBrace(line)
                    "{" -> LBrace(line)
                    "]" -> RBracket(line)
                    "[" -> LBracket(line)
                    "(" -> LParen(line)
                    ")" -> RParen(line)
                    else -> throw IllegalArgumentException("Prefix in table prefixes but not in when block in scanner.")
                }
            )
        } else if (str.startsWith("\'", strIdx, ignoreCase = true)) {
            strIdx += 1
            if (str[strIdx] == '\\') {
                strIdx += 2
                val char = escapeSequences[str[strIdx]] ?: str[strIdx]
                tokens.add(CharTok(char, line))
            } else {
                tokens.add(CharTok(str[strIdx], line))
                strIdx += 1
            }
            strIdx += 1
        } else if (str[strIdx].isLetter() || str[strIdx] == '_') {
            var span = 0
            for (c in str.substring(strIdx)) {
                if (c.isLetter() || c == '_') {
                    span++
                } else {
                    break
                }
            }
            val ident = str.substring(strIdx, strIdx + span)
            strIdx += span
            tokens.add(Ident(ident, line))
        } else if (Character.digit(str[strIdx], 16) != -1) {
            var span = 0
            for (c in str.substring(strIdx)) {
                if (Character.digit(str[strIdx + span], 16) != -1 || c == '.') {
                    span++
                } else {
                    break
                }
            }
            val num = str.substring(strIdx, strIdx + span)
            strIdx += span
            val isint = !num.contains(".")
            if (isint) {
                tokens.add(Num(true, Integer.decode(num).toLong(), 0.0, line))
            } else {
                tokens.add(Num(false, 0, num.toDouble(), line))
            }
        } else if (str.startsWith("\"", strIdx, ignoreCase = true)) {
            val matchResult: String = stringLitRegex.find(str.substring(strIdx))?.value ?: ""
            strIdx += matchResult.length

            val escapedLiteral = StringBuilder()
            var i = 0
            while (i < matchResult.length) {
                val currentChar = matchResult[i]
                if (currentChar == '\"') {
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
            tokens.add(StringTok(escapedLiteral.toString(), line))
        } else {
            tokens.add(Unimplemented(str[strIdx++], line))
        }
    }
    tokens.add(EndOfFile(line))
    return tokens
}