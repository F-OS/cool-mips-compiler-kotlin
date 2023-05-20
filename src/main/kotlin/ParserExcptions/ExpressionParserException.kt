package ParserExcptions

sealed class ParserException(var s: String) : Throwable()
class ExpressionParserException(s: String) : ParserException(s) {

}

class StatementParserException(s: String) : ParserException(s) {

}
