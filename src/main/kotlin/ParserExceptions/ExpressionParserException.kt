package ParserExceptions

open class ParserException(var s: String) : Throwable()
class ExpressionParserException(s: String) : ParserException(s)
class StatementParserException(s: String) : ParserException(s)
class DeclarationParserException(s: String) : ParserException(s)
