import ParserExcptions.ParserException
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import kotlin.system.exitProcess

fun main(args: Array<String>) {
	if (args.count() > 1) {
		println("Usage: interpreter [script]")
		exitProcess(64)
	} else if (args.count() == 1) {
		runFile(args[0])
	} else {
		runPrompt()
	}
}

fun runPrompt() {
	while (true) {
		print("> ")
		val line: String? = readLine()
		if (line != null && line != "") {
			runcode(line)
		} else {
			break
		}
	}
}

fun runFile(path: String) {
	try {
		val lines = Files.readAllLines(Paths.get(path))
		runcode(lines.joinToString("\n"))
	} catch (e: IOException) {
		println("The file $path is not valid or you do not have correct permissions.")
	}
}

fun runcode(lines: String) {
	val tokens: List<Token> = scanToken(lines)
	println(tokens)
	val parser: Parser = Parser(tokens as MutableList<Token>)
	try {
		val tree = parser.parseDeclaration()
		println(tree)
	} catch (p: ParserException) {
		println(p.s)
	}
}
