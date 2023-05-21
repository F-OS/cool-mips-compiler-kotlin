import AST.Declaration
import ASTTransforms.ASTPrinter
import Parser.CodeParser
import Parser.ParserException
import Tokenizer.Token
import Tokenizer.scanToken
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
		val line: String? = readlnOrNull()
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
	val codeParser: CodeParser = CodeParser(tokens as MutableList<Token>)
	var hadError = true
	var errorNum = 0
	var tree: List<Declaration> = listOf()
	do {
		try {
			hadError = false
			tree = tree + codeParser.parseProgram()
			println(tree)
		} catch (p: ParserException) {
			hadError = true
			errorNum++
			println(p.s)
			tree = codeParser.synchronize() + tree
		}
	} while (hadError)
	if (hadError) {
		println("Had ${errorNum} error${if (errorNum == 1) "" else "s"}.")
		return
	}
	val astPrint = ASTPrinter()
	astPrint.visitTree(tree)

}
