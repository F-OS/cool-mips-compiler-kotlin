import AST.Declaration
import ASTTransforms.TreeTyper.TypeGenerator
import Parser.CodeParser
import Parser.ParserException
import Tokenizer.Token
import Tokenizer.scanToken
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import kotlin.system.exitProcess

class Compiler {
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            if (args.count() > 1) {
                println("Usage: interpreter [script]")
                exitProcess(64)
            } else if (args.count() == 1) {
                Compiler().runFile(args[0])
            } else {
                Compiler().runPrompt()
            }
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
        var hadError: Boolean = false
        var errorNum = 0
        var tree: List<Declaration> = listOf()
        do {
            try {
                tree = tree + codeParser.parseProgram()
            } catch (p: ParserException) {
                hadError = true
                errorNum++
                println(p.s)
                tree = codeParser.synchronize() + tree
            }
        } while (codeParser.hasTokens())
        if (hadError) {
            println("Had ${errorNum} error${if (errorNum == 1) "" else "s"}.")
        }
        val symbolVerify = TypeGenerator()
        val typedTree = symbolVerify.visitTree(tree)
        if (symbolVerify.errors != 0) {
            println("Had ${symbolVerify.errors} error${if (symbolVerify.errors == 1) "" else "s"}.")
            return
        }

    }
}