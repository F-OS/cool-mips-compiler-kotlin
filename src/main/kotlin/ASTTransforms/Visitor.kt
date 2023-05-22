package ASTTransforms

import AST.*

interface Visitor {
    fun visit(node: Lambda): Any
    fun visit(node: BinaryOp): Any
    fun visit(node: UnaryOp): Any
    fun visit(node: ListAccess): Any
    fun visit(node: VariableAccess): Any
    fun visit(node: Modify): Any
    fun visit(node: ScopeOf): Any
    fun visit(node: Call): Any
    fun visit(node: IntegerNode): Any
    fun visit(node: Floating): Any
    fun visit(node: Bool): Any
    fun visit(node: StringLit): Any
    fun visit(node: CharNode): Any
    fun visit(node: If): Any
    fun visit(node: For): Any
    fun visit(node: ForEach): Any
    fun visit(node: While): Any
    fun visit(node: DoWhile): Any
    fun visit(node: Continue): Any
    fun visit(node: Break): Any
    fun visit(node: Label): Any
    fun visit(node: Switch): Any
    fun visit(node: Assignment): Any
    fun visit(node: Return): Any
    fun visit(node: Goto): Any
    fun visit(node: Try): Any
    fun visit(node: Throw): Any
    fun visit(node: ExprStatement): Any
    fun visit(node: Block): Any
    fun visit(node: SimpleVarDeclaration): Any
    fun visit(node: ArrayDeclaration): Any
    fun visit(node: EnumDeclaration): Any
    fun visit(node: ClassDeclaration): Any
    fun visit(node: StructDeclaration): Any
    fun visit(node: FunctionDeclaration): Any
    fun visit(node: Ternary): Any
}