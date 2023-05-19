package AST

sealed class Statement {
}

class If(val conditional: Expression, val consequent: Block, val has_alternate: Boolean, val alternate: Block) : Statement()
{

}

class For(val loopident: String, val has_init: Boolean,
          val initalizer: Statement, val has_condition: Boolean,
          val conditional: Expression, val has_iteration: Boolean,
          val iteration: Statement, val body: Block) : Statement()
{

}

class ForEach(val loopident: String, val iterval: String, val collectionvar: String, val body: Block) : Statement()
{

}

class While(val loopident: String, val has_conditional: Boolean, val conditional: Expression, val body: Block) : Statement()
{

}

class DoWhile(val loopident: String, val has_conditional: Boolean, val conditional: Expression, val body: Block) : Statement()
{

}

class Continue(val ident: String) : Statement()
{

}

class Break(val ident: String) : Statement()
{

}

class Label(val ident: String) : Statement()
{

}

class Switch(val switchon: Expression, val cases: List<Pair<Expression, Block>>) : Statement()
{

}

class Assignment(val ident: String, val expr: Expression) : Statement()
{

}

class Return(val has_value: Boolean, val expr: Expression) : Statement()
{

}

class Goto(val gotoident: String) : Statement()
{

}

class Try(val block: Block, val catches: String, val catchesAs: String, val catch: Block) : Statement()
{

}

class Throw(val ident: String, val attached_value: Boolean, val expr: Expression) : Statement()
{

}

class Block(val statements: List<Declaration>) : Statement()
{

}

