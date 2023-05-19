package bytecodeVM

import bytecodeVM.VMObjectType.*

sealed class VMInstruction {
    abstract fun execute(state: VMState)
}

data class VMIns_Push(val dataval: VMObject): VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(dataval)
    }
}

class VMIns_PushTrue: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(VMBoolean(true))
    }
}

class VMIns_PushFalse: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(VMBoolean(false))
    }
}

class VMIns_PushZero: VMInstruction() {
    override fun
            execute(state: VMState)
    {
        state.stack.push(VMInt(0))
    }
}

class VMIns_PushOne: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(VMInt(1))
    }
}

class VMIns_PushNOne: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(VMInt(-1))
    }
}

class VMIns_PushFZero: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(VMDouble(0.0))
    }
}

class VMIns_PushFOne: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(VMDouble(1.0))
    }
}

class VMIns_PushNull: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.push(VMNullObj())
    }
}

class VMIns_Pop: VMInstruction() {
    override fun execute(state: VMState)
    {
        state.stack.pop()
    }
}

class VMIns_Dup: VMInstruction() {
    override fun execute(state: VMState)
    {
        val obj: VMObject = state.stack.peek()
        state.stack.push(obj)
    }
}

class VMIns_Add: VMInstruction() {
    override fun execute(state: VMState)
    {
        val b: VMObject = state.stack.pop();
        val a: VMObject = state.stack.pop();
        val obj: VMObject = when(a.getType())
        {
            BOOLEAN -> {
                when(b.getType())
                {
                    BOOLEAN -> {
                        VMBoolean((a as VMBoolean).value or (b as VMBoolean).value)
                    }
                    STRING -> {
                        val res: String = if ((a as VMBoolean).value) "true" else "false" + (b as VMStringObj).value
                        VMStringObj(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }
            }
            INT -> {
                when(b.getType())
                {
                    INT -> {
                        val res: Int = (a as VMInt).value + (b as VMInt).value
                        VMInt(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMInt).value.toDouble() + (b as VMDouble).value
                        VMDouble(res)
                    }
                    ARRAY -> {
                        TODO("Implement array append.")
                    }
                    STRING -> {
                        val res: String = (a as VMInt).toString() + (b as VMStringObj).value
                        VMStringObj(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }
            }
            DOUBLE ->
            {
                when(b.getType())
                {
                    INT -> {
                        val res: Double = (a as VMDouble).value + (b as VMInt).value
                        VMDouble(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMDouble).value + (b as VMDouble).value
                        VMDouble(res)
                    }
                    ARRAY -> {
                        TODO("Implement array append.")
                    }
                    STRING -> {
                        val res: String = (a as VMStringObj).value + (b as VMInt).toString()
                        VMStringObj(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }

            }
            ARRAY -> {
                TODO("Implement array append.")
            }
            else -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }

        }
        state.stack.push(obj)
    }
}

class VMIns_Sub: VMInstruction() {
    override fun execute(state: VMState)
    {
        val b: VMObject = state.stack.pop();
        val a: VMObject = state.stack.pop();
        val obj: VMObject = when(a.getType())
        {
            BOOLEAN -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }
            INT -> {
                when(b.getType())
                {
                    INT -> {
                        val res: Int = (a as VMInt).value - (b as VMInt).value
                        VMInt(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMInt).value.toDouble() - (b as VMDouble).value
                        VMDouble(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }
            }
            DOUBLE ->
            {
                when(b.getType())
                {
                    INT -> {
                        val res: Double = (a as VMDouble).value - (b as VMInt).value
                        VMDouble(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMDouble).value - (b as VMDouble).value
                        VMDouble(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }

            }
            else -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }

        }
        state.stack.push(obj)
    }
}

class VMIns_Mul: VMInstruction() {
    override fun execute(state: VMState)
    {
        val b: VMObject = state.stack.pop();
        val a: VMObject = state.stack.pop();
        val obj: VMObject = when(a.getType())
        {
            BOOLEAN -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }
            INT -> {
                when(b.getType())
                {
                    INT -> {
                        val res: Int = (a as VMInt).value * (b as VMInt).value
                        VMInt(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMInt).value.toDouble() * (b as VMDouble).value
                        VMDouble(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }
            }
            DOUBLE ->
            {
                when(b.getType())
                {
                    INT -> {
                        val res: Double = (a as VMDouble).value * (b as VMInt).value
                        VMDouble(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMDouble).value * (b as VMDouble).value
                        VMDouble(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }

            }
            else -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }

        }
        state.stack.push(obj)
    }
}

class VMIns_Div: VMInstruction() {
    override fun execute(state: VMState)
    {
        val b: VMObject = state.stack.pop();
        val a: VMObject = state.stack.pop();
        val obj: VMObject = when(a.getType())
        {
            BOOLEAN -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }
            INT -> {
                when(b.getType())
                {
                    INT -> {
                        val res: Int = (a as VMInt).value / (b as VMInt).value
                        VMInt(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMInt).value.toDouble() / (b as VMDouble).value
                        VMDouble(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }
            }
            DOUBLE ->
            {
                when(b.getType())
                {
                    INT -> {
                        val res: Double = (a as VMDouble).value / (b as VMInt).value
                        VMDouble(res)
                    }
                    DOUBLE -> {
                        val res: Double = (a as VMDouble).value / (b as VMDouble).value
                        VMDouble(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }

            }
            else -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }

        }
        state.stack.push(obj)
    }
}

class VMIns_Mod: VMInstruction() {
    override fun execute(state: VMState)
    {
        val b: VMObject = state.stack.pop();
        val a: VMObject = state.stack.pop();
        val obj: VMObject = when(a.getType())
        {
            BOOLEAN -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }
            INT -> {
                when(b.getType())
                {
                    INT -> {
                        val res: Int = (a as VMInt).value.mod((b as VMInt).value)
                        VMInt(res)
                    }
                    else ->
                    {
                        throw IllegalArgumentException("Attempt to add incompatible types.")
                    }
                }
            }
            else -> {
                throw IllegalArgumentException("Attempt to add incompatible types.")
            }

        }
        state.stack.push(obj)
    }
}