package bytecodeVM

import stdLib.Stack

const val STACKSIZE = 1024 * 64
data class VMState(var parent: VMState? = null) {
    var stack = Stack<VMObject>(STACKSIZE)
    var children: MutableList<VMState> = arrayListOf()
}