package stdLib

class Stack<T>(private val stackSize: Int) {
    private val contents = arrayOfNulls<Any?>(stackSize)
    private var stackLoc = 0

    fun push(item: T) {
        if (isFull()) {
            throw IllegalStateException("Stack is full")
        }
        contents[stackLoc++] = item
    }

    fun pop(): T {
        if (isEmpty()) {
            throw NoSuchElementException("Stack is empty")
        }
        @Suppress("UNCHECKED_CAST")
        return contents[--stackLoc] as T
    }

    fun isEmpty(): Boolean {
        return stackLoc == 0
    }

    fun isFull(): Boolean {
        return stackLoc == stackSize
    }

    fun peek(): T {
        if (isEmpty()) {
            throw NoSuchElementException("Stack is empty")
        }
        @Suppress("UNCHECKED_CAST")
        return contents[stackLoc - 1] as T
    }
}