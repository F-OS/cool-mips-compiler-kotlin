package bytecodeVM

import objects.*

sealed class VMObject {
    abstract fun getType(): VMObjectType
}

enum class VMObjectType {
    BOOLEAN, INT, DOUBLE, ARRAY, CLASS, CLOSURE, FUNCTION, STRING, NULL
}

data class VMBoolean(val value: Boolean) : VMObject() {
    override fun getType() = VMObjectType.BOOLEAN
}

data class VMInt(val value: Int) : VMObject() {
    override fun getType() = VMObjectType.INT
}

data class VMDouble(val value: Double) : VMObject() {
    override fun getType() = VMObjectType.DOUBLE
}

data class VMStringObj(val value: String) : VMObject() {
    override fun getType() = VMObjectType.STRING
}

data class VMArrayObj(val value: ObjArray) : VMObject() {
    override fun getType() = VMObjectType.ARRAY
}

data class VMClassObj(val value: ObjClass) : VMObject() {
    override fun getType() = VMObjectType.CLASS
}

data class VMClosureObj(val value: ObjClosure) : VMObject() {
    override fun getType() = VMObjectType.CLOSURE
}

data class VMNullObj(val nothing: Any? = null) : VMObject() {
    override fun getType() = VMObjectType.NULL
}

