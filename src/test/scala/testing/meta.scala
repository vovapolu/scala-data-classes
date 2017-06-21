package testing.meta

import fommil.data.impl.data

@data(product = true, checkSerializable = false)
class FooMetaCaseClass[+T](a: Boolean, s: String, t: T, i: Int = 0)