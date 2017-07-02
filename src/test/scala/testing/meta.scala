package testing.meta

import fommil.data.impl.data

@data(product = true, serializable = true, shapeless = true)
class FooMetaCaseClass[+T](a: Boolean, s: String, t: T, i: Int = 0)