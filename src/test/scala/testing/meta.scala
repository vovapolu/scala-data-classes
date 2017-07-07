package testing.meta

import fommil.stalagmite.data

import _root_.scala._
import _root_.scala.Predef._

@data(product = true, serializable = true, shapeless = true, memoiseRefs = Seq('a, 's))
class FooMeta[+T](a: Boolean, s: String, t: T, i: Int = 0)