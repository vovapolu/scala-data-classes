package testing.meta

import fommil.stalagmite.data

import _root_.scala.{ Boolean, Int, Option }
import _root_.java.lang.String

@data(
  product = true,
  checkSerializable = false
)
class FooMeta[+T](a: Boolean, s: String, t: T, i: Int = 0)

@data(
  memoise = true,
  memoiseRefs = Seq('s),
  memoiseHashCode = true,
  memoiseToString = true,
  memoiseStrong = true,
  memoiseEqualsByValue = false
)
class FooMetaMemoised(a: Boolean, s: String)

@data(
  memoise = true,
  memoiseRefs = Seq('s),
  memoiseStrong = true,
  memoiseEqualsByValue = false
)
class FooMetaInternMemoised(a: Boolean, s: String)

@data(
  memoise = true,
  memoiseRefs = Seq('s),
  memoiseHashCode = true,
  memoiseToString = true,
  memoiseEqualsByValue = false
)
class FooMetaMemoisedWeak(a: Boolean, s: String)

@data(
  optimiseHeapOptions = true,
  optimiseHeapBooleans = true,
  optimiseHeapStrings = true,
  product = true
)
class FooMetaOptimiseHeap(a: Option[Boolean],
                          b: Option[Boolean],
                          s: Option[String])
