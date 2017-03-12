`@data` classes a la carte in Scala.

**STATUS: currently specifying, not implemented yet. If you're a student, please see the [Scala Google Summer of Code](https://github.com/scala/scala-lang/blob/master/gsoc/2017.md#case-classes-a-la-carte-with-scalameta) page and apply!**

An alternative to `case class` giving much more control over the internal representation of a data class.

The following features are currently being considered in [`src/test/scala/testing`](https://github.com/fommil/scala-data-classes/tree/master/src/test/scala/testing). If you have any further ideas, please comment on the issue tracker:

## `final case class` parity

```scala
@data(product = true, checkSerializable = false /*, companionExtends = true */)
class Foo[+T](a: Boolean, s: String, t: T, i: Int = 0)
```

should give feature parity with

```scala
final case class Foo[+T] private (a: Boolean, s: String, t: T, i: Int = 0)
```

User-defined methods and fields are being debated in [#5](https://github.com/fommil/scala-data-classes/issues/5)

- `product` (i.e. implementing `Product`) will be disabled by default because it encourages runtime inspection instead of compiletime safety.
- `checkSerializable` (i.e. checking all parameters for `Serializable`) will be enabled by default because this is the sort of thing that should be checked at compiletime.
- `companionExtends` disabled by default and not possible for non-parametric classes, makes the companion extend a function. [Has binary compatibility consequences](https://issues.scala-lang.org/browse/SI-3664).

Implicit instances of `shapeless.{Generic, LabelledGeneric, Typeable}` are generated on the companion. This saves shapeless from having to derive one at every call site, speeding up downstream compiles.

## Memoisation

```scala
@data(memoise = true, memoiseStrings = true, memoiseHashCode = true, memoiseToString = true)
class Foo(a: Boolean, s: String)
```

The following features are independent, but can be combined:

- `memoise` uses a weak reference cache such that reference and value equality are equivalent (at the cost of GC pressure)
- `memoiseStrings` uses a weak reference cache for `String` fields (at the cost of GC pressure)
- `memoiseHashCode` stores the `hashCode` in a `val` (at the cost of heap)
- `memoiseToString` stores the `toString` in a `val` (at the cost of heap)

Further ideas for memoisation should go in [#6](https://github.com/fommil/scala-data-classes/issues/6)

## Optimise Heap

```scala
@data(optimiseHeapOptions = true, optimiseHeapStrings = true)
class Foo(a: Option[Boolean], b: Option[Boolean], s: Option[String])
```

- `optimiseHeapOptions` stores `Option` `AnyRef`s as `null`-able fields and a (shared) bitmask for `AnyVal`s. Does not allow `null` or `Some(null)`.
- `optimiseHeapBooleans` re-uses the (shared) bitmask for bit packing of `Boolean` parameters
- `optimiseHeapStrings` unwraps `String` as `Array[Char]`, saving 64 bits per `String` field per instance (at the cost of object churn and `String.{hashCode,equals}` performance)

For this example: 3 `Option` wrappers, `Boolean` boxing, `Boolean` packing, `String` wrapping, we save 6 references (384 bits) per instance.

Note that changing the heap representation does not affect the serialised form (the publicly fields are used).

Further ideas for heap optimisation should go in [#3](https://github.com/fommil/scala-data-classes/issues/3)
