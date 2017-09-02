![data and codata](https://pbs.twimg.com/media/C4puwPsVUAAPPW5.jpg)

An alternative to `case class` and extensions for `sealed trait` giving much more control over the internal representation of ADTs for [Functional Programming in Scala](https://leanpub.com/fp-scala-mortals) style.

This project is twinned with [stalactite](https://gitlab.com/fommil/stalactite) (which provides performance optimisations and cleaner syntax for typeclass derivation).

This project was sponsored by the [Scala Google Summer of Code](https://github.com/scala/scala-lang/blob/master/gsoc/2017.md#case-classes-a-la-carte-with-scalameta). A [blog post](https://vovapolu.github.io/scala/stalagmite/perf/2017/09/02/stalagmite-performance.html) was written at the end of the programme by Vovapolu with a summary of the initial achievements.

## Installation

### Maven Central

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.fommil/stalagmite_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.fommil/stalagmite_2.12)

```scala
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
libraryDependencies += "com.fommil" %% "stalagmite" % "<version>"
```

### Snapshots

```scala
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "com.fommil" %% "stalagmite" % "<version + 0.0.1>-SNAPSHOT"
```

## Limitations

Stalagmite is built using the next-generation Scala macro system [`scala.meta`](https://github.com/scalameta/paradise).

Unfortunately `scala.meta` is not yet supported in IntelliJ, Scala IDE, ENSIME, Scaladocs nor the REPL. We await the acceptance of the Scala Center proposal [Production Ready Scala Macros](https://github.com/scalacenter/advisoryboard/pull/30).

## Features

The following features are currently being considered in [`src/test/scala/testing`](https://github.com/fommil/stalagmite/tree/master/src/test/scala/testing). If you have any further ideas, please comment on the issue tracker:

### `final case class` parity

```scala
@data(product = true, checkSerializable = false /*, companionExtends = true */)
class Foo[+T](a: Boolean, s: String, t: T, i: Int = 0)
```

should give feature parity with

```scala
final case class Foo[+T] private (a: Boolean, s: String, t: T, i: Int = 0)
```

Extending `trait` / `interface` is allowed, but extending `class` / `abstract class` is forbidden.

User-defined methods and fields are being debated in [#5](https://github.com/fommil/stalagmite/issues/5).

- `product` (i.e. implementing `Product`) will be disabled by default because it encourages runtime inspection instead of compiletime safety.
- `checkSerializable` (i.e. checking all parameters for `Serializable`) will be enabled by default because this is the sort of thing that should be checked at compiletime.
- `companionExtends` disabled by default and not possible for non-parametric classes, makes the companion extend a function. [Has binary compatibility consequences](https://issues.scala-lang.org/browse/SI-3664).

Implicit instances of `shapeless.{Generic, LabelledGeneric, Typeable}` are generated on the companion. This saves shapeless from having to derive one at every call site, speeding up downstream compiles.

### Memoisation

Memoisation uses best endeavours to re-use existing instances instead of creating new ones.

High levels of memoisation in a JVM mean that the overall heap size for a `class` can be **dramatically** reduced in some cases, but at the cost of extra CPU cycles during construction and GC pressure. Also, the incidence of instance-based equality hits go up (so `equals` can get faster!).

```scala
@data(memoise = true, memoiseRefs = Seq('s), memoiseHashCode = true, memoiseToString = true, memoiseStrong = true)
class Foo(a: Boolean, s: String)
```

The following features are independent, but can be combined:

- `memoise` uses an interner cache to reduce duplication on the heap (at the cost of lookup and GC pressure)
- `memoiseRefs` (takes `Seq[Symbol]`) uses a memoisation cache for the selected `AnyRef` fields (at the cost of lookup and GC pressure)
- `memoizeStringsIntern` false by default, special-cases `String` fields to use the JVM's `intern` (trumps `memoiseRefs`)
- `memoiseHashCode` stores the `hashCode` in a `val`, and uses this as a shortcut in `equals` (at the cost of initialisation and heap)
- `memoiseToString` stores the `toString` in a `val` (at the cost of initialisation and heap)
- `memoiseStrong` weak by default, means your instances are never garbage collected and `equals` is identity based (extreme caution!)

Further ideas for memoisation should go in [#6](https://github.com/fommil/stalagmite/issues/6)

See [#8](https://github.com/fommil/stalagmite/issues/8) for a way of speeding up `equals` for instances with value equality (but not reference equality), at the cost of even more heap.

We are using Guava's `{Weak,String}Interner` to implement memoisation, but what we want a `SoftReference` interner that uses a custom `Equality`.

### Optimise Heap

```scala
@data(optimiseHeapOptions = true, optimiseHeapStrings = true)
class Foo(a: Option[Boolean], b: Option[Boolean], s: Option[String])
```

- `optimiseHeapOptions` stores `Option` `AnyRef`s as `null`-able fields and a (shared) bitmask for `AnyVal`s. Does not allow `null` or `Some(null)`.
- `optimiseHeapBooleans` re-uses the (shared) bitmask for bit packing of `Boolean` parameters
- `optimiseHeapStrings` unwraps `String` as `Array[Char]`, saving 64 bits per `String` field per instance (at the cost of object churn and `String.{hashCode,equals}` performance)

For this example: 3 `Option` wrappers, `Boolean` boxing, `Boolean` packing, `String` wrapping, we save 6 references (384 bits) per instance.

Note that changing the heap representation does not affect the serialised form (the public visible fields are used).

Further ideas for heap optimisation should go in [#3](https://github.com/fommil/stalagmite/issues/3)
