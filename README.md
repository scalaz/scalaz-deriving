![data and codata](https://pbs.twimg.com/media/C4puwPsVUAAPPW5.jpg)

`stalactite` (twinned
with [`stalagmite`](https://github.com/fommil/stalagmite)) provides
something like Haskell's `deriving` for Scala:

```scala
import stalactite._

@deriving(FooFormat)
case class Bar(s: String, b: Boolean)
```

expanding to

```scala
case class Bar(s: String, b: Boolean)
object Bar {
  implicit val fooFormat: FooFormat[Bar] = DerivedFooFormat.gen
}
```

for a `FooFormat` that follows the [export-hook](https://github.com/milessabin/export-hook#the-type-class-provider) pattern.

Also supports (if your generic typeclass derivation supports it):

- type parameters (`implicit def`)
- `sealed trait`
- database of popular wirings (see `deriving.scala` for built-ins)
- user-defined wirings (see the `build.sbt` for an example)

The preferred way to use this library is to add (semi-auto) derived typeclasses to all `case class` and `sealed trait`, rather than relying on complex (and slow) full generic derivation.

## Installation

### Maven Central

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.fommil/stalactite_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.fommil/stalactite_2.12)

```scala
libraryDependencies += "com.fommil" %% "stalactite" % "<version>"
```

### Snapshots

```scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.fommil" %% "stalactite" % "<version + 0.0.1>-SNAPSHOT"
```
