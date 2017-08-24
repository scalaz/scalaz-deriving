![data and codata](https://pbs.twimg.com/media/C4puwPsVUAAPPW5.jpg)

`stalactite` (twinned
with [`stalagmite`](https://github.com/fommil/stalagmite)) makes it convenient to derive typeclasses written in the [export-hook](https://github.com/milessabin/export-hook#the-type-class-provider) style.

## Benefits

- faster compiles (never deeper than one `case class`)
- faster runtime (less object allocation)
- cleaner compiler errors (know *exactly* what is missing)
- simpler implicit rules (less time fighting the compiler)

## Usage

```scala
import stalactite._

@deriving(Encoder, Decoder)
case class Bar(s: String, b: Boolean)
```

expanding to

```scala
case class Bar(s: String, b: Boolean)
object Bar {
  implicit val encoder: Encoder[Bar] = DerivedEncoder.gen
  implicit val decoder: Decoder[Bar] = DerivedDecoder.gen
}
```

Also supports:

- type parameters (`implicit def`)
- `sealed trait` and `object`
- database of popular wirings (see `deriving.scala` for built-ins)
- user-defined wirings (see the `build.sbt` for an example)

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
