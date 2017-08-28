![data and codata](https://pbs.twimg.com/media/C4puwPsVUAAPPW5.jpg)

`stalactite` (twinned
with [`stalagmite`](https://github.com/fommil/stalagmite)) makes it
convenient to derive typeclasses for specific data types. The benefits
are:

- much faster compiles (never deeper than one `case class`)
- faster runtime (less object allocation)
- simpler implicit rules (less time fighting the compiler)
- cleaner compiler errors (know where an implicit is missing)

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

- type parameters (will use `implicit def`)
- `sealed trait` and `object`
- `extends AnyVal` (if the typeclass has a `def xmap` or [`scalaz.InvariantFunctor`](https://static.javadoc.io/org.scalaz/scalaz_2.12/7.2.15/scalaz/InvariantFunctor.html))

### Supported Derivations

The macro expansion only works if a semi-auto derivation method exists
for your typeclass. The following pattern of writing typeclasses is
strongly recommended:

```scala
@typeclass trait Foo[F[_]] {
  // put your methods here
}
object Foo {
  // put your default instances here
}
```

and in a separate file put your generic derivation, which is opt-in
and not automatically derived

```scala
@typeclass DerivedFoo[F[_]] extends Foo[F]
object DerivedFoo {
    def gen[T]: DerivedFoo[T] = ???
}
```

This pattern requires no further imports from users of your
typeclasses at their use point as `gen` can make use of the
`DerivedFoo` implicit scope when recursing.

You may choose to implement `gen` with your own macro (e.g. as
`play-json` does) or, recommended, with shapeless such as described
in [Shapeless for Mortals](http://fommil.com/scalax15/). A yasnippet
template for an encoder is available
at
[semiencoder](https://github.com/fommil/dotfiles/blob/master/.emacs.d/snippets/scala-mode/semiencoder).

We plan on simplifying the process of generic derivation in #4.

### Custom

We provide wirings for several popular libraries out-of-the-box in
=stalactite.conf= (and will accept Merge Requests to add more).

You can provide your own project-specific wirings by setting up your
build to point to your configuration file, e.g.

```scala
scalacOptions += s"-Xmacro-settings:stalactite.config=project/stalactite.conf"
```

The config file is plain text with one line per wiring, formatted:
`fqn.TypeClass=fqn.DerivedTypeClass.method`

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
