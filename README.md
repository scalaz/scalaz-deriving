`scalaz-deriving` makes it convenient to derive typeclasses for specific data
types. The benefits are:

- much faster compiles (never deeper than one `case class`)
- faster runtime (less object allocation)
- simpler implicit rules (less time fighting the compiler)
- cleaner compiler errors (know where an implicit is missing)

There are two parts to this library:

- a `@deriving` macro for removing the boilerplate from semi-auto typeclass
  derivation with [shapeless](http://fommil.com/scalax15/),
  [magnolia](http://magnolia.work/) and hand-rolled macros.
- `scalaz-deriving` an experimental and principled way for typeclass authors to
  define generic typeclasses with a clean API and no macro magic.

# Deriving Macro

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
- `extends AnyVal` (if the typeclass `TC[A]` has a `def xmap[B](f: A => B, g: B => A): TC[B]` or [`scalaz.InvariantFunctor`](https://static.javadoc.io/org.scalaz/scalaz_2.12/7.2.15/scalaz/InvariantFunctor.html))

### Standard Derivation

The macro expansion only works if a semi-auto derivation method exists
for your typeclass. The following pattern of writing typeclasses is
strongly recommended:

```scala
@typeclass trait Foo[A] {
  // put your methods here
}
object Foo extends FooLowPriority {
  // put your stdlib instances here
}
trait FooLowPriority {
  // put your slow derivations here (e.g. those that take a lot of evidence)
}
```

and in a separate file put your generic derivation, which is opt-in
and not automatically derived

```scala
trait DerivedFoo[A] extends Foo[A]
object DerivedFoo {
  def gen[A]: DerivedFoo[A] = ???
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

### `.Aux` Derivation

If you would prefer to let the compiler infer the type

```scala
@deriving(Generic, LabelledGeneric)
case class Bar(s: String, b: Boolean)
```

generating

```scala
object Bar {
  implicit val generic = shapeless.Generic[Bar]
  implicit val labelled = shapeless.LabelledGeneric[Bar]
}
```

(note that the type is not bound on the LHS, allowing for complex types), you can do so by creating a `.Aux` rule (see customisation below). Note that because these derivations are typically "flat", we do not require implicit evidence for the typeclass for all type parameters. e.g. for

```scala
@deriving(Generic)
final case class Gaz[T](t: T)
```

we generate

```scala
object Gaz {
  implicit def generic[T] = Generic[Gaz[T]]
}
```

not

```scala
object Gaz {
  implicit def generic[T: Generic] = Generic[Gaz[T]]
}
```

### Custom

We provide wirings for several popular libraries out-of-the-box in
=stalactite.conf= (and will accept Merge Requests to add more).

You can provide your own project-specific wirings by setting up your
build to point to your configuration files, e.g.

```scala
scalacOptions ++= {
  val dir = (baseDirectory in ThisBuild).value / "project"
  Seq(
    s"-Xmacro-settings:stalactite.targets=$dir/stalactite-targets.conf",
    s"-Xmacro-settings:stalactite.defaults=$dir/stalactite-defaults.conf"
  )
}
```

The `targets` config file is plain text with one line per wiring,
formatted: `fqn.TypeClass=fqn.DerivedTypeClass.method` for standard
mappings, or `fqn.TypeClass.Aux=fqn.DerivedTypeClass.method` for
`.Aux` rules.

The `defaults` config file is plain text with one line per default
typeclass. Use the `fqn.TypeClass` name, special patterns (such as
`.Aux`) will be picked up correctly from the `targets` config.

## Installation

### IntelliJ Users

Stalactite will work out-of-the box once [#388 is merged and released](https://github.com/JetBrains/intellij-scala/pull/388).

Until then, you can install this
[Custom Scala Plugin](https://github.com/fommil/stalactite/releases/download/v0.0.2/scala-plugin.zip).

### Maven Central

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.fommil/stalactite_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.fommil/stalactite_2.12)

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

libraryDependencies += "com.fommil" %% "stalactite" % "<version>"
```

### Snapshots

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "com.fommil" %% "stalactite" % "<version + 0.0.1>-SNAPSHOT"
```

# `scalaz-deriving`

`scalaz-deriving` adds new typeclasses to `scalaz` and will be included in scalaz 7.3.x:

| Typeclass         | method      | given          | signature         | returns |
|-------------------|-------------|----------------|-------------------|---------|
|`LazyApplicative`  | `apply2`    | `F[A1], F[A2]` | `(A1, A2) => Z`   | `F[Z]`  |
|`Coapplicative`    | `coapply2`  | `F[A1], F[A2]` | `(A1 \/ A2) => Z` | `F[Z]`  |
|`LazyDivisible`    | `divide2`   | `F[A1], F[A2]` | `Z => (A1, A2)`   | `F[Z]`  |
|`Codivide`         | `codivide2` | `F[A1], F[A2]` | `Z => (A1 \/ A2)` | `F[Z]`  |

with `apply3` and `apply4` defined in terms of `apply2`, etc.

These typeclasses are a formalism to derive both covariant (reader) and contravariant (writer) typeclasses, for products (`case classes`) and coproducts (`sealed traits`).

We provide generic variants (unlimited arity) using the [iotaz](https://github.com/frees-io/iota) high performance generic programming library.

A typeclass author will implement one of the following interfaces:

```scala
abstract class CovariantDerivez[F[_], G[_]: Monad: FromFoldable1] extends Derivez[F] {
  def productz[Z](f: (F ~> Id) => Z): F[Z]
  def coproductz[Z](f: (F ~> G) => G[Z]): F[Z]
}

abstract class ContravariantDerivez[F[_]] extends Derivez[F] {
  def productz[Z, G[_]: Foldable](f: Z =*> G): F[Z]
  def coproductz[Z](f: Z =+> Maybe): F[Z]
}
```

`~>` should be familiar to users of scalaz: a natural transformation. Covariant derivation requires a function from `F[A] => A` for products, or `F[A] => G` (a `Monad` of your choice that can be built from a `Foldable1`), returning a `Z`. The coproduct `F[A]` are evaluated lazily to avoid computation (at least the first is evaluated [because of #1515](https://github.com/scalaz/scalaz/issues/1515)).

The operators `=*>` and `=+>` are introduced by this library:

- `=*>` takes a `Z` or `(Z, Z)`, returning a `Foldable` of `A`s and their corresponding `F[A]`.
- `=+>` takes a `Z`, returning an `A` and its corresponding `F[A]`, or a `Maybe` if given `(Z, Z)`.

For example, for a covariant typeclass `Default`:

```scala
trait Default[A] {
  def default: A
}
object Default {
  ...
  implicit val Derivez: CovariantDerivez[Default] = new CovariantDerivez[Default, Id] {
    val choose = λ[Default ~> Id](_.default)
    override def productz[Z](f: (Default ~> Id) => Z): Default[Z] = instance { f(choose) }
    override def coproductz[Z](f: (Default ~> Id) => Id[Z]): Default[Z] = instance { f(choose) }
  }
```

which also defines `Functor[Default]`, giving us `.xmap` and `.map` for free.

For a contravariant typeclass, `Equal` write:

```scala
trait Equal[F] {
  def equal(a1: F, a2: F): Boolean
}
object Equal {
  ...
  implicit val Derivez: ContravariantDerivez[Equal] = new ContravariantDerivez[Equal] {
    def productz[Z, G[_]: Foldable](f: Z =*> G): Equal[Z] = { (z1: Z, z2: Z) =>
      f(z1, z2).all { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }
    }

    def coproductz[Z](f: Z =+> Maybe): Equal[Z] = { (z1: Z, z2: Z) =>
      f(z1, z2).map { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }.getOrElse(false)
    }
  }
}
```

which defines `InvariantFunctor[Equal]`, giving us `.xmap` and `.contramap` for free.

The API is currently in flux and support for typeclasses that require labels (e.g. `Show` / encoders / decoders) is coming soon.
