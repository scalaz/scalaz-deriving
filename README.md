`scalaz-deriving` makes it easy to derive typeclass instances for your data types. The benefits are:

- [much faster compiles](https://github.com/propensive/magnolia/pull/45)
- [faster runtime](https://github.com/fosskers/scalaz-and-cats/pull/4)
- simpler implicit rules (less time fighting the compiler)
- cleaner compiler errors (know where an implicit is missing)
- easy to write derivation logic for your own typeclasses

There are two independent and complementary parts to this library:

- a `@deriving` macro annotation to easily add `implicit` typeclass instances to companion objects. This macro is compatible with [magnolia](http://magnolia.work/), [shapeless generic derivation](http://fommil.com/scalax15/), and hand-rolled derivers (e.g. `play-json`).
- `scalaz-deriving`, a principled way for typeclass authors to define typeclass derivations, plus derivations for some `scalaz-core` typeclasses (e.g. `Equal`, `Show`).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [`@deriving` Macro Annotation](#deriving-macro-annotation)
    - [Backends](#backends)
        - [`Derived` Style](#derived-style)
        - [`.Aux` Derivation](#aux-derivation)
- [`scalaz-deriving`](#scalaz-deriving)
- [Installation](#installation)
    - [IntelliJ Users](#intellij-users)
    - [Maven Central](#maven-central)
    - [Caveats](#caveats)

<!-- markdown-toc end -->

# `@deriving` Macro Annotation

The `@deriving` annotation simplifies the *semi-auto* pattern, whereby implicit evidence is explicitly added to data type companions, rather than being inferred at the point of use (known as *full-auto*). In short,

```scala
@scalaz.deriving(Encoder, Decoder)
case class Bar(s: String, b: Boolean)
```

expands to

```scala
case class Bar(s: String, b: Boolean)
object Bar {
  implicit val encoder: Encoder[Bar] = DerivedEncoder.gen
  implicit val decoder: Decoder[Bar] = DerivedDecoder.gen
}
```

The annotation also supports:

- type parameters (using `implicit def` rather than `implicit val`)
- `sealed trait` and `object`
- `extends AnyVal` (if the typeclass `TC[A]` has a `def xmap[B](f: A => B, g: B => A): TC[B]` or [`scalaz.InvariantFunctor`](https://static.javadoc.io/org.scalaz/scalaz_2.12/7.2.15/scalaz/InvariantFunctor.html))

## Backends

The default expansion is to generate a call to `scalaz.Derivez.gen[TC, A]`, which (if installed) expands into:

```scala
val gen = scalaz.ProdGen.gen[Foo]
val tcs = iotaz.Prod(Need(implicitly[Equal[String]]), Need(implicitly[Equal[Int]]))
scalaz.Derivez.xproductz(tcs, gen.labels)(gen.to, gen.from)
```

We provide wirings for several popular libraries out-of-the-box (e.g. `play.json.Format`) and you can provide your own project-specific wirings by setting up your build to point to your configuration files, e.g.

```scala
scalacOptions ++= {
  val dir = (baseDirectory in ThisBuild).value / "project"
  Seq(
    s"-Xmacro-settings:deriving.targets=$dir/deriving-targets.conf",
    s"-Xmacro-settings:deriving.defaults=$dir/deriving-defaults.conf"
  )
}
```

If you are a library author, you can distribute a "deriving.conf" in the same format in your published artifacts and scalaz-deriving will find it.

The `targets` config file is plain text with one line per wiring, formatted: `fqn.TypeClass=fqn.DerivedTypeClass.method`.

The `defaults` config file is plain text with one line per typeclass that you wish to **always** be derived. This is best reserved for performance optimisations, e.g. avoiding multiple `shapeless.Generic` derivations, rather than for feature-based typeclasses.

### `Derived` Style

If you are writing a [magnolia](http://magnolia.work/), [shapeless generic derivation](http://fommil.com/scalax15/), or custom macro, please follow this convention:

```scala
@typeclass trait Foo[A] { ... }

trait DerivedFoo[A] extends Foo[A]
object DerivedFoo {
  def gen: DerivedFoo[A] = ???
}
```

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

(note that the type is not bound on the LHS, allowing for complex types), you can do so by creating a `.Aux` rule in the config file, similar to the included rule:

```
shapeless.Generic.Aux=shapeless.Generic.apply
```

# `scalaz-deriving`

`scalaz-deriving` adds new typeclasses to `scalaz`:

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
  def productz[Z, G[_]: Traverse](f: Z =*> G): F[Z]
  def coproductz[Z](f: Z =+> Maybe): F[Z]
}
```

`~>` should be familiar to users of scalaz: a natural transformation. Covariant derivation requires a function from `F[A] => A` for products, or `F[A] => G` (a non-empty `Monad` of your choice).

The operators `=*>` and `=+>` are introduced by this library and are used for contravariant derivation:

- `=*>` takes a `Z` or `(Z, Z)`, returning a `Traverse` of `A`s and their corresponding `F[A]`.
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
    def productz[Z, G[_]: Traverse](f: Z =*> G): Equal[Z] = { (z1: Z, z2: Z) =>
      f(z1, z2).all { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }
    }

    def coproductz[Z](f: Z =+> Maybe): Equal[Z] = { (z1: Z, z2: Z) =>
      f(z1, z2).map { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }.getOrElse(false)
    }
  }
}
```

which defines `InvariantFunctor[Equal]`, giving us `.xmap` and `.contramap` for free.

If your typeclass requires access to labels (e.g. names of `case class` and `sealed trait` values) then you should use the `LabelledDerivez` variants:

```scala
implicit val ShowDerivez: LabelledContravariantDerivez[Show] = new LabelledContravariantDerivez[Show] {
  def contramap[A, B](r: Show[A])(f: B => A): Show[B] = Show.show { b => r.show(f(b)) }
  def productz[Z, G[_]: Traverse](f: Z =*> G): Show[Z] = Show.show { z: Z =>
    "(" +: f(z).map { case fa /~\ ((label, a)) => label +: "=" +: fa.show(a) }.intercalate(",") :+ ")"
  }
  def coproductz[Z](f: Z =+> Maybe): Show[Z] = Show.show { z: Z =>
    f(z) match { case fa /~\ ((label, a)) => label +: fa.show(a) }
  }
}
```

Note that `contramap` (and `map`) are not provided automatically by these variants so must be implemented. The calls to `=*>` and `=+>` return a subtely different response than the non-label variants, with the `label` easily extracted with a pattern match.

# Installation

## IntelliJ Users

`@deriving` will work out-of-the box if you are using the nightly plugin release.

## Maven Central

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.fommil/deriving-macro_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.fommil/deriving-macro_2.12)

The artefacts are independent and may be installed separately. The `@deriving` macro requires that the [macro paradise](https://docs.scala-lang.org/overviews/macros/paradise.html) compiler plugin is installed:

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.fommil" %% "deriving-macro" % "<version>",
  "com.fommil" %% "scalaz-deriving" % "<version>"
)
```

Snapshots are also available if you have `resolvers += Resolver.sonatypeRepo("snapshots")`.

## Caveats

This is a beta release, missing important features, tracked in the [1.0 Milestone](https://gitlab.com/fommil/scalaz-deriving/milestones/1).

`scalaz-deriving` does not and will not support typeclasses with contravariant or covariant type parameters (e.g. `[-A]` and `[+A]`). Fundamentally, Scala's [variance is broken](https://leanpub.com/fpmortals/read#leanpub-auto-type-variance) and should be avoided.

The macro that generates the [iotaz](https://github.com/frees-io/iota) representation is very primitive and does not support exotic language features or renaming type parameters in GADTs. This will be addressed as iota becomes more mature. Indeed, much of the internals of `scalaz-deriving` [will be ported to iota](https://gitlab.com/fommil/scalaz-deriving/issues/47) and contributors would be very welcome to help with this effort.

Macro annotations do not work in ENSIME and Scala IDE. The generated methods are visible after a compile but you may still experience strangeness in files that use the `@deriving` macro. This will be addressed by [rewriting `@deriving` as a compiler plugin](https://gitlab.com/fommil/scalaz-deriving/issues/41).

