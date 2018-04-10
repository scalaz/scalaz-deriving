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

- [Compiler Plugin](#compiler-plugin)
    - [`@deriving`](#deriving)
    - [`@xderiving`](#xderiving)
- [`scalaz-deriving`](#scalaz-deriving)
- [Installation](#installation)
    - [IntelliJ Users](#intellij-users)
    - [Maven Central](#maven-central)
    - [Breaking Changes](#breaking-changes)
        - [`deriving-macro`](#deriving-macro)
        - [`scalaz-deriving`](#scalaz-deriving)
    - [Caveats](#caveats)

<!-- markdown-toc end -->

# Compiler Plugin

## `@deriving`

The `@deriving` annotation simplifies the *semi-auto* pattern, whereby implicit evidence is explicitly added to data type companions, rather than being inferred at the point of use (known as *full-auto*). In short,

```scala
@scalaz.deriving(Encoder, Decoder)
case class Bar(s: String, b: Boolean)
```

expands to

```scala
object Bar {
  implicit val _deriving_encoder: Encoder[Bar] = scalaz.Derivez.gen[Encoder, Bar]
  implicit val _deriving_decoder: Decoder[Bar] = scalaz.Derivez.gen[Decoder, Bar]
}
```

The annotation also supports type parameters, using `implicit def` rather than `implicit val`, and can be used on `sealed trait` or `object`.

You can provide your own project-specific wirings in a `deriving.conf` file, which will also be available for users of your library if it is published.

The config file is plain text with one line per wiring, formatted: `fqn.TypeClass=fqn.DerivedTypeClass.method`, comments start with `#`.

## `@xderiving`

A variant `@xderiving` works only on classes with one parameter (including those that extend `AnyVal`), making use of an `.xmap` that the typeclass may provide directly or via an instance of `scalaz.InvariantFunctor`, e.g.

```scala
@scalaz.xderiving(Encoder, Decoder)
class Foo(val s: String)
```

expands into

```scala
object Foo {
  implicit val _deriving_encoder: Encoder[Foo] = implicitly[Encoder[String]].xmap(new Foo(_), _.s)
  implicit val _deriving_decoder: Decoder[Foo] = implicitly[Decoder[String]].xmap(new Foo(_), _.s)
}
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

`@deriving` and `@xderiving` will work out-of-the box since [2018.1.18](https://plugins.jetbrains.com/plugin/1347-scala).

## Maven Central

```scala
val derivingVersion = "<version>"
addCompilerPlugin("com.fommil" %% "deriving-plugin" % derivingVersion)
libraryDependencies ++= Seq(
  "com.fommil" %% "deriving-macro" % derivingVersion,
  "com.fommil" %% "scalaz-deriving" % derivingVersion
)
```

where `<version>` is the latest on [maven central](http://search.maven.org/#search|ga|1|g:com.fommil a:scalaz-deriving_2.12).

Snapshots are also available if you have `resolvers += Resolver.sonatypeRepo("snapshots")`.

If you wish to use `@deriving` with a custom deriver, you need to add your `resources` directory to the compiler classpath, e.g.

```scala
  // WORKAROUND: https://github.com/sbt/sbt/issues/1965
  def resourcesOnCompilerCp(config: Configuration): Setting[_] =
    managedClasspath in config := {
      val res = (resourceDirectory in config).value
      val old = (managedClasspath in config).value
      Attributed.blank(res) +: old
    }
```

and call with, e.g. `resourcesOnCompilerCp(Compile)`.

## Breaking Changes

We provide some automated rules to migrate when we introduce breaking changes. You must have a recent version of [scalafix](https://scalacenter.github.io/scalafix/docs/users/installation) installed.

### `deriving-macro`

- 0.9.0
  - `@stalactite.deriving` renamed to `@scalaz.deriving`
    - `scalafix replace:stalactite.deriving/scalaz.deriving`
- 0.10.0
  - the default deriver was changed from `Foo.gen` to `scalaz.Derivez.gen`, add your typeclasses to `deriving.conf`
  - the `-default.conf` derivations feature was removed
- 0.11.0
  - the `.Aux` derivation feature was removed
  - `@scalaz.deriving` special casing for `extends AnyVal` was replaced with `@scalaz.xderiving`
    - `scalafix https://gitlab.com/fommil/scalaz-deriving/raw/master/scalafix/rules/src/main/scala/fix/Deriving_0_11_0.scala`
  - the compiler plugin must be enabled
- 0.12.0
  - the `-Xmacro-settings:deriving` flag was removed, use `deriving.conf` in `resources`

### `scalaz-deriving`

The changelog will not be documented until 1.0.

## Caveats

`scalaz-deriving` does not and will not support typeclasses with contravariant or covariant type parameters (e.g. `[-A]` and `[+A]`). Fundamentally, Scala's [variance is broken](https://leanpub.com/fpmortals/read#leanpub-auto-type-variance) and should be avoided. Enforce this in your builds with the [`DisableSyntax`](https://scalacenter.github.io/scalafix/docs/rules/DisableSyntax) lint.

The macro that generates the [iotaz](https://github.com/frees-io/iota) representation does not support exotic language features or renaming type parameters in GADTs. This will be addressed as iota becomes more mature. Indeed, much of the internals of `scalaz-deriving` [will be ported to iota](https://gitlab.com/fommil/scalaz-deriving/issues/47).

Sometimes the scaladoc compiler can get confused and publishing will fail. We recommend that you simply disable scaladocs: nobody reads them and the source is always a better reference anyway:

```scala
sources in (Compile, doc) := Nil
```
