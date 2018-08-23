`scalaz-deriving` makes it easy to derive typeclass instances for your data types. The benefits are:

- [much faster compiles](https://github.com/propensive/magnolia/pull/45)
- simpler implicit rules (less time fighting the compiler)
- easy to write derivation logic for your own typeclasses

There are two independent and complementary parts to this library:

- a `@deriving` annotation to easily add `implicit` typeclass instances to companion objects. This macro is compatible with [magnolia](http://magnolia.work/), [shapeless generic derivation](http://fommil.com/scalax15/), and hand-rolled derivers (e.g. `play-json`). `@deriving` **does not depend on scalaz**.
- `scalaz-deriving`, a principled way for typeclass authors to define typeclass derivations, plus derivations for some `scalaz-core` typeclasses (e.g. `Equal`, `Monoid`).

**NOTE: This project does not track feature requests or bug reports from users.** General questions can be asked in https://gitter.im/scalaz/scalaz 💖 See `CONDUCT` for more.

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
  implicit val _deriving_encoder: Encoder[Bar] = scalaz.Deriving.gen[Encoder, Bar]
  implicit val _deriving_decoder: Decoder[Bar] = scalaz.Deriving.gen[Decoder, Bar]
}
```

The annotation is compatible with the `@newtype` annotation [by estatico](https://github.com/estatico/scala-newtype)

```scala
@newtype
@deriving(Encoder, Decoder)
case class Bar(s: String)
```

expanding into

```scala
@newtype
case class Bar(s: String)
object {
  implicit val _deriving_encoder: Encoder[Bar] = deriving
  implicit val _deriving_decoder: Decoder[Bar] = deriving
}
```

The annotation also supports type parameters, using `implicit def` rather than `implicit val`, and can be used on `sealed` classes, or `object`.

You can provide your own project-specific wirings in a `deriving.conf` file, which will also be available for users of your library if it is published.

The config file is plain text with one line per wiring, formatted: `fqn.TypeClass=fqn.DerivedTypeClass.method`, comments start with `#`.

If you wish to use `@deriving` with a custom deriver defined in the same project as the deriver, add your `resources` directory to the compiler classpath, e.g.

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

| Typeclass        | method    | given          | signature         | returns |
|------------------|-----------|----------------|-------------------|---------|
|`Applicative`     | `apply2`  | `F[A1], F[A2]` | `(A1, A2) => Z`   | `F[Z]`  |
|`Alt` (new)       | `altly2`  | `F[A1], F[A2]` | `(A1 \/ A2) => Z` | `F[Z]`  |
|`Divisible`       | `divide2` | `F[A1], F[A2]` | `Z => (A1, A2)`   | `F[Z]`  |
|`Decidable` (new) | `choose2` | `F[A1], F[A2]` | `Z => (A1 \/ A2)` | `F[Z]`  |

and `scalaz.Deriving`, which supports arbitrarily large `case class` and `sealed trait` ADTs and works out of the box with the `@deriving` annotation.

As a typeclass author you only need to implement `Deriving` for your typeclass.

If your typeclass can implement `Decidable` or `Alt` and satisfy their laws, you can:

1. wrap your `Decidable` or `Alt` with `ExtendedInvariantAlt` (lowest cognitive overhead).
2. directly implement the generic arbitrary variants `Decidablez` / `Altz` (highest performance, more complex).

If your typeclass cannot satisfy the `Decidable` or `Alt` laws, write a fresh `LabelledEncoder` or `LabelledDecoder`, which will also give you access to field names.

As an extra convenience, if `@deriving` is used on a `sealed` class it is not necessary to add the annotation to the known subtypes!

The following derivations are provided out-of-the-box for scalaz-core typeclasses:

1. `Equal` / `Order`
2. `Semigroup` / `Monoid`

and opt-in extras for

1. `scalaz.Show`
2. `org.scalacheck.Arbitrary`

Learn by example in `scalaz-deriving/src/test/scala/examples` and a detailed tutorial in the chapter "Typeclass Derivation" of [Functional Programming for Mortals with Scalaz](https://leanpub.com/fpmortals/read#leanpub-auto-typeclass-derivation).

# Installation

## IntelliJ Users

`@deriving` and `@xderiving` will work out-of-the box since [2018.1.18](https://plugins.jetbrains.com/plugin/1347-scala).

## Maven Central

```scala
val derivingVersion = "<version>"
libraryDependencies ++= Seq(
  // the @deriving and @xderiving plugin and macro
  "org.scalaz" %% "deriving-macro" % derivingVersion,
  compilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion),

  // the scalaz-deriving Altz / Decidablez / Deriving API and macros
  "org.scalaz" %% "scalaz-deriving" % derivingVersion,

  // instances for Show and Arbitrary
  "org.scalaz" %% "scalaz-deriving-magnolia" % derivingVersion,
  "org.scalaz" %% "scalaz-deriving-scalacheck" % derivingVersion,

  // shapeless alternatives to Deriving. See below for additional actions.
  "org.scalaz" %% "scalaz-deriving-shapeless" % derivingVersion
)
```

where `<version>` is the latest on [maven central](http://search.maven.org/#search|ga|1|g:org.scalaz%20a:scalaz-deriving_2.12).

To use the opt-in shapeless alternatives, which can sometimes improve runtime performance (and sometimes slow it down), either manually call the `DerivingEqual.gen` etc from your companions, or create a `deriving.conf` containing the wirings following the instructions above. Recall that shapeless derivations require annotations on every element of an ADT, not just the top element.

## Caveats

`scalaz-deriving` does not and will not support typeclasses with contravariant or covariant type parameters (e.g. `[-A]` and `[+A]`). Fundamentally, Scala's [variance is broken](https://leanpub.com/fpmortals/read#leanpub-auto-type-variance) and should be avoided. Enforce this in your builds with the [`DisableSyntax`](https://scalacenter.github.io/scalafix/docs/rules/DisableSyntax) lint.

When adding the `@deriving` annotation to a `sealed` class: 1) the derivation will be repeated if there are multiple `sealed` layers (which might slow down compiles), 2) the implicit scope of the subtype's companion is not searched, 3) only works for =scalaz-deriving= and magnolia, it doesn't work for shapeless derivers.

The macro that generates the [iotaz](https://github.com/frees-io/iota) representation does not support exotic language features or renaming type parameters in GADTs. This will be addressed as iota becomes more mature.

Sometimes the scaladoc compiler can get confused and publishing will fail. We recommend that you simply disable scaladocs: nobody reads them and the source is always a better reference anyway:

```scala
sources in (Compile, doc) := Nil
```
