![data and codata](https://pbs.twimg.com/media/C4puwPsVUAAPPW5.jpg)

`stalactite` (twinned
with [`stalagmite`](https://github.com/fommil/stalagmite)) provides
something like Haskell's `deriving` for Scala:

```scala
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

with support for popular and user-defined typeclasses.
