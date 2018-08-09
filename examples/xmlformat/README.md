And XML AST with encoding and decoding typeclasses.

The generic typeclass derivation uses some sensible defaults but can be
customised with the `@x.attr` and `@x.body` annotations on field members.

= Fields =

For the following

```scala
case class Bar(wobble: String)
case class Foo(wibble: Bar)
```

the default encoding is

```scala
<Bar><wobble>...</wobble></Bar>
<Foo><wibble><wobble>...</wobble></wibble></Foo>
```

where each case class gets assigned an `XTag` according to its name, which is
overwritten when included as part of an outer tag. Attributes and body are
preserved.

An alternative encoding is available using the `@x.body` annotation which will
use the provided encoding directly, ignoring the field name, e.g.

```scala
case class Baz(@x.body wibble: Bar)
<Baz><Bar><wobble>...</wobble></Bar></Baz>
```

this is useful when the decoder may use the tag name to distinguish between
coproducts.

```scala
sealed trait AA
case object B extends AA
case object C extends AA

case class A(@x.body as: List[AA])

<A>
  <B></B>
  <B></B>
  <C></C>
  <T/>
</A>
```

If the `@x.body` type has a `Monoid`, failure to decode anything will give
`Monoid.empty`. If it has a `Semigroup`, one or more values must be decoded or
there will be a failure. If there is neither, exactly one value is expected.

= Coproducts =

Coproducts are differentiated by a `"typehint"` attribute. Recall that tag names
can be rewritten by a container, therefore it is not feasible in the general
case to use tag names to differentiate between coproduct values.

However, if an element of the coproduct has an `@x.body`, tags will be used to
differentiate instead of attributes.

= Attributes =

```scala
case class Fuh(@x.attr foo: String)

<Fuh foo="Hello world"/>
```

Attributes can be specified with the `@x.attr` annotation and can only be used on
types that have an `XStrEncoder`.

= Body content =

```scala
case class Duh(@x.body body: String)

<Duh>Hello world!</Duh>
```

Values can be specified as the content of the tag with the `@x.body` if they have
an `XStrEncoder`. It is the developer's responsibility to ensure that only one
field has this tag. Multiple content fields will be concatenated and may not be
decoded.

= `Option` fields =

`Option` fields are special cased and a `None` value will not appear in the
output.
