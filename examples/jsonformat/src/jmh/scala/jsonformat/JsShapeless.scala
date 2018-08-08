// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scalaz.{ Coproduct => _, :+: => _, _ }, Scalaz._
import shapeless._, labelled._

import JsDecoder.fail
import JsDecoder.ops._

sealed trait DerivedJsEncoder[A, R, J <: HList] {
  def toJsFields(r: R, anns: J): IList[(String, JsValue)]
}
object DerivedJsEncoder extends DerivedJsEncoder1 {
  @inline def gen[A, R, J <: HList](
    implicit
    G: LabelledGeneric.Aux[A, R],
    J: Annotations.Aux[json, A, J],
    R: Cached[Strict[DerivedJsEncoder[A, R, J]]]
  ): JsEncoder[A] = new JsEncoder[A] {
    def toJson(a: A) = JsObject(R.value.value.toJsFields(G.to(a), J()))
  }

  @inline implicit def hnil[A]: DerivedJsEncoder[A, HNil, HNil] =
    new DerivedJsEncoder[A, HNil, HNil] {
      def toJsFields(h: HNil, a: HNil) = IList.empty
    }

  @inline implicit def cnil[A]: DerivedJsEncoder[A, CNil, HNil] =
    new DerivedJsEncoder[A, CNil, HNil] {
      def toJsFields(c: CNil, a: HNil) = sys.error("impossible")
    }

  @inline implicit def cconsAnnotated[
    A,
    K <: Symbol,
    H,
    T <: Coproduct,
    J <: HList
  ](
    implicit
    A: Annotation[json, A],
    K: Witness.Aux[K],
    H: Lazy[JsEncoder[H]],
    T: DerivedJsEncoder[A, T, J]
  ): DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J] =
    new DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J] {
      private val hint = A().field.getOrElse("type") -> JsString(K.value.name)
      def toJsFields(ht: FieldType[K, H] :+: T, anns: None.type :: J) =
        ht match {
          case Inl(head) =>
            H.value.toJson(head) match {
              case JsObject(fields) => hint :: fields
              case v                => IList.single("xvalue" -> v)
            }
          case Inr(tail) => T.toJsFields(tail, anns.tail)
        }
    }

  @inline implicit def cconsAnnotatedCustom[
    A,
    K <: Symbol,
    H,
    T <: Coproduct,
    J <: HList
  ](
    implicit
    A: Annotation[json, A],
    K: Witness.Aux[K],
    H: Lazy[JsEncoder[H]],
    T: DerivedJsEncoder[A, T, J]
  ): DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J] =
    new DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J] {
      private val hintfield = A().field.getOrElse("type")
      def toJsFields(ht: FieldType[K, H] :+: T, anns: Some[json] :: J) =
        ht match {
          case Inl(head) =>
            val ann = anns.head.get
            H.value.toJson(head) match {
              case JsObject(fields) =>
                val hint = (hintfield -> JsString(
                  ann.hint.getOrElse(K.value.name)
                ))
                hint :: fields
              case v =>
                val xvalue = ann.field.getOrElse("xvalue")
                IList.single(xvalue -> v)
            }
          case Inr(tail) => T.toJsFields(tail, anns.tail)
        }
    }
}
private[jsonformat] trait DerivedJsEncoder1 {
  @inline implicit def hcons[A, K <: Symbol, H, T <: HList, J <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsEncoder[H]],
    T: DerivedJsEncoder[A, T, J]
  ): DerivedJsEncoder[A, FieldType[K, H] :: T, None.type :: J] =
    new DerivedJsEncoder[A, FieldType[K, H] :: T, None.type :: J] {
      private val field = K.value.name
      def toJsFields(ht: FieldType[K, H] :: T, anns: None.type :: J) =
        ht match {
          case head :: tail =>
            val rest = T.toJsFields(tail, anns.tail)
            H.value.toJson(head) match {
              case JsNull => rest
              case value  => (field -> value) :: rest
            }
        }
    }

  @inline implicit def hconsCustom[A, K <: Symbol, H, T <: HList, J <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsEncoder[H]],
    T: DerivedJsEncoder[A, T, J]
  ): DerivedJsEncoder[A, FieldType[K, H] :: T, Some[json] :: J] =
    new DerivedJsEncoder[A, FieldType[K, H] :: T, Some[json] :: J] {
      def toJsFields(ht: FieldType[K, H] :: T, anns: Some[json] :: J) =
        ht match {
          case head :: tail =>
            val ann  = anns.head.get
            val rest = T.toJsFields(tail, anns.tail)
            H.value.toJson(head) match {
              case JsNull if !ann.nulls => rest
              case value =>
                val field = ann.field.getOrElse(K.value.name)
                (field -> value) :: rest
            }
        }
    }

  @inline implicit def ccons[A, K <: Symbol, H, T <: Coproduct, J <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsEncoder[H]],
    T: DerivedJsEncoder[A, T, J]
  ): DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J] =
    new DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J] {
      private val hint = ("type" -> JsString(K.value.name))
      def toJsFields(ht: FieldType[K, H] :+: T, anns: None.type :: J) =
        ht match {
          case Inl(head) =>
            H.value.toJson(head) match {
              case JsObject(fields) => hint :: fields
              case v                => IList.single("xvalue" -> v)
            }
          case Inr(tail) => T.toJsFields(tail, anns.tail)
        }
    }

  @inline implicit def cconsCustom[
    A,
    K <: Symbol,
    H,
    T <: Coproduct,
    J <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsEncoder[H]],
    T: DerivedJsEncoder[A, T, J]
  ): DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J] =
    new DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J] {
      def toJsFields(ht: FieldType[K, H] :+: T, anns: Some[json] :: J) =
        ht match {
          case Inl(head) =>
            val ann = anns.head.get
            H.value.toJson(head) match {
              case JsObject(fields) =>
                val hint = ("type" -> JsString(
                  ann.hint.getOrElse(K.value.name)
                ))
                hint :: fields
              case v =>
                val xvalue = ann.field.getOrElse("xvalue")
                IList.single(xvalue -> v)
            }
          case Inr(tail) => T.toJsFields(tail, anns.tail)
        }
    }
}

sealed trait DerivedCoproductJsDecoder[A, R, J <: HList] {
  def fromJsObject(j: JsObject, anns: J): String \/ R
}
object DerivedCoproductJsDecoder extends DerivedCoproductJsDecoder1 {
  @inline def gen[A, R, J <: HList](
    implicit G: LabelledGeneric.Aux[A, R],
    J: Annotations.Aux[json, A, J],
    R: Cached[Strict[DerivedCoproductJsDecoder[A, R, J]]]
  ): JsDecoder[A] = new JsDecoder[A] {
    def fromJson(j: JsValue) = j match {
      case o @ JsObject(_) => R.value.value.fromJsObject(o, J()).map(G.from)
      case other           => fail("JsObject", other)
    }
  }

  @inline implicit def cnil[A]: DerivedCoproductJsDecoder[A, CNil, HNil] =
    new DerivedCoproductJsDecoder[A, CNil, HNil] {
      def fromJsObject(j: JsObject, a: HNil) =
        fail(s"JsObject with 'type' field", j)
    }

  @inline implicit def cconsAnnotated[
    A,
    K <: Symbol,
    H,
    T <: Coproduct,
    J <: HList
  ](
    implicit
    A: Annotation[json, A],
    K: Witness.Aux[K],
    H: Lazy[JsDecoder[H]],
    T: DerivedCoproductJsDecoder[A, T, J]
  ): DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, None.type :: J] =
    new DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, None.type :: J] {
      private val hintfield = A().field.getOrElse("type")
      def fromJsObject(j: JsObject, anns: None.type :: J) = {
        val hint = (hintfield -> JsString(K.value.name))
        if (j.fields.element(hint)) {
          j.get("xvalue")
            .into {
              case \/-(x) => H.value.fromJson(x)
              case -\/(_) => H.value.fromJson(j)
            }
            .map(h => Inl(field[K](h)))
        } else
          T.fromJsObject(j, anns.tail).map(Inr(_))
      }
    }

  @inline implicit def cconsAnnotatedCustom[
    A,
    K <: Symbol,
    H,
    T <: Coproduct,
    J <: HList
  ](
    implicit
    A: Annotation[json, A],
    K: Witness.Aux[K],
    H: Lazy[JsDecoder[H]],
    T: DerivedCoproductJsDecoder[A, T, J]
  ): DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, Some[json] :: J] =
    new DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, Some[json] :: J] {
      private val hintfield = A().field.getOrElse("type")
      def fromJsObject(j: JsObject, anns: Some[json] :: J) = {
        val ann  = anns.head.get
        val hint = (hintfield -> JsString(ann.hint.getOrElse(K.value.name)))
        if (j.fields.element(hint)) {
          val xvalue = ann.field.getOrElse("xvalue")
          j.get(xvalue)
            .into {
              case \/-(x) => H.value.fromJson(x)
              case -\/(_) => H.value.fromJson(j)
            }
            .map(h => Inl(field[K](h)))
        } else
          T.fromJsObject(j, anns.tail).map(Inr(_))
      }
    }

}
private[jsonformat] trait DerivedCoproductJsDecoder1 {
  @inline implicit def ccons[A, K <: Symbol, H, T <: Coproduct, J <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsDecoder[H]],
    T: DerivedCoproductJsDecoder[A, T, J]
  ): DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, None.type :: J] =
    new DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, None.type :: J] {
      private val hint = ("type" -> JsString(K.value.name))
      def fromJsObject(j: JsObject, anns: None.type :: J) =
        if (j.fields.element(hint)) {
          j.get("xvalue")
            .into {
              case \/-(x) => H.value.fromJson(x)
              case -\/(_) => H.value.fromJson(j)
            }
            .map(h => Inl(field[K](h)))
        } else
          T.fromJsObject(j, anns.tail).map(Inr(_))
    }

  @inline implicit def cconsCustom[
    A,
    K <: Symbol,
    H,
    T <: Coproduct,
    J <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsDecoder[H]],
    T: DerivedCoproductJsDecoder[A, T, J]
  ): DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, Some[json] :: J] =
    new DerivedCoproductJsDecoder[A, FieldType[K, H] :+: T, Some[json] :: J] {
      def fromJsObject(j: JsObject, anns: Some[json] :: J) = {
        val ann  = anns.head.get
        val hint = ("type" -> JsString(ann.hint.getOrElse(K.value.name)))
        if (j.fields.element(hint)) {
          val xvalue = ann.field.getOrElse("xvalue")
          j.get(xvalue)
            .into {
              case \/-(x) => H.value.fromJson(x)
              case -\/(_) => H.value.fromJson(j)
            }
            .map(h => Inl(field[K](h)))
        } else
          T.fromJsObject(j, anns.tail).map(Inr(_))
      }
    }
}

sealed trait DerivedProductJsDecoder[A, R, J <: HList, D <: HList] {
  def fromJsObject(j: JsObject, anns: J, defaults: D): String \/ R
}
object DerivedProductJsDecoder extends DerivedProductJsDecoder1 {
  @inline def gen[A, R, J <: HList, D <: HList](
    implicit G: LabelledGeneric.Aux[A, R],
    J: Annotations.Aux[json, A, J],
    D: Default.AsOptions.Aux[A, D],
    R: Cached[Strict[DerivedProductJsDecoder[A, R, J, D]]]
  ): JsDecoder[A] = new JsDecoder[A] {
    def fromJson(j: JsValue) = j match {
      case o @ JsObject(_) =>
        R.value.value.fromJsObject(o, J(), D()).map(G.from)
      case other => fail("JsObject", other)
    }
  }

  @inline implicit def hnil[A]: DerivedProductJsDecoder[A, HNil, HNil, HNil] =
    new DerivedProductJsDecoder[A, HNil, HNil, HNil] {
      private val nil                                        = HNil.right[String]
      def fromJsObject(j: JsObject, a: HNil, defaults: HNil) = nil
    }
}
private[jsonformat] trait DerivedProductJsDecoder1 {
  @inline implicit def hcons[
    A,
    K <: Symbol,
    H,
    T <: HList,
    J <: HList,
    D <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsDecoder[H]],
    T: DerivedProductJsDecoder[A, T, J, D]
  ): DerivedProductJsDecoder[A, FieldType[K, H] :: T, None.type :: J, Option[H] :: D] =
    new DerivedProductJsDecoder[
      A,
      FieldType[K, H] :: T,
      None.type :: J,
      Option[H] :: D
    ] {
      private val fieldname = K.value.name
      def fromJsObject(
        j: JsObject,
        anns: None.type :: J,
        defaults: Option[H] :: D
      ) =
        for {
          head <- j.get(fieldname) match {
                   case \/-(v) => H.value.fromJson(v)
                   case -\/(_) =>
                     defaults.head match {
                       case Some(default) => \/-(default)
                       case None          => H.value.fromJson(JsNull)
                     }
                 }
          tail <- T.fromJsObject(j, anns.tail, defaults.tail)
        } yield field[K](head) :: tail
    }

  @inline implicit def hconsCustom[
    A,
    K <: Symbol,
    H,
    T <: HList,
    J <: HList,
    D <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: Lazy[JsDecoder[H]],
    T: DerivedProductJsDecoder[A, T, J, D]
  ): DerivedProductJsDecoder[
    A,
    FieldType[K, H] :: T,
    Some[json] :: J,
    Option[H] :: D
  ] =
    new DerivedProductJsDecoder[
      A,
      FieldType[K, H] :: T,
      Some[json] :: J,
      Option[H] :: D
    ] {
      def fromJsObject(
        j: JsObject,
        anns: Some[json] :: J,
        defaults: Option[H] :: D
      ) = {
        val ann       = anns.head.get
        val fieldname = ann.field.getOrElse(K.value.name)
        for {
          head <- j.get(fieldname) match {
                   case \/-(v) => H.value.fromJson(v)
                   case err @ -\/(_) =>
                     defaults.head match {
                       case Some(default)     => \/-(default)
                       case None if ann.nulls => err
                       case None              => H.value.fromJson(JsNull)
                     }
                 }
          tail <- T.fromJsObject(j, anns.tail, defaults.tail)
        } yield field[K](head) :: tail
      }
    }
}
