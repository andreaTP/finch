package io.finch

import com.twitter.io.Buf
import shapeless.Witness

/**
 * An abstraction that is responsible for encoding the value of type `A`.
 */
trait Encode[A] {
  type ContentType <: String

  def apply(a: A): Buf
}

trait LowPriorityEncodeInstances {

  type Aux[A, CT <: String] = Encode[A] { type ContentType = CT }

  type ApplicationJson[A] = Aux[A, Witness.`"application/json"`.T]
  type TextPlain[A] = Aux[A, Witness.`"text/plain"`.T]
  type NoContentType[A] = Aux[A, Witness.`""`.T]

  def instance[A, CT <: String](fn: A => Buf): Aux[A, CT] =
    new Encode[A] {
      type ContentType = CT
      def apply(a: A): Buf = fn(a)
    }

  def applicationJson[A](fn: A => Buf): ApplicationJson[A] =
    instance[A, Witness.`"application/json"`.T](fn)

  def textPlain[A](fn: A => Buf): TextPlain[A] =
    instance[A, Witness.`"text/plain"`.T](fn)

  def noContentType[A](fn: A => Buf): NoContentType[A] =
    instance[A, Witness.`""`.T](fn)
}

object Encode extends LowPriorityEncodeInstances {

  class Implicitly[A] {
    def apply[CT <: String](w: Witness.Aux[CT])(implicit
      e: Encode.Aux[A, CT]
    ): Encode.Aux[A, CT] = e
  }

  @inline def apply[A]: Implicitly[A] = new Implicitly[A]

  implicit val encodeException: TextPlain[Exception] =
    textPlain(e => Buf.Utf8(Option(e.getMessage).getOrElse("")))

  implicit val encodeString: TextPlain[String] =
    textPlain(Buf.Utf8.apply)

  implicit val encodeUnit: NoContentType[Unit] =
    noContentType(_ => Buf.Empty)

  implicit val encodeBuf: NoContentType[Buf] =
    noContentType(identity)
}
