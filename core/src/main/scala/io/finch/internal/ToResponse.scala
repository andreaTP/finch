package io.finch.internal

import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.http.{Response, Status, Version}
import com.twitter.io.{Buf, Reader}
import io.finch.Encode
import shapeless.Witness

/**
 * Represents a conversion from `A` to [[Response]].
 */
trait ToResponse[-A] {
  def apply(a: A): Response
}

object ToResponse {

  /**
   * Returns an instance for a given type.
   */
  @inline def apply[A](implicit tr: ToResponse[A]): ToResponse[A] = tr

  /**
   * Constructs an instance from a function.
   */
  def instance[A](f: A => Response): ToResponse[A] = new ToResponse[A] {
    def apply(a: A): Response = f(a)
  }

  implicit val responseToResponse: ToResponse[Response] = instance(identity)

  implicit def encodeableToResponse[A, CT <: String](implicit
    e: Encode.Aux[A, CT],
    w: Witness.Aux[CT]
  ): ToResponse[A] = instance { a =>
    val rep = Response()
    rep.content = e(a)
    rep.contentType = w.value

    rep
  }

  /**
   * Provides implicit ToResponse for `AsyncStream[Buf]`.
   * If it reaches the end of the stream it closes the connection.
   *
   * @note Http server should be initialized with `.withStreaming(enabled = true)` to get advantage
   *       of streaming (chunked requests).
   */
  implicit val asyncToResponse: ToResponse[AsyncStream[Buf]] =
    instance { a =>
      val writable = Reader.writable()
      a.foreachF(writable.write).ensure(writable.close())
      Response(Version.Http11, Status.Ok, writable)
    }
}
