/*
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.znet2

import java.io._

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.LogTag
import com.waz.threading.{ CancellableFuture, SerialDispatchQueue, Threading }
import com.waz.utils.{ IoUtils, JsonDecoder, JsonEncoder }
import com.waz.znet2.HttpClient.{ BodyDeserializer, BodySerializer, ProgressCallback }
import okhttp3.{ Call, Callback, OkHttpClient }
import org.json.JSONObject

import scala.concurrent.Promise
import scala.util.{ Failure, Success, Try }

object HttpClient {

  val BufferSize = 2048

  type ProgressCallback = Progress => Unit

  case class Progress(progress: Long, total: Option[Long]) {
    val isCompleted: Boolean = total.forall(_ == progress)
  }

  sealed trait HttpClientError                                       extends Throwable
  case class UnsuccessfulResponseError[T](response: HttpResponse[T]) extends HttpClientError
  case class ConnectionError(err: Throwable)                         extends HttpClientError
  case class EncodingError(err: Throwable)                           extends HttpClientError
  case class DecodingError(err: Throwable)                           extends HttpClientError
  case class EmptyBodyError[T](response: HttpResponse[T])            extends HttpClientError

  case class DeserializingParameters(tmpFileGenerator: () => File)

  trait BodySerializer[T] {
    def serialize(value: HttpRequest[T]): HttpRequest[Http.Body]
  }

  object BodySerializer {

    import HttpRequest._

    def apply[T](f: HttpRequest[T] => HttpRequest[Http.Body]): BodySerializer[T] = new BodySerializer[T] {
      override def serialize(value: HttpRequest[T]): HttpRequest[Http.Body] = f(value)
    }

    def contramap[A, B](f: A => B)(implicit bs: BodySerializer[B]): BodySerializer[A] =
      apply(lift(f) andThen bs.serialize)

    implicit val EmptyBodySerializer: BodySerializer[Http.EmptyBody] = apply(_.copy(body = None))

    implicit val BytesBodySerializer: BodySerializer[Array[Byte]] =
      apply(lift(bytes => Http.Body(Some(Http.MediaType.Bytes), new ByteArrayInputStream(bytes), Some(bytes.length))))

    implicit val JsonBodySerializer: BodySerializer[JSONObject] =
      apply(request => BytesBodySerializer.serialize(request.map(_.toString.getBytes("utf8"))))

    implicit val FileBodySerializer: BodySerializer[File] =
      apply(lift(file => Http.Body(None, new FileInputStream(file), Some(file.length()))))

    implicit def objectToJsonBodySerializer[T](implicit e: JsonEncoder[T]): BodySerializer[T] =
      contramap(e.apply)

  }

  trait BodyDeserializer[T] {
    def deserialize(response: HttpResponse[Http.Body], callback: Option[ProgressCallback], parameters: DeserializingParameters): HttpResponse[T]
  }

  object BodyDeserializer {

    import HttpResponse._

    def apply[T](f: (HttpResponse[Http.Body], Option[ProgressCallback], DeserializingParameters) => HttpResponse[T]): BodyDeserializer[T] =
      new BodyDeserializer[T] {
        override def deserialize(response: HttpResponse[Http.Body], callback: Option[ProgressCallback], parameters: DeserializingParameters): HttpResponse[T] =
          f(response, callback, parameters)
      }

    //TODO Not contramap, find proper name
    def contramap[A, B](f: B => A)(implicit bd: BodyDeserializer[B]): BodyDeserializer[A] =
      apply((response, callback, params) => bd.deserialize(response, callback, params).map(f))

    implicit val HttpEmptyBodyDeserializer: BodyDeserializer[Http.EmptyBody] =
      apply((response, _, _) => response.copy(body = None))

    implicit val BytesBodyDeserializer: BodyDeserializer[Array[Byte]] =
      apply((response, callback, _) => map(response)(body => IoUtils.toByteArray(body.data)))

    implicit val FileBodyDeserializer: BodyDeserializer[File] = BodyDeserializer { (response, callback, params) =>
      map(response) { body =>
        val file = params.tmpFileGenerator()
        IoUtils.withResource(new FileOutputStream(file)) { out =>
          val buf                 = Array.fill[Byte](2048)(0)
          var totalRead           = 0L
          var readCount           = 0
          callback.foreach(c => c(Progress(0, body.dataLength)))
          while ({ readCount = body.data.read(buf); readCount != -1 }) {
            totalRead += readCount
            out.write(buf, 0, readCount)
            callback.foreach(c => c(Progress(totalRead, body.dataLength)))
          }
        }

        file
      }
    }

    implicit val JsonBodyDeserializer: BodyDeserializer[JSONObject] =
      apply((response, _, _) => map(response)(body => new JSONObject(new String(IoUtils.toByteArray(body.data)))) )

    implicit def objectToJsonBodyDeserializer[T](implicit d: JsonDecoder[T]): BodyDeserializer[T] =
      contramap((json: JSONObject) => d(json))

  }

}

trait HttpClient {

  def call[T: BodySerializer, R: BodyDeserializer](
      request: HttpRequest[T],
      uploadCallback: Option[ProgressCallback] = None,
      downloadCallback: Option[ProgressCallback] = None
  ): CancellableFuture[HttpResponse[R]]

  def decodedResult[T: BodySerializer, R: BodyDeserializer](
      request: HttpRequest[T],
      uploadCallback: Option[ProgressCallback] = None,
      downloadCallback: Option[ProgressCallback] = None
  ): CancellableFuture[R]

}

class HttpClientOkHttpImpl() extends HttpClient {
  import HttpClient._
  import OkHttpConverters._

  private implicit val dispatcher: SerialDispatchQueue = new SerialDispatchQueue(Threading.ThreadPool)
  private val client                                   = new OkHttpClient()

  private val deserializingParameters = DeserializingParameters(
    tmpFileGenerator =
      () => new File(s"${System.getProperty("java.io.tmpdir")}/http_client_tmp_${System.currentTimeMillis()}")
  )

  override def call[T, R](
      request: HttpRequest[T],
      uploadCallback: Option[ProgressCallback] = None,
      downloadCallback: Option[ProgressCallback] = None
  )(implicit bs: BodySerializer[T], bd: BodyDeserializer[R]): CancellableFuture[HttpResponse[R]] =
    CancellableFuture(bs.serialize(request))
      .recoverWith { case err: Throwable => CancellableFuture.failed(EncodingError(err)) }
      .flatMap { serializedRequest =>
        val promise = Promise[HttpResponse[Http.Body]]
        new CancellableFuture(promise) {
          private val okCall = client.newCall(convertHttpRequest(serializedRequest, uploadCallback, BufferSize))
          okCall.enqueue(new Callback {
            override def onResponse(call: Call, response: okhttp3.Response): Unit =
              promise.trySuccess(convertOkHttpResponse(response))
            override def onFailure(call: Call, e: IOException): Unit =
              promise.tryFailure(ConnectionError(e))
          })

          override def cancel()(implicit tag: LogTag): Boolean = {
            okCall.cancel()
            super.cancel()(tag)
          }
        }
      }
      .flatMap { response =>
        Try(bd.deserialize(response, downloadCallback, deserializingParameters)) match {
          case Success(result) => CancellableFuture.successful(result)
          case Failure(err)  => CancellableFuture.failed(DecodingError(err))
        }
      }

  override def decodedResult[T: BodySerializer, R: BodyDeserializer](
      request: HttpRequest[T],
      uploadCallback: Option[ProgressCallback] = None,
      downloadCallback: Option[ProgressCallback] = None
  ): CancellableFuture[R] = call[T, R](request, uploadCallback, downloadCallback).flatMap { response =>
    if (!response.code.isSuccessful) CancellableFuture.failed(UnsuccessfulResponseError(response))
    response.body match {
      case None         => CancellableFuture.failed(EmptyBodyError(response))
      case Some(result) => CancellableFuture.successful(result)
    }
  }

}
