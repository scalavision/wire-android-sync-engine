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
import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.utils.{IoUtils, JsonDecoder, JsonEncoder}
import com.waz.znet2.Http._
import com.waz.znet2.HttpClient._
import okhttp3.{Call, Callback, OkHttpClient}
import org.json.JSONObject

import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

object HttpClient {

  val BufferSize = 2048

  type ProgressCallback = Progress => Unit

  case class Progress(progress: Long, total: Option[Long]) {
    val isCompleted: Boolean = total.forall(_ == progress)
  }

  sealed trait HttpClientError                                       extends Throwable
  case class UnsuccessfulResponseError[T](response: Response[T]) extends HttpClientError
  case class ConnectionError(err: Throwable)                         extends HttpClientError
  case class EncodingError(err: Throwable)                           extends HttpClientError
  case class DecodingError(err: Throwable)                           extends HttpClientError
  case class EmptyBodyError[T](response: Response[T])            extends HttpClientError

  trait RequestSerializer[T] {
    def serialize(request: Request[T]): Request[Body]
  }

  object RequestSerializer {

    def apply[T](f: Request[T] => Request[Body]): RequestSerializer[T] = new RequestSerializer[T] {
      override def serialize(request: Request[T]): Request[Body] = f(request)
    }

    implicit val EmptyBodyRequestSerializer: RequestSerializer[EmptyBody] =
      apply(request => request.copy(body = None))

    implicit def serializerFromBodySerializer[T](implicit bs: BodySerializer[T]): RequestSerializer[T] =
      apply(request => request.copy(body = request.body.map(bs.serialize)))

  }

  trait BodySerializer[T] {
    def serialize(body: T): Body
  }

  object BodySerializer {

    def apply[T](f: T => Body): BodySerializer[T] = new BodySerializer[T] {
      override def serialize(body: T): Body = f(body)
    }

    def contramap[A, B](bs: BodySerializer[A])(f: B => A): BodySerializer[B] =
      apply(f andThen bs.serialize)

    implicit val BytesBodySerializer: BodySerializer[Array[Byte]] =
      apply(bytes => Body(Some(MediaType.Bytes), new ByteArrayInputStream(bytes), Some(bytes.length)))

    implicit val JsonBodySerializer: BodySerializer[JSONObject] =
      apply(json => BytesBodySerializer.serialize(json.toString.getBytes("utf8")))

    implicit val FileBodySerializer: BodySerializer[File] =
      apply(file => Body(None, new FileInputStream(file), Some(file.length())))

    implicit def objectToJsonBodySerializer[T](implicit e: JsonEncoder[T]): BodySerializer[T] =
      contramap(JsonBodySerializer)(e.apply)

  }

  trait ResponseDeserializer[T] {
    def deserialize(response: Response[Body], parameters: ResponseDeserializer.Parameters): Response[T]
  }

  object ResponseDeserializer {

    case class Parameters(tmpFileGenerator: () => File)

    def apply[T](f: (Response[Body], Parameters) => Response[T]): ResponseDeserializer[T] = new ResponseDeserializer[T] {
      override def deserialize(response: Response[Body], parameters: Parameters): Response[T] = f(response, parameters)
    }

    implicit val EmptyResponseBodyDeserializer: ResponseDeserializer[EmptyBody] =
      apply((response, _) => response.copy(body = None))

    implicit def deserializerFromBodyDeserializer[T](implicit bd: BodyDeserializer[T]): ResponseDeserializer[T] =
      apply((response, params) => response.copy(body = response.body.map(bd.deserialize(_, params))))
  }

  trait BodyDeserializer[T] {
    def deserialize(body: Body, parameters: ResponseDeserializer.Parameters): T
  }

  object BodyDeserializer {
    import ResponseDeserializer.Parameters

    def apply[T](f: (Body, Parameters) => T): BodyDeserializer[T] = new BodyDeserializer[T] {
      override def deserialize(body: Body, parameters: Parameters): T = f(body, parameters)
    }

    def map[A, B](bd: BodyDeserializer[A])(f: A => B): BodyDeserializer[B] =
      apply((body, params) => f(bd.deserialize(body, params)))

    implicit val BytesBodyDeserializer: BodyDeserializer[Array[Byte]] =
      apply((body, _) => IoUtils.toByteArray(body.data))

    implicit val FileBodyDeserializer: BodyDeserializer[File] =
      apply { (body, params) =>
        val file = params.tmpFileGenerator()
        IoUtils.copy(body.data, new FileOutputStream(file))
        file
    }

    implicit val JsonBodyDeserializer: BodyDeserializer[JSONObject] =
      apply((body, _) => new JSONObject(new String(IoUtils.toByteArray(body.data))))

    implicit def objectToJsonBodyDeserializer[T](implicit d: JsonDecoder[T]): BodyDeserializer[T] =
      map(JsonBodyDeserializer)(json => d(json))

  }

}

trait HttpClient {

  def call[T: RequestSerializer, R: ResponseDeserializer](
      request: Request[T],
      uploadCallback: Option[ProgressCallback] = None,
      downloadCallback: Option[ProgressCallback] = None
  ): CancellableFuture[Response[R]]

  def decodedResult[T: RequestSerializer, R: ResponseDeserializer](
      request: Request[T],
      uploadCallback: Option[ProgressCallback] = None,
      downloadCallback: Option[ProgressCallback] = None
  ): CancellableFuture[R]

}

class HttpClientOkHttpImpl() extends HttpClient {
  import HttpClient._
  import OkHttpConverters._

  private implicit val dispatcher: SerialDispatchQueue = new SerialDispatchQueue(Threading.ThreadPool)
  private val client                                   = new OkHttpClient()

  private val deserializingParameters = ResponseDeserializer.Parameters(
    tmpFileGenerator =
      () => new File(s"${System.getProperty("java.io.tmpdir")}/http_client_tmp_${System.currentTimeMillis()}")
  )

  override def call[T, R](
      request: Request[T],
      uploadCallback: Option[ProgressCallback] = None,
      downloadCallback: Option[ProgressCallback] = None
  )(implicit bs: RequestSerializer[T], bd: ResponseDeserializer[R]): CancellableFuture[Response[R]] =
    CancellableFuture(bs.serialize(request))
      .recoverWith { case err: Throwable => CancellableFuture.failed(EncodingError(err)) }
      .flatMap { serializedRequest =>
        val promise = Promise[Response[Body]]
        new CancellableFuture(promise) {
          private val okCall = client.newCall(convertHttpRequest(serializedRequest, uploadCallback, BufferSize))
          okCall.enqueue(new Callback {
            override def onResponse(call: Call, response: okhttp3.Response): Unit =
              promise.trySuccess(convertOkHttpResponse(response, downloadCallback))
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
        Try(bd.deserialize(response, deserializingParameters)) match {
          case Success(result) => CancellableFuture.successful(result)
          case Failure(err)    => CancellableFuture.failed(DecodingError(err))
        }
      }

  override def decodedResult[T: RequestSerializer, R: ResponseDeserializer](
      request: Request[T],
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
