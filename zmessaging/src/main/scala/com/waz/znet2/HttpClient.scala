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

import java.io.IOException

import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.znet2.HttpResponse.BodyDeserializer
import okhttp3.{Call, Callback, OkHttpClient}

import scala.concurrent.{ExecutionContext, Promise}
import scala.util.{Failure, Success, Try}

object HttpClient {

  sealed trait HttpClientError extends Throwable
  case class ResponseError(code: Http.FailedResponseCode) extends HttpClientError
  case class ConnectionError(err: Throwable) extends HttpClientError
  case class DecodingError(err: Throwable) extends HttpClientError
  case object EmptyBodyError extends HttpClientError

}

trait HttpClient {
  def call(request: HttpRequest): CancellableFuture[HttpResponse]
  def decodedResult[T](request: HttpRequest)(implicit bd: BodyDeserializer[T]): CancellableFuture[T]
}

class HttpClientOkHttpImpl extends HttpClient {
  import HttpClient._
  import OkHttpConverters._

  private implicit val dispatcher: SerialDispatchQueue = new SerialDispatchQueue(Threading.ThreadPool)
  private val client = new OkHttpClient()

  override def call(request: HttpRequest): CancellableFuture[HttpResponse] = {
    val promise = Promise[HttpResponse]
    new CancellableFuture[HttpResponse](promise) {
      private val okRequest = client.newCall(convertHttpRequest(request))
      okRequest.enqueue(new Callback {
        override def onResponse(call: Call, response: okhttp3.Response): Unit = {
          promise.trySuccess(convertOkHttpResponse(response))
        }
        override def onFailure(call: Call, e: IOException): Unit = {
          promise.tryFailure(ConnectionError(e))
        }
      })

      override def onCancelled(body: => Unit)(implicit executor: ExecutionContext): Unit = {
        super.onCancelled(body)
        okRequest.cancel()
      }
    }
  }

  override def decodedResult[T](request: HttpRequest)(implicit bd: BodyDeserializer[T]): CancellableFuture[T] = {
    call(request).flatMap { response =>
      Try(response.body.map(bd.apply)) match {
        case Success(Some(decoded)) => CancellableFuture.successful(decoded)
        case Success(None)          => CancellableFuture.failed(EmptyBodyError)
        case Failure(err)           => CancellableFuture.failed(DecodingError(err))
      }
    }
  }

}
