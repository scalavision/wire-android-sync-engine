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

import java.io.InputStream

import com.waz.utils.IoUtils
import com.waz.znet2.Http.Method._
import com.waz.znet2.HttpClient.{Progress, ProgressCallback}
import okhttp3.{Headers => OkHeaders, MediaType => OkMediaType, Request => OkRequest, RequestBody => OkRequestBody, Response => OkResponse, WebSocket => OkWebSocket}
import okio.BufferedSink

import scala.collection.JavaConverters._

object OkHttpConverters {

  def convertHttpMethod(method: Http.Method): String = {
    method match {
      case Get      => "GET"
      case Post     => "POST"
      case Put      => "PUT"
      case Patch    => "PATCH"
      case Delete   => "DELETE"
      case Head     => "HEAD"
    }
  }

  def convertMediaType(mediatype: String): OkMediaType = {
    OkMediaType.parse(mediatype)
  }

  private def createRequestBody(body: Http.RawBody, callback: Option[ProgressCallback], bufferSize: Int): OkRequestBody = {
    createCustomRequestBody(
      callback,
      body.mediaType.map(convertMediaType),
      bufferSize,
      body.data,
      body.dataLength
    )
  }

  def convertHttpRequest(request: Http.Request[Http.RawBody], callback: Option[ProgressCallback], bufferSize: Int): OkRequest = {
    createOkHttpRequest(request, createRequestBody(_, callback, bufferSize))
  }

  private def createOkHttpRequest(request: Http.Request[Http.RawBody], bodyConverter: Http.RawBody => OkRequestBody): OkRequest = {
    new OkRequest.Builder()
      .url(request.url)
      .method(convertHttpMethod(request.httpMethod), request.body.map(bodyConverter).orNull)
      .headers(OkHeaders.of(request.headers.headers.asJava))
      .build()
  }

  def convertResponseCode(code: Int): Http.ResponseCode = Http.ResponseCode(code)

  def convertOkHttpResponse(response: OkResponse, callback: Option[ProgressCallback]): Http.Response[Http.RawBody] = {
    Http.Response(
      code = convertResponseCode(response.code()),
      headers = Http.Headers.create(response.headers().toMultimap.asScala.mapValues(_.asScala.head).toMap),
      body = Option(response.body()).map { body =>
        val data = body.byteStream()
        val dataLength = if (body.contentLength() == -1) None else Some(body.contentLength())
        Http.RawBody(
          mediaType = Option(body.contentType()).map(_.toString),
          data = callback.map(createProgressInputStream(_, data, dataLength)).getOrElse(data),
          dataLength = dataLength
        )
      }
    )
  }

  def createCustomRequestBody(callback: Option[ProgressCallback],
                              mediaType: Option[OkMediaType],
                              bufferSize: Int,
                              data: InputStream,
                              dataLength: Option[Long]): OkRequestBody = new OkRequestBody() {
    override val contentType: OkMediaType = mediaType.orNull
    override val contentLength: Long = dataLength.getOrElse(-1)

    def writeTo(sink: BufferedSink): Unit = IoUtils.copy(
      in = callback.map(createProgressInputStream(_, data, dataLength)).getOrElse(data),
      out = sink.outputStream()
    )
  }

  private def createProgressInputStream(callback: ProgressCallback, data: InputStream, dataLength: Option[Long]): ProgressInputStream = {
    new ProgressInputStream(data, new ProgressInputStream.Listener {
      override def progressUpdated(bytesRead: Long): Unit = {
        callback(Progress(bytesRead, dataLength))
      }
    })
  }

}
