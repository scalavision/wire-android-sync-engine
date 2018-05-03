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

import com.waz.znet2.Http.Method._
import com.waz.znet2.Http.SerializedBody
import okhttp3.{Headers => OkHeaders, MediaType => OkMediaType, Request => OkRequest, RequestBody => OkRequestBody, Response => OkResponse, WebSocket => OkWebSocket}

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

  def convertHttpRequestBody(body: SerializedBody): OkRequestBody = {
    OkRequestBody.create(convertMediaType(body.mediaType), body.data)
  }

  def convertHttpRequest(request: HttpRequest): OkRequest = {
    new OkRequest.Builder()
      .url(request.url)
      .method(convertHttpMethod(request.httpMethod), request.body.map(convertHttpRequestBody).orNull)
      .headers(OkHeaders.of(request.headers.headers.asJava))
      .build()
  }

  def convertResponseCode(code: Int): Http.ResponseCode = {
    if (List(200, 201, 204).contains(code)) Http.SuccessResponseCode(code)
    else Http.FailedResponseCode(code)
  }

  def convertOkHttpResponse(response: OkResponse): HttpResponse = {
    HttpResponse(
      code = convertResponseCode(response.code()),
      headers = Http.Headers.create(response.headers().toMultimap.asScala.mapValues(_.asScala.head).toMap),
      body = Option(response.body()).map(body => SerializedBody(Option(body.contentType()).map(_.toString).getOrElse(""), body.bytes()))
    )
  }

}
