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

import java.net.URL

import com.waz.utils.JsonEncoder
import com.waz.utils.wrappers.URI
import com.waz.znet2.Http.SerializedBody
import org.json.JSONObject

case class HttpRequest(url: URL, httpMethod: Http.Method, headers: Http.Headers, body: Option[SerializedBody])

object HttpRequest {

  def withoutBody(url: URL,
                  queryParameters: List[(String, String)] = List.empty,
                  httpMethod: Http.Method = Http.Method.Get,
                  headers: Http.Headers = Http.Headers.empty): HttpRequest = create(url, queryParameters, httpMethod, headers, None)

  def apply[T](url: URL,
               queryParameters: List[(String, String)] = List.empty,
               httpMethod: Http.Method = Http.Method.Get,
               headers: Http.Headers = Http.Headers.empty,
               body: T)
              (implicit bs: BodySerializer[T]): HttpRequest = create(url, queryParameters, httpMethod, headers, Some(bs(body)))


  private def create(url: URL,
                     queryParameters: List[(String, String)],
                     httpMethod: Http.Method,
                     headers: Http.Headers,
                     body: Option[SerializedBody]): HttpRequest = {
    //TODO Refactor this conversions
    val urlWithParameters = new URL(queryParameters.foldLeft(URI.parse(url.toString).buildUpon){ case (builder, (key, value)) =>
      builder.appendQueryParameter(key, value)
    }.build.toString)
    new HttpRequest(urlWithParameters, httpMethod, headers, body)
  }

  trait BodySerializer[T] extends (T => SerializedBody)
  object BodySerializer {

    def apply[T](f: T => SerializedBody): BodySerializer[T] = new BodySerializer[T] {
      override def apply(v1: T): SerializedBody = f(v1)
    }

    def contramap[A,B](f: A => B)(implicit bs: BodySerializer[B]): BodySerializer[A] =
      apply(obj => bs(f(obj)))

    implicit val BytesBodySerializer: BodySerializer[Array[Byte]] =
      apply(bytes => SerializedBody(Http.MediaType.Bytes, bytes))

    implicit val JsonBodySerializer: BodySerializer[JSONObject] =
      apply(json => SerializedBody(Http.MediaType.Json, json.toString.getBytes("utf8")))

    implicit def objectToJsonBodySerializer[T](implicit e: JsonEncoder[T]): BodySerializer[T] =
      contramap(e.apply)

  }

}
