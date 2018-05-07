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
import java.net.URL
import java.util.Locale

import com.waz.utils.wrappers.URI

object Http {

  object MediaType {
    val Json  = "application/json"
    val Bytes = "application/octet-stream"
  }

  sealed trait Method
  object Method {
    case object Get    extends Method
    case object Post   extends Method
    case object Put    extends Method
    case object Delete extends Method
    case object Patch  extends Method
    case object Head   extends Method
  }

  case class ResponseCode(value: Int) extends AnyVal {
    import ResponseCode._
    def isSuccessful: Boolean = List(Success, Created, NoResponse).contains(value)
  }

  object ResponseCode {
    val Success             = 200
    val Created             = 201
    val NoResponse          = 204
    val MovedPermanently    = 301
    val MovedTemporarily    = 302
    val SeeOther            = 303
    val BadRequest          = 400
    val Unauthorized        = 401
    val Forbidden           = 403
    val NotFound            = 404
    val RateLimiting        = 420
    val LoginRateLimiting   = 429
    val Conflict            = 409
    val PreconditionFailed  = 412
    val InternalServerError = 500
  }

  case class Headers(headers: Map[String, String] = Map.empty[String, String]) {
    assert(!headers.keys.exists(key => Headers.toLower(key) != key)) //TODO Do we care?

    def get(key: String): Option[String] = headers.get(Headers.toLower(key))

    def foreach(key: String)(f: String => Unit): Unit = headers.get(Headers.toLower(key)).foreach(f)

    override def toString: String = s"Headers[$headers]"
  }

  object Headers {
    val empty = Headers()

    def create(entries: (String, String)*): Headers = create(entries.toMap)
    def create(headers: Map[String, String]): Headers =
      Headers(headers.map { case (key, value) => toLower(key) -> value })

    private def toLower(key: String) = key.toLowerCase(Locale.US) //TODO Do we need this?
  }

  case class RawBody(mediaType: Option[String], data: InputStream, dataLength: Option[Long] = None)

  sealed trait EmptyBody

  object Request {

    def create[T](
        url: URL,
        queryParameters: List[(String, String)] = List.empty,
        method: Http.Method = Http.Method.Get,
        headers: Http.Headers = Http.Headers.empty,
        body: Option[T] = None
    ): Request[T] = {
      //TODO Refactor this conversions
      val urlWithParameters = new URL(
        queryParameters
          .foldLeft(URI.parse(url.toString).buildUpon) {
            case (builder, (key, value)) =>
              builder.appendQueryParameter(key, value)
          }
          .build
          .toString
      )
      new Request(urlWithParameters, method, headers, body)
    }

    def withoutBody(
        url: URL,
        queryParameters: List[(String, String)] = List.empty,
        method: Method = Method.Get,
        headers: Headers = Headers.empty
    ): Request[EmptyBody] = create(url, queryParameters, method, headers)

  }

  //TODO Body should not be optional
  case class Request[T](url: URL, httpMethod: Method, headers: Headers, body: Option[T])

  //TODO Body should not be optional
  case class Response[T](code: ResponseCode, headers: Headers, body: Option[T])

}
