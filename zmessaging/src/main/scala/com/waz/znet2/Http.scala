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
    case object Get extends Method
    case object Post extends Method
    case object Put extends Method
    case object Delete extends Method
    case object Patch extends Method
    case object Head extends Method
  }


  case class ResponseCode(value: Int) extends AnyVal {
    import ResponseCode._
    def isSuccessful: Boolean = List(Success, Created, NoResponse).contains(value)
  }

  object ResponseCode {
    val Success               = 200
    val Created               = 201
    val NoResponse            = 204
    val MovedPermanently      = 301
    val MovedTemporarily      = 302
    val SeeOther              = 303
    val BadRequest            = 400
    val Unauthorized          = 401
    val Forbidden             = 403
    val NotFound              = 404
    val RateLimiting          = 420
    val LoginRateLimiting     = 429
    val Conflict              = 409
    val PreconditionFailed    = 412
    val InternalServerError   = 500
  }

  case class Headers(headers: Map[String, String] = Map.empty[String, String]){
    assert(!headers.keys.exists(key => Headers.toLower(key) != key)) //TODO Do we care?

    def get(key: String): Option[String] = headers.get(Headers.toLower(key))

    def foreach(key: String)(f: String => Unit): Unit = headers.get(Headers.toLower(key)).foreach(f)

    override def toString: String = s"Headers[$headers]"
  }

  object Headers {
    val empty = Headers()

    def create(entries: (String, String)*): Headers = create(entries.toMap)
    def create(headers: Map[String, String]): Headers = Headers(headers.map { case (key, value) => toLower(key) -> value })

    private def toLower(key: String) = key.toLowerCase(Locale.US) //TODO Do we need this?
  }

  case class Body(mediaType: Option[String], data: InputStream, dataLength: Option[Long] = None)

  sealed trait EmptyBody

}

object HttpRequest {

  def map[A,B](a: HttpRequest[A])(f: A => B): HttpRequest[B] = a.copy(body = a.body.map(f))

  def lift[A,B](f: A => B): HttpRequest[A] => HttpRequest[B] = map(_)(f)

  def create[T](url: URL,
                queryParameters: List[(String, String)] = List.empty,
                method: Http.Method = Http.Method.Get,
                headers: Http.Headers = Http.Headers.empty,
                body: Option[T] = None): HttpRequest[T] = {
    //TODO Refactor this conversions
    val urlWithParameters = new URL(queryParameters.foldLeft(URI.parse(url.toString).buildUpon){ case (builder, (key, value)) =>
      builder.appendQueryParameter(key, value)
    }.build.toString)
    new HttpRequest(urlWithParameters, method, headers, body)
  }

  def withoutBody(url: URL,
                  queryParameters: List[(String, String)] = List.empty,
                  method: Http.Method = Http.Method.Get,
                  headers: Http.Headers = Http.Headers.empty): HttpRequest[Http.EmptyBody] = {
    create(url, queryParameters, method, headers)
  }

}

case class HttpRequest[T](url: URL, httpMethod: Http.Method, headers: Http.Headers, body: Option[T]) {
  def map[B](f: T => B): HttpRequest[B] = HttpRequest.map(this)(f)
}

object HttpResponse {

  def map[A,B](a: HttpResponse[A])(f: A => B): HttpResponse[B] = a.copy(body = a.body.map(f))

  def lift[A,B](f: A => B): HttpResponse[A] => HttpResponse[B] = map(_)(f)

}

case class HttpResponse[T](code: Http.ResponseCode, headers: Http.Headers, body: Option[T]) {
  def map[B](f: T => B): HttpResponse[B] = HttpResponse.map(this)(f)
}
