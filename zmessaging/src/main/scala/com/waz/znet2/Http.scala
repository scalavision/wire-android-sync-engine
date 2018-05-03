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

import java.util.Locale

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

  sealed trait ResponseCode
  case class SuccessResponseCode(value: Int) extends ResponseCode
  case class FailedResponseCode(value: Int) extends ResponseCode

  object ResponseCode {
    val Success               = SuccessResponseCode(200)
    val Created               = SuccessResponseCode(201)
    val NoResponse            = SuccessResponseCode(204)
    val MovedPermanently      = FailedResponseCode(301)
    val MovedTemporarily      = FailedResponseCode(302)
    val SeeOther              = FailedResponseCode(303)
    val BadRequest            = FailedResponseCode(400)
    val Unauthorized          = FailedResponseCode(401)
    val Forbidden             = FailedResponseCode(403)
    val NotFound              = FailedResponseCode(404)
    val RateLimiting          = FailedResponseCode(420)
    val LoginRateLimiting     = FailedResponseCode(429)
    val Conflict              = FailedResponseCode(409)
    val PreconditionFailed    = FailedResponseCode(412)
    val InternalServerError   = FailedResponseCode(500)
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

  case class SerializedBody(mediaType: String, data: Array[Byte])

}
