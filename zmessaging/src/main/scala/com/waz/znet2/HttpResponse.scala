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

import com.waz.utils.JsonDecoder
import com.waz.znet2.Http.SerializedBody
import org.json.JSONObject

case class HttpResponse(code: Http.ResponseCode, headers: Http.Headers, body: Option[SerializedBody])

object HttpResponse {

  trait BodyDeserializer[T] extends (SerializedBody => T)
  object BodyDeserializer {

    def apply[T](f: SerializedBody => T): BodyDeserializer[T] = new BodyDeserializer[T] {
      override def apply(v1: SerializedBody): T = f(v1)
    }

    //TODO Not contramap, find proper name
    def contramap[A,B](f: B => A)(implicit bd: BodyDeserializer[B]): BodyDeserializer[A] =
      apply(body => f(bd(body)))

    implicit val BytesBodyDeserializer: BodyDeserializer[Array[Byte]] =
      apply(body => body.data)

    implicit val JsonBodySerializer: BodyDeserializer[JSONObject] =
      apply(body => new JSONObject(new String(body.data)))

    implicit def objectToJsonBodyDeserializer[T](implicit d: JsonDecoder[T]): BodyDeserializer[T] =
      contramap((json: JSONObject) => d(json))

  }

}
