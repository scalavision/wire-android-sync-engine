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

import com.waz.specs.ZSpec
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet2.Http.SuccessResponseCode
import okhttp3.mockwebserver.{MockResponse, MockWebServer}
import org.json.JSONObject

class HttpClientSpec extends ZSpec {

  case class Foo(a: Int, b: String)

  implicit val fooEncoder: JsonEncoder[Foo] = new JsonEncoder[Foo] {
    override def apply(data: Foo): JSONObject = JsonEncoder { o =>
      o.put("a", data.a)
      o.put("b", data.b)
    }
  }

  implicit val fooDecoder: JsonDecoder[Foo] = new JsonDecoder[Foo] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject): Foo = {
      Foo(decodeInt('a), 'b)
    }
  }


  private var mockServer: MockWebServer = _

  override protected def beforeEach(): Unit = {
    mockServer = new MockWebServer()
    mockServer.start()
  }

  override protected def afterEach(): Unit = {
    mockServer.shutdown()
  }


  feature("Http client") {

    scenario("should execute request and get response successfully when server is responding.") {
      val testResponseCode = SuccessResponseCode(201)
      val testBodyStr = "test body"

      mockServer.enqueue(
        new MockResponse()
          .setResponseCode(testResponseCode.value)
          .setBody(testBodyStr)
      )

      val client = new HttpClientOkHttpImpl()
      val request = HttpRequest.withoutBody(mockServer.url("/test").url())

      var response: HttpResponse = null

      noException shouldBe thrownBy { response = result { client.call(request) } }

      response.code                       shouldBe testResponseCode
      response.body                       shouldBe an[Some[_]]
      new String(response.body.get.data)  shouldBe testBodyStr
    }

  }

  scenario("should execute request and decode response body successfully when server is responding.") {
    val testResponseCode = SuccessResponseCode(201)
    val testResponseObject = Foo(1, "ok")
    val testResponseBodyStr = fooEncoder(testResponseObject).toString

    mockServer.enqueue(
      new MockResponse()
        .setResponseCode(testResponseCode.value)
        .setBody(testResponseBodyStr)
    )

    val client = new HttpClientOkHttpImpl()
    val request = HttpRequest.withoutBody(mockServer.url("/test").url())

    val responseObjectFuture = client.decodedResult[Foo](request)
    var responseObject: Foo = null
    noException shouldBe thrownBy { responseObject = result { responseObjectFuture } }

    responseObject shouldBe testResponseObject
  }

}
