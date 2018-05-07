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

import java.io.{ByteArrayInputStream, File}

import com.waz.specs.ZSpec
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet2.HttpClient.Progress
import com.waz.znet2.Http._
import okhttp3.mockwebserver.{MockResponse, MockWebServer}
import okio.{Buffer, Okio}
import org.json.JSONObject

import scala.collection.mutable.ArrayBuffer

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

    scenario("return http response when server is responding.") {
      val testResponseCode = ResponseCode(201)
      val testBodyStr = "test body"

      mockServer.enqueue(
        new MockResponse()
          .setResponseCode(testResponseCode.value)
          .setBody(testBodyStr)
      )

      val client = new HttpClientOkHttpImpl()
      val request = Request.withoutBody(mockServer.url("/test").url())

      var response: Response[Array[Byte]] = null

      noException shouldBe thrownBy { response = result { client.call[EmptyBody, Array[Byte]](request) } }

      response.code                       shouldBe testResponseCode
      response.body                       shouldBe an[Some[_]]
      new String(response.body.get)       shouldBe testBodyStr
    }

  }

  scenario("return decoded response body [Foo] when server is responding.") {
    val testResponseCode = ResponseCode(201)
    val testResponseObject = Foo(1, "ok")
    val testResponseBodyStr = fooEncoder(testResponseObject).toString

    mockServer.enqueue(
      new MockResponse()
        .setResponseCode(testResponseCode.value)
        .setBody(testResponseBodyStr)
    )

    val client = new HttpClientOkHttpImpl()
    val request = Request.withoutBody(mockServer.url("/test").url())

    val responseObjectFuture = client.decodedResult[EmptyBody, Foo](request)
    var responseObject: Foo = null
    noException shouldBe thrownBy {
      responseObject = result { responseObjectFuture }
    }

    responseObject shouldBe testResponseObject
  }

  scenario("return decoded response body [File] when server is responding.") {
    val testResponseCode = ResponseCode(201)
    val testResponseObject = Foo(1, "ok")
    val testResponseBodyStr = fooEncoder(testResponseObject).toString

    mockServer.enqueue(
      new MockResponse()
        .setResponseCode(testResponseCode.value)
        .setBody(testResponseBodyStr)
    )

    val client = new HttpClientOkHttpImpl()
    val request = Request.withoutBody(mockServer.url("/test").url())

    val responseObjectFuture = client.decodedResult[EmptyBody, File](request)
    var responseFile: File = null
    noException shouldBe thrownBy {
      responseFile = result { responseObjectFuture }
    }

    responseFile.exists() shouldBe true
    scala.io.Source.fromFile(responseFile).mkString shouldBe testResponseBodyStr
  }

  scenario("should execute upload request and call progress callback when server is responding.") {
    val testResponseCode = ResponseCode(201)
    val testRequestBody = Array.fill[Byte](100000)(1)

    mockServer.enqueue(new MockResponse().setResponseCode(testResponseCode.value))

    val client = new HttpClientOkHttpImpl()
    val request = Request.create(mockServer.url("/test").url(), method = Method.Post, body = Some(testRequestBody))

    val progressAcc = ArrayBuffer.empty[Progress]
    noException shouldBe thrownBy {
      await { client.call[Array[Byte], EmptyBody](request, uploadCallback = Some(p => progressAcc.append(p))) }
    }

    checkProgressSequence(
      progressAcc.toList,
      contentLength = testRequestBody.length
    )
  }

  scenario("should execute download request and call progress callback when server is responding.") {
    val testResponseCode = ResponseCode(200)
    val testResponseBody = Array.fill[Byte](100000)(1)
    val buffer = new Buffer()
    buffer.writeAll(Okio.source(new ByteArrayInputStream(testResponseBody)))

    mockServer.enqueue(new MockResponse().setResponseCode(testResponseCode.value).setBody(buffer))

    val client = new HttpClientOkHttpImpl()
    val request = Request.withoutBody(mockServer.url("/test").url())

    val progressAcc = ArrayBuffer.empty[Progress]
    noException shouldBe thrownBy {
      await { client.call[EmptyBody, File](request, downloadCallback = Some(p => progressAcc.append(p))) }
    }

    checkProgressSequence(
      progressAcc.toList,
      contentLength = testResponseBody.length
    )
  }

  def checkProgressSequence(list: List[Progress], contentLength: Long): Unit =
    withClue(s"Progress sequence: $list") {
      list.head.progress shouldBe 0
      list.last.isCompleted shouldBe true
      list foreach { p => p.progress should be <= p.total.getOrElse(0L) }
      list zip list.tail foreach { case (prev, curr) => prev.progress should be < curr.progress }
    }

}
