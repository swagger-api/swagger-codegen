package Java

import java.util.{Collections, Comparator}

import com.wordnik.swagger.codegen.CodegenResponse
import com.wordnik.swagger.codegen.languages.JavaClientCodegen
import com.wordnik.swagger.models._
import com.wordnik.swagger.models.properties.StringProperty
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class JavaOperationTest extends FlatSpec with Matchers {

  it should "convert an operation response" in {
    val operation = new Operation().response(200, new Response()
      .description("success")
      .schema(new StringProperty())
      .example("first", "first example")
      .example("second", "second example")
    ).response(404, new Response()
      .description("not-found")
      .schema(new StringProperty()))

    val codegen = new JavaClientCodegen()
    val co = codegen.fromOperation("/path", "get", operation)

    co.path should be("/path")
    co.httpMethod should be("GET")

    val responses = co.responses
    responses.size() should be(2)
    responses.get(0).hasMore should equal(true)
    responses.get(1).hasMore should equal(false)
    Collections.sort(responses, new Comparator[CodegenResponse] {
      override def compare(o1: CodegenResponse, o2: CodegenResponse): Int = o1.code.compareTo(o2.code)
    })

    val resp1 = responses.get(0)
    resp1.code should be("200")
    resp1.message should be("success")
    // TODO schema
    // TODO examples

    val resp2 = responses.get(1)
    resp2.code should be("404")
    resp2.message should be("not-found")
  }
}