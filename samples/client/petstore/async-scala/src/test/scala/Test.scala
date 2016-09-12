import io.swagger.client._
import io.swagger.client.api._
import io.swagger.client.model._
import java.net.URI

import io.swagger.client.SwaggerClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import com.wordnik.swagger._
import com.wordnik.swagger.client
import com.wordnik.swagger.client.ClientResponseReaders.Json4sFormatsReader._
import com.wordnik.swagger.client.RequestWriters.Json4sFormatsWriter._
import com.wordnik.swagger.client.SwaggerConfig

@RunWith(classOf[JUnitRunner])
class SimpleTest extends FlatSpec with Matchers {
  implicit val formats = org.json4s.DefaultFormats
  implicit val reader = JsonFormatsReader
  implicit val writer = JsonFormatsWriter

  it should "call the api" in {
    val config = SwaggerConfig.forUrl(new URI("http://petstore.swagger.io/v2"))
    val client = new SwaggerClient(config)

    val future = client.pet.getPetById(3)
    val await = Await.ready(future, Duration.Inf)

    await onComplete {
      case Success(pet) => {
        println(pet)
      }
      case Failure(t) => println("failed " + t.getMessage)
    }
  }
}
