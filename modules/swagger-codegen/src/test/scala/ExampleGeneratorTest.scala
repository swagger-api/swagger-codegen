import com.wordnik.swagger.models._
import com.wordnik.swagger.util.Json

import com.wordnik.swagger.codegen.examples._
import com.wordnik.swagger.models.properties._
import io.swagger.parser._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import javax.xml.bind.annotation._

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class ExampleGeneratorTest extends FlatSpec with Matchers {
  it should "do xml magic" in {
    val id = new IntegerProperty()
      .example(135)
    id.setXml(new Xml().attribute(true))
    val phone = new ModelImpl()
      .name("phone")
      .property("number", new StringProperty().example("(650) 949-7777"))

    val address = new ModelImpl()
      .name("address")
      .property("street", new StringProperty().example("12345 El Monte Blvd"))
      .property("city", new StringProperty().example("Los Altos Hills"))
      .property("state", new StringProperty().example("CA"))
      .property("zip", new StringProperty().example("94022"))
      .property("phones", new ArrayProperty().items(new RefProperty("Phone")))

    val user = new ModelImpl()
      .name("User")
      .property("name", new StringProperty())
      .property("id", id)
      .property("address", new RefProperty("Address"))
      .xml(new Xml().name("user"))

    val xml = new XmlExampleGenerator(
      Map[String, Model](
        "User" -> user,
        "Address" -> address,
        "Phone" -> phone).asJava)
    val xmlString = xml.toXml(new RefProperty("User"))

    xmlString should be(
"""<user id="135">
  <name>string</name>
  <address>
    <street>12345 El Monte Blvd</street>
    <city>Los Altos Hills</city>
    <state>CA</state>
    <zip>94022</zip>
    <phone>
      <number>(650) 949-7777</number>
    </phone>
  </address>
</user>""")
  }
}

class User {
  var name: String = _
  @XmlAttribute
  var id: Long = _
  var address: Address = _
}

class Address {
  var street: String = _
  var city: String = _
  var state: String = _
  var zip: String = _
  var phone: Phone = _
}

class Phone {
  var number: String = _
}