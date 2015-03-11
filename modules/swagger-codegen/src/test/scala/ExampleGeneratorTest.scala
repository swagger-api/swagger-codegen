import com.wordnik.swagger.models._
import com.wordnik.swagger.util.Json

import com.wordnik.swagger.codegen.examples._
import com.wordnik.swagger.models.properties._
import io.swagger.parser._

import javax.xml.bind.annotation._
import javax.xml.bind.JAXBContext
import javax.xml.bind.JAXBException
import javax.xml.bind.Marshaller

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

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
      .name("user")
      .property("name", new StringProperty())
      .property("id", id)
      .property("address", new RefProperty("Address"))

    val xml = new XmlExampleGenerator(
      Map[String, Model](
        "User" -> user,
        "Address" -> address,
        "Phone" -> phone).asJava)
    println(xml.toXml(new RefProperty("User")))
  }
}

@RunWith(classOf[JUnitRunner])
class PojoGenerator extends FlatSpec with Matchers {
  it should "write xml" in {
    val phone = new Phone()
    phone.number = "(650) 949-7777"

    val address = new Address()
    address.street = "12345 El Monte Blvd"
    address.phone = phone

    val user = new User();
    user.name = "dog"
    user.id = 123L
    user.address = address

    val jaxbContext = JAXBContext.newInstance( classOf[User] )
    val jaxbMarshaller = jaxbContext.createMarshaller()
    jaxbMarshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, true )
    jaxbMarshaller.marshal( user, System.out )
  }
}

@XmlRootElement
class User {
  @XmlElement
  var name: String = _
  @XmlAttribute
  var id: Long = _
  @XmlElement
  var address: Address = _
}

@XmlRootElement
class Address {
  @XmlElement
  var street: String = _
  @XmlElement
  var city: String = _
  @XmlElement
  var state: String = _
  @XmlElement
  var zip: String = _
  @XmlElement
  var phone: Phone = _
}

@XmlRootElement
class Phone {
  @XmlElement
  var number: String = _
}