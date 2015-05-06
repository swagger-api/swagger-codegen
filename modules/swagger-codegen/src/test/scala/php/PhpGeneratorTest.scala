package php

import com.wordnik.swagger.codegen.languages.PhpClientCodegen
import org.junit.runner.RunWith
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class PhpGeneratorTest extends FlatSpec with Matchers {

    it should "change host names to slashes" in {
      val generator = new PhpClientCodegen()

      val hostName = "dynamic.yoyodyne.com"
      val expected = "yoyodyne\\dynamic"

      generator.hostToNamespace(hostName) should be (expected)
    }
}
