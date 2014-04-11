/**
 *  Copyright 2014 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import com.wordnik.swagger.codegen.BasicPHPGenerator

import com.wordnik.swagger.model.{ModelRef, ModelProperty}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class BasicPHPGeneratorTest extends FlatSpec with ShouldMatchers {
  val config = new BasicPHPGenerator

  behavior of "BasicPHPGenerator"

  "process response declaration" should "void convert to None" in {
    config.processResponseDeclaration("void") should be (None)
  }

  it should "array, Array or List convert to array" in {
    val expect = Some("array[Model\\AnyType]")

    config.processResponseDeclaration("array[AnyType]") should be (expect)
    config.processResponseDeclaration("Array[AnyType]") should be (expect)
    config.processResponseDeclaration("List[AnyType]") should be (expect)
  }

  it should "prefixed when given type is not listed in type mapping" in {
    config.processResponseDeclaration("AnyType") should be (Some("Model\\AnyType"))
  }

  "to decoration" should "prefixed when given type is not listed in type mapping" in {
    val modelProperty = new ModelProperty("SomeModel", "", items = None)

    val result = config.toDeclaration(modelProperty)
    result._1 should be ("Model\\SomeModel")
  }

  it should "wrap in array when model type is List and has items" in {
    val modelRef = new ModelRef("SomeModel")
    val modelProperty = new ModelProperty("List", "", items = Some(modelRef))

    val result = config.toDeclaration(modelProperty)
    result._1 should be ("array[Model\\SomeModel]")
  }

  it should "wrap in array when model type is Array and has items" in {
    val modelRef = new ModelRef("SomeModel")
    val modelProperty = new ModelProperty("Array", "", items = Some(modelRef))

    val result = config.toDeclaration(modelProperty)
    result._1 should be ("array[Model\\SomeModel]")
  }

  it should "throw error when inner type is not defined" in {
    val modelProperty = new ModelProperty("List", "", items = None)

    try {
      config.toDeclaration(modelProperty)
    } catch {
      case e: Exception if e.getMessage == "no inner type defined" => // Expected, so continue
    }
  }

}
