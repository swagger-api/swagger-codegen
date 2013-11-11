/**
 *  Copyright 2012 Wordnik, Inc.
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

import com.wordnik.swagger.codegen._

import java.io.File

object PHPPetstoreCodegen extends BasicPHPGenerator {
  def main(args: Array[String]) = generateClient(args)

  // PHP uses backslash and not the dot as namespace separator
  override def packageSeparator = "\\"

  // location of templates
  override def templateDir = "php-modern"

  // where to write generated code
  override def destinationDir = "samples/client/petstore/php-modern"

  // package for invoker
  override def invokerPackage = Some("com\\wordnik\\client")

  // package for models
  override def modelPackage = Some("com\\wordnik\\petstore\\model")

  // package for api classes
  override def apiPackage = Some("com\\wordnik\\petstore\\api")

  override def supportingFiles = List(
    ("APIClient.mustache", destinationDir + File.separator + toPathName(invokerPackage.get) + File.separator, "APIClient.php")
  )
}
