package io.swagger.petstore

import argonaut.Argonaut._
import argonaut.CodecJson

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
case class Category(id: Option[Long],
                name: Option[String]
                )

object Category {
  /**
   * Creates the codec for converting Category from and to JSON.
   */
  implicit val CategoryCodec: CodecJson[Category] = casecodec2(Category.apply, Category.unapply)("id", "name")
}
