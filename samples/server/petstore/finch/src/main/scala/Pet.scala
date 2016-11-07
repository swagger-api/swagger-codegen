package io.swagger.petstore

import argonaut.Argonaut._
import argonaut.CodecJson
//import Category
//import Tag

/**
 * A pet for sale in the pet store
 * @param id 
 * @param category 
 * @param name 
 * @param photoUrls 
 * @param tags 
 * @param status pet status in the store
 */
case class Pet(id: Option[Long],
                category: Option[Category],
                name: String,
                photoUrls: List[String],
                tags: Option[List[Tag]],
                status: Option[String]
                )

object Pet {
  /**
   * Creates the codec for converting Pet from and to JSON.
   */
  implicit val PetCodec: CodecJson[Pet] = casecodec6(Pet.apply, Pet.unapply)("id", "category", "name", "photoUrls", "tags", "status")
}
