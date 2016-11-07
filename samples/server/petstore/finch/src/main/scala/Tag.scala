package io.swagger.petstore

import argonaut.Argonaut._
import argonaut.CodecJson

/**
 * A tag for a pet
 * @param id 
 * @param name 
 */
case class Tag(id: Option[Long],
                name: Option[String]
                )

object Tag {
  /**
   * Creates the codec for converting Tag from and to JSON.
   */
  implicit val TagCodec: CodecJson[Tag] = casecodec2(Tag.apply, Tag.unapply)("id", "name")
}
