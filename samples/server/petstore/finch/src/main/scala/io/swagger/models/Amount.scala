package io.swagger.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import io.swagger._
import io.swagger.models.Currency

/**
 * some description 
 * @param value some description 
 * @param currency 
 */
case class Amount(value: Double,
                currency: Currency
                )

object Amount {
    /**
     * Creates the codec for converting Amount from and to JSON.
     */
    implicit val decoder: Decoder[Amount] = deriveDecoder
    implicit val encoder: ObjectEncoder[Amount] = deriveEncoder
}
