package io.swagger.petstore

import argonaut.Argonaut._
import argonaut.CodecJson
//import java.util.Date

/**
 * An order for a pets from the pet store
 * @param id 
 * @param petId 
 * @param quantity 
 * @param shipDate 
 * @param status Order Status
 * @param complete 
 */
case class Order(id: Option[Long],
                petId: Option[Long],
                quantity: Option[Int],
                shipDate: Option[Date],
                status: Option[String],
                complete: Option[Boolean]
                )

object Order {
  /**
   * Creates the codec for converting Order from and to JSON.
   */
  implicit val OrderCodec: CodecJson[Order] = casecodec6(Order.apply, Order.unapply)("id", "petId", "quantity", "shipDate", "status", "complete")
}
