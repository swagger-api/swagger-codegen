
package io.swagger.client.model

import java.util.Date

case class Order (
    id: Option[Long],
    petId: Option[Long],
    quantity: Option[Integer],
    shipDate: Option[Date],
    /* Order Status */
    status: Option[String],
    complete: Option[Boolean]
)
object Order {
    def toStringBody(id: Object, petId: Object, quantity: Object, shipDate: Object, status: Object, complete: Object) =
        s"""
        | {
        | "id":$id,"petId":$petId,"quantity":$quantity,"shipDate":$shipDate,"status":$status,"complete":$complete
        | }
        """.stripMargin
}
