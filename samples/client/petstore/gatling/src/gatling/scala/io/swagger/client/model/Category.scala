
package io.swagger.client.model


case class Category (
    id: Option[Long],
    name: Option[String]
)
object Category {
    def toStringBody(id: Object, name: Object) =
        s"""
        | {
        | "id":$id,"name":$name
        | }
        """.stripMargin
}
