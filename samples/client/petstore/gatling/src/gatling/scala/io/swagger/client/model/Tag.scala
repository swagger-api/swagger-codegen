
package io.swagger.client.model


case class Tag (
    id: Option[Long],
    name: Option[String]
)
object Tag {
    def toStringBody(id: Object, name: Object) =
        s"""
        | {
        | "id":$id,"name":$name
        | }
        """.stripMargin
}
