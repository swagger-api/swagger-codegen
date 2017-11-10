
package io.swagger.client.model

import io.swagger.client.model.Category
import io.swagger.client.model.List
import io.swagger.client.model.Tag

case class Pet (
    id: Option[Long],
    category: Option[Category],
    name: String,
    photoUrls: List[String],
    tags: Option[List[Tag]],
    /* pet status in the store */
    status: Option[String]
)
object Pet {
    def toStringBody(id: Object, category: Object, name: Object, photoUrls: Object, tags: Object, status: Object) =
        s"""
        | {
        | "id":$id,"category":$category,"name":$name,"photoUrls":$photoUrls,"tags":$tags,"status":$status
        | }
        """.stripMargin
}
