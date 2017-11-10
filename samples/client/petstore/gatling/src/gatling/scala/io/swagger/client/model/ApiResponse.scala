
package io.swagger.client.model


case class ApiResponse (
    code: Option[Integer],
    _type: Option[String],
    message: Option[String]
)
object ApiResponse {
    def toStringBody(code: Object, _type: Object, message: Object) =
        s"""
        | {
        | "code":$code,"_type":$_type,"message":$message
        | }
        """.stripMargin
}
