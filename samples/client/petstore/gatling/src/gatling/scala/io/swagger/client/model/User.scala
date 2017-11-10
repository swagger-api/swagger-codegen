
package io.swagger.client.model


case class User (
    id: Option[Long],
    username: Option[String],
    firstName: Option[String],
    lastName: Option[String],
    email: Option[String],
    password: Option[String],
    phone: Option[String],
    /* User Status */
    userStatus: Option[Integer]
)
object User {
    def toStringBody(id: Object, username: Object, firstName: Object, lastName: Object, email: Object, password: Object, phone: Object, userStatus: Object) =
        s"""
        | {
        | "id":$id,"username":$username,"firstName":$firstName,"lastName":$lastName,"email":$email,"password":$password,"phone":$phone,"userStatus":$userStatus
        | }
        """.stripMargin
}
