package io.swagger.petstore

import argonaut.Argonaut._
import argonaut.CodecJson

/**
 * A User who is purchasing from the pet store
 * @param id 
 * @param username 
 * @param firstName 
 * @param lastName 
 * @param email 
 * @param password 
 * @param phone 
 * @param userStatus User Status
 */
case class User(id: Option[Long],
                username: Option[String],
                firstName: Option[String],
                lastName: Option[String],
                email: Option[String],
                password: Option[String],
                phone: Option[String],
                userStatus: Option[Int]
                )

object User {
  /**
   * Creates the codec for converting User from and to JSON.
   */
  implicit val UserCodec: CodecJson[User] = casecodec8(User.apply, User.unapply)("id", "username", "firstName", "lastName", "email", "password", "phone", "userStatus")
}
