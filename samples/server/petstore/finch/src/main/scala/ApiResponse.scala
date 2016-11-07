package io.swagger.petstore

import argonaut.Argonaut._
import argonaut.CodecJson

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param _type 
 * @param message 
 */
case class ApiResponse(code: Option[Int],
                _type: Option[String],
                message: Option[String]
                )

object ApiResponse {
  /**
   * Creates the codec for converting ApiResponse from and to JSON.
   */
  implicit val ApiResponseCodec: CodecJson[ApiResponse] = casecodec3(ApiResponse.apply, ApiResponse.unapply)("code", "type", "message")
}
