package io.swagger.petstore

/**
 * The parent error from which most PetstoreAPI errors extend. Thrown whenever something in the api goes wrong.
 */
abstract class PetstoreError(msg: String) extends Exception(msg) {
  def message: String
}
