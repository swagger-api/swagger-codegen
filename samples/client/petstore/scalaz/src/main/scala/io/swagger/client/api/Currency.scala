package io.swagger.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime
import Currency._

case class Currency (
  
object Currency {
  import DateTimeCodecs._

  implicit val CurrencyCodecJson: CodecJson[Currency] = CodecJson.derive[Currency]
  implicit val CurrencyDecoder: EntityDecoder[Currency] = jsonOf[Currency]
  implicit val CurrencyEncoder: EntityEncoder[Currency] = jsonEncoderOf[Currency]
}
