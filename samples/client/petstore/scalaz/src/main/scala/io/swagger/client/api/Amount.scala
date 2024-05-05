package io.swagger.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime
import Amount._

case class Amount (
  /* some description  */
  value: Double,
currency: Currency)

object Amount {
  import DateTimeCodecs._

  implicit val AmountCodecJson: CodecJson[Amount] = CodecJson.derive[Amount]
  implicit val AmountDecoder: EntityDecoder[Amount] = jsonOf[Amount]
  implicit val AmountEncoder: EntityEncoder[Amount] = jsonEncoderOf[Amount]
}
