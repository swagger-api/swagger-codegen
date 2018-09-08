module DateTime exposing (DateTime, dateTimeDecoder, dateTimeEncoder)

import Iso8601 exposing (fromTime, toTime)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time exposing (Posix)


type alias DateTime =
    Posix


dateTimeDecoder : Decoder DateTime
dateTimeDecoder =
    Decode.string
        |> Decode.andThen decodeIsoString


dateTimeEncoder : DateTime -> Encode.Value
dateTimeEncoder model =
    Encode.string <| fromTime model


decodeIsoString : String -> Decoder DateTime
decodeIsoString str =
    case toTime str of
        Ok date ->
            Decode.succeed date

        Err e ->
            Decode.fail <|
                "Cannot convert "
                    ++ str
                    ++ " to DateTime"
