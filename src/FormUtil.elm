module FormUtil exposing (Input, decodeField, empty, required)

import Form.Decoder as Decoder exposing (Decoder)


{-| requiredCheckは、Blur時にTrueになる
-}
type alias Input =
    { requiredCheck : Bool, value : String }


empty : Input
empty =
    Input False ""


{-| 空の値だったら、受け取ったRequiredErrorとする
-}
required : err -> Decoder String err a -> Decoder Input err a
required err d =
    Decoder.with <|
        \{ requiredCheck, value } ->
            case value of
                "" ->
                    Decoder.fail err

                _ ->
                    Decoder.lift .value d


decodeField : Decoder String err a -> Input -> Result (List err) (Maybe a)
decodeField =
    Decoder.run << optional


optional : Decoder String err a -> Decoder Input err (Maybe a)
optional d =
    Decoder.with <|
        \{ requiredCheck, value } ->
            let
                liftValue =
                    Decoder.lift .value <| Decoder.map Just d
            in
            case String.trim value of
                "" ->
                    if requiredCheck then
                        liftValue

                    else
                        Decoder.always Nothing

                _ ->
                    liftValue
