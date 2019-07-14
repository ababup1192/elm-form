module Goat.Age exposing (Age(..), Error(..), decoder, errorField)

import Form.Decoder as Decoder exposing (Decoder)


{-| IntにDecodeして、バリデーションして、Ageにmap
-}
type Age
    = Age Int


decoder : Decoder String Error Age
decoder =
    Decoder.int InvalidInt
        |> Decoder.assert (Decoder.minBound Negative 0)
        |> Decoder.map Age


type Error
    = InvalidInt
    | Negative


errorField : Error -> String
errorField err =
    case err of
        InvalidInt ->
            "Invalid input."

        Negative ->
            "Age must not be negative number."
