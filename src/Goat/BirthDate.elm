module Goat.BirthDate exposing (BirthDate(..), Error(..), decoder, errorField)

import Form.Decoder as Decoder exposing (Decoder, Validator)


type BirthDate
    = BirthDateNothing
    | BirthDate BirthDate_


type alias BirthDate_ =
    { year : Int, month : Int, day : Int }


type alias BirthDateForm =
    { yearStr : String, monthStr : String, dayStr : String }


decoder : Decoder BirthDateForm Error BirthDate
decoder =
    let
        stringDefined : String -> Bool
        stringDefined =
            not << String.isEmpty
    in
    Decoder.with <|
        \{ yearStr, monthStr, dayStr } ->
            if List.all String.isEmpty [ yearStr, monthStr, dayStr ] then
                Decoder.always BirthDateNothing

            else if List.all stringDefined [ yearStr, monthStr, dayStr ] then
                Decoder.map3 BirthDate_
                    (Decoder.lift .yearStr <| Decoder.int InvalidInt)
                    (Decoder.lift .monthStr <| Decoder.int InvalidInt)
                    (Decoder.lift .dayStr <| Decoder.int InvalidInt)
                    |> Decoder.map BirthDate

            else
                Decoder.fail MissingError


type Error
    = InvalidInt
    | MissingError


errorField : Error -> String
errorField err =
    case err of
        MissingError ->
            "Missing parameter."

        _ ->
            "Invalid input."
