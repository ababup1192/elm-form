module Goat.BirthDate exposing (BirthDate(..), Error(..), SelectInt(..), decoder, errorField)

import Form.Decoder as Decoder exposing (Decoder, Validator)


type BirthDate
    = BirthDateNothing
    | BirthDate BirthDate_


type alias BirthDate_ =
    { year : Int, month : Int, day : Int }


type SelectInt
    = UnSelected
    | Select Int


selectIntToInt : SelectInt -> Int
selectIntToInt selectInt =
    case selectInt of
        UnSelected ->
            -1

        Select int ->
            int


type alias BirthDateForm =
    { year : SelectInt, month : SelectInt, day : SelectInt }


decoder : Decoder BirthDateForm Error BirthDate
decoder =
    let
        stringDefined : String -> Bool
        stringDefined =
            not << String.isEmpty
    in
    Decoder.with <|
        \{ year, month, day } ->
            case ( year, month, day ) of
                ( Select _, Select _, Select _ ) ->
                    Decoder.map3 BirthDate_
                        (Decoder.lift .year selectedIntDecoder)
                        (Decoder.lift .month selectedIntDecoder)
                        (Decoder.lift .day selectedIntDecoder)
                        |> Decoder.map BirthDate

                ( UnSelected, UnSelected, UnSelected ) ->
                    Decoder.always BirthDateNothing

                _ ->
                    Decoder.fail MissingError


selectedIntDecoder : Decoder SelectInt Error Int
selectedIntDecoder =
    Decoder.identity |> Decoder.map selectIntToInt


type Error
    = MissingError


errorField : Error -> String
errorField err =
    case err of
        MissingError ->
            "Missing parameter."
