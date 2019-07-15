module Goat.BirthDateTests exposing (decoderTest, errorFieldTest)

import Expect exposing (Expectation)
import Form.Decoder as Decoder
import Fuzz exposing (Fuzzer, int, list, string)
import Goat.BirthDate exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    describe "Goat.BirthDate.decoder"
        [ test "正常にdecodeできる" <|
            \_ ->
                let
                    birthDateForm =
                        { yearStr = "2019", monthStr = "4", dayStr = "1" }
                in
                Decoder.run decoder birthDateForm
                    |> Expect.equal (Ok <| BirthDate { year = 2019, month = 4, day = 1 })
        , test "すべて空の場合、BirthDateNothingとなる" <|
            \_ ->
                let
                    birthDateForm =
                        { yearStr = "", monthStr = "", dayStr = "" }
                in
                Decoder.run decoder birthDateForm
                    |> Expect.equal (Ok BirthDateNothing)
        , test "入力が欠けている場合、BirthDateのdecodeに失敗する" <|
            \_ ->
                let
                    birthDateForm =
                        { yearStr = "2019", monthStr = "", dayStr = "1" }
                in
                Decoder.run decoder birthDateForm
                    |> Expect.equal (Err <| [ MissingError ])

        -- Inputの都合上起きない
        , test "数値ではない場合、BirthDateのdecodeに失敗する" <|
            \_ ->
                let
                    birthDateForm =
                        { yearStr = "2019", monthStr = "4", dayStr = "a" }
                in
                Decoder.run decoder birthDateForm
                    |> Expect.equal (Err <| [ InvalidInt ])
        ]


errorFieldTest : Test
errorFieldTest =
    describe "Goat.BirthDate.errorField"
        [ test "パラメータが欠けていることに対するメッセージ" <|
            \_ ->
                errorField MissingError
                    |> Expect.equal "Missing parameter."
        , test "負の値に対するエラーメッセージ" <|
            \_ ->
                errorField InvalidInt
                    |> Expect.equal "Invalid input."
        ]
