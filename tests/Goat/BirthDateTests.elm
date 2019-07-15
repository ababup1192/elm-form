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
                        { year = Select 2019, month = Select 4, day = Select 1 }
                in
                Decoder.run decoder birthDateForm
                    |> Expect.equal (Ok <| BirthDate { year = 2019, month = 4, day = 1 })
        , test "すべて空の場合、BirthDateNothingとなる" <|
            \_ ->
                let
                    birthDateForm =
                        { year = UnSelected, month = UnSelected, day = UnSelected }
                in
                Decoder.run decoder birthDateForm
                    |> Expect.equal (Ok BirthDateNothing)
        , test "入力が欠けている場合、BirthDateのdecodeに失敗する" <|
            \_ ->
                let
                    birthDateForm =
                        { year = Select 2019, month = UnSelected, day = Select 1 }
                in
                Decoder.run decoder birthDateForm
                    |> Expect.equal (Err <| [ MissingError ])
        ]


errorFieldTest : Test
errorFieldTest =
    describe "Goat.BirthDate.errorField"
        [ test "パラメータが欠けていることに対するメッセージ" <|
            \_ ->
                errorField MissingError
                    |> Expect.equal "Missing parameter."
        ]
