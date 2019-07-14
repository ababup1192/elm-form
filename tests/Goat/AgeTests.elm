module Goat.AgeTests exposing (decoderTest, errorFieldTest)

import Expect exposing (Expectation)
import Form.Decoder as Decoder
import Fuzz exposing (Fuzzer, int, list, string)
import Goat.Age exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    describe "Goat.Age.decoder"
        [ test "正常に年齢にdecodeできる" <|
            \_ ->
                let
                    str =
                        "15"
                in
                Decoder.run decoder str
                    |> Expect.equal (Ok <| Age 15)
        , test "数値ではない場合、年齢のdecodeに失敗する" <|
            \_ ->
                let
                    str =
                        "aaa"
                in
                Decoder.run decoder str
                    |> Expect.equal (Err <| [ InvalidInt ])
        , test "負の数値の場合、年齢のdecodeに失敗する" <|
            \_ ->
                let
                    str =
                        "-1"
                in
                Decoder.run decoder str
                    |> Expect.equal (Err <| [ Negative ])
        ]


errorFieldTest : Test
errorFieldTest =
    describe "Goat.Age.errorField"
        [ test "数値変換エラーに対するメッセージ" <|
            \_ ->
                let
                    str =
                        "15"
                in
                errorField InvalidInt
                    |> Expect.equal "Invalid input."
        , test "負の値に対するエラーメッセージ" <|
            \_ ->
                let
                    str =
                        "15"
                in
                errorField Negative
                    |> Expect.equal "Age must not be negative number."
        ]
