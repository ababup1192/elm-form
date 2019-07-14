module Goat.GoatTests exposing (decoderTest)

import Expect exposing (Expectation)
import Form.Decoder as Decoder
import Fuzz exposing (Fuzzer, int, list, string)
import Goat.Age exposing (..)
import Goat.Goat as Goat exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    describe "Goat.decoder"
        [ test "FormからGoatにdecodeできる" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "15" } }
                in
                Decoder.run Goat.decoder registerForm
                    |> Expect.equal (Ok <| Goat (Age 15))
        , test "どれか一つでもFormの値がバリデーションに引っかかる場合、decodeに失敗する" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "" } }
                in
                Decoder.run Goat.decoder registerForm
                    |> Expect.equal (Err <| [ AgeRequired ])
        ]
