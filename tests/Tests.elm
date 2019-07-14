module Tests exposing (goatDecoderTest)

import Expect exposing (Expectation)
import Form.Decoder as Decoder
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


goatDecoderTest : Test
goatDecoderTest =
    describe "Goat.decoder"
        [ test "FormからGoatにdecodeできる" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "15" } }
                in
                Decoder.run decoder registerForm
                    |> Expect.equal (Ok <| Goat (Age 15))
        ]
