module Tests exposing (ageDecoderTest, ageErrorFieldTest, goatDecoderTest, inputErrorFieldTest)

import Expect exposing (Expectation)
import Form.Decoder as Decoder
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


inputErrorFieldTest : Test
inputErrorFieldTest =
    describe "inputErrorField"
        [ test "エラーが無いとき、エラーは表示されない" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "15" } }
                in
                inputErrorField ageErrorField ageDecoder_ registerForm.age False
                    |> Expect.equal emptyElement
        , test "RequiredCheck(ページを開いたとき)がFalseのとき、エラーは表示されない" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = False, value = "" } }
                in
                inputErrorField ageErrorField ageDecoder_ registerForm.age False
                    |> Expect.equal emptyElement
        , test "RequiredCheckがTrueのとき、必須エラーが表示される" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "" } }
                in
                inputErrorField ageErrorField ageDecoder_ registerForm.age True
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "errorField", Selector.text "(required)" ]
        , test "エラーがあるとき、エラー表示がされる" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "-1" } }
                in
                inputErrorField ageErrorField ageDecoder_ registerForm.age False
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "errorField", Selector.text "Age must not be negative number." ]
        ]


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
        , test "どれか一つでもFormの値がバリデーションに引っかかる場合、decodeに失敗する" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "" } }
                in
                Decoder.run decoder registerForm
                    |> Expect.equal (Err <| [ AgeRequired ])
        ]


ageDecoderTest : Test
ageDecoderTest =
    describe "Goat.Age.decoder"
        [ test "正常に年齢にdecodeできる" <|
            \_ ->
                let
                    str =
                        "15"
                in
                Decoder.run ageDecoder_ str
                    |> Expect.equal (Ok <| Age 15)
        , test "数値ではない場合、年齢のdecodeに失敗する" <|
            \_ ->
                let
                    str =
                        "aaa"
                in
                Decoder.run ageDecoder_ str
                    |> Expect.equal (Err <| [ InvalidInt ])
        , test "負の数値の場合、年齢のdecodeに失敗する" <|
            \_ ->
                let
                    str =
                        "-1"
                in
                Decoder.run ageDecoder_ str
                    |> Expect.equal (Err <| [ Negative ])
        ]


ageErrorFieldTest : Test
ageErrorFieldTest =
    describe "Goat.Age.errorField"
        [ test "数値変換エラーに対するメッセージ" <|
            \_ ->
                let
                    str =
                        "15"
                in
                ageErrorField InvalidInt
                    |> Expect.equal "Invalid input."
        , test "負の値に対するエラーメッセージ" <|
            \_ ->
                let
                    str =
                        "15"
                in
                ageErrorField Negative
                    |> Expect.equal "Age must not be negative number."
        ]
