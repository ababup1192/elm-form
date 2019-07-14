module Tests exposing (inputErrorFieldTest)

import Expect exposing (Expectation)
import Form.Decoder as Decoder
import Fuzz exposing (Fuzzer, int, list, string)
import Goat.Age as Age
import Goat.Goat as Goat
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
                inputErrorField Age.errorField Age.decoder registerForm.age False
                    |> Expect.equal emptyElement
        , test "RequiredCheck(ページを開いたとき)がFalseのとき、エラーは表示されない" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = False, value = "" } }
                in
                inputErrorField Age.errorField Age.decoder registerForm.age False
                    |> Expect.equal emptyElement
        , test "RequiredCheckがTrueのとき、必須エラーが表示される" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "" } }
                in
                inputErrorField Age.errorField Age.decoder registerForm.age True
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "errorField", Selector.text "(required)" ]
        , test "エラーがあるとき、エラー表示がされる" <|
            \_ ->
                let
                    registerForm =
                        { age = { requiredCheck = True, value = "-1" } }
                in
                inputErrorField Age.errorField Age.decoder registerForm.age False
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "errorField", Selector.text "Age must not be negative number." ]
        ]
