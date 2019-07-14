module Goat.Goat exposing (Error(..), Goat, RegisterForm, decoder)

import Form.Decoder as Decoder exposing (Decoder)
import FormUtil as FormUtil
import Goat.Age as Age exposing (Age)


{-| フォームの値を表す型 InputのRecord
{ age = { requiredCheck: True, value = "15" } }
-}
type alias RegisterForm =
    { age : FormUtil.Input
    }


{-| Decode後にマッピングされる型
-}
type alias Goat =
    { age : Age
    }


{-| FormDecoder
-}
decoder : Decoder RegisterForm Error Goat
decoder =
    Decoder.map Goat
        decoderAge


{-| Requiredのハンドリングを特別扱いしつつ、.ageにLiftする
-}
decoderAge : Decoder RegisterForm Error Age
decoderAge =
    Age.decoder
        |> Decoder.mapError AgeError
        |> FormUtil.required AgeRequired
        |> Decoder.lift .age


{-| すべてのDecoderのエラーをまとめてる。Requiredを特別扱いしたいので外に出す。
-}
type Error
    = AgeError Age.Error
    | AgeRequired
