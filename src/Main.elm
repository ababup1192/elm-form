module Main exposing (main)

import Browser
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { registerForm : RegisterForm
    , pageState : PageState
    }


type PageState
    = Registering
    | FixingRegisterErrors
    | ShowGoats


type alias RegisterForm =
    { age : Input
    }


inputEmpty : Input
inputEmpty =
    Input ""


type Input
    = Input String


inputToString : Input -> String
inputToString (Input v) =
    v


type alias Goat =
    { age : Age
    }


type Age
    = Age Int


ageToString : Age -> String
ageToString (Age n) =
    String.fromInt n


decoder : Decoder RegisterForm Error Goat
decoder =
    Decoder.map Goat
        decoderAge


decoderAge : Decoder RegisterForm Error Age
decoderAge =
    ageDecoder_
        |> Decoder.mapError AgeError
        |> required AgeRequired
        |> Decoder.lift .age


type Error
    = AgeError AgeError
    | AgeRequired


required : err -> Decoder String err a -> Decoder Input err a
required err d =
    Decoder.with <|
        \(Input a) ->
            case a of
                "" ->
                    Decoder.fail err

                _ ->
                    Decoder.lift inputToString d


ageDecoder_ : Decoder String AgeError Age
ageDecoder_ =
    Decoder.int InvalidInt
        |> Decoder.assert (Decoder.minBound Negative 0)
        |> Decoder.map Age


type AgeError
    = InvalidInt
    | Negative


init : () -> ( Model, Cmd Msg )
init _ =
    ( { registerForm =
            { age = inputEmpty
            }
      , pageState = Registering
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Elm 0.19 starter"
    , body =
        case model.pageState of
            Registering ->
                [ div [] [] ]

            FixingRegisterErrors ->
                [ div [] [] ]

            ShowGoats ->
                [ div [] [] ]
    }



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
