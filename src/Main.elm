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


ageErrorField : AgeError -> List String
ageErrorField err =
    case err of
        InvalidInt ->
            [ "Invalid input."
            , "Please input integer."
            ]

        Negative ->
            [ "Age must not be negative number."
            , "Please input positive integer."
            ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { registerForm =
            { age = Input "1"
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
                [ div []
                    [ registerFormView model
                    ]
                ]

            ShowGoats ->
                [ div [] [] ]
    }


registerFormView : Model -> Html Msg
registerFormView model =
    let
        { registerForm } =
            model

        hasError : Error -> Bool
        hasError err =
            case Decoder.run decoder registerForm of
                Ok _ ->
                    False

                Err errs ->
                    List.member err errs
    in
    div []
        [ inputErrorField
            ageErrorField
            ageDecoder_
            registerForm.age
        ]


inputErrorField : (err -> List String) -> Decoder String err a -> Input -> Html Msg
inputErrorField f d input =
    case decodeField d input of
        Ok _ ->
            emptyElement

        Err errs ->
            errorField <|
                List.map f errs


errorField : List (List String) -> Html msg
errorField errs =
    List.map
        (wrap2
            << List.map (\s -> p [ class "errorField_p" ] [ text s ])
        )
        errs
        |> div
            [ class "errorField"
            ]


wrap2 : List (Html msg) -> Html msg
wrap2 children =
    div
        []
        children


decodeField : Decoder String err a -> Input -> Result (List err) (Maybe a)
decodeField =
    Decoder.run << optional


optional : Decoder String err a -> Decoder Input err (Maybe a)
optional d =
    Decoder.with <|
        \(Input a) ->
            case a of
                "" ->
                    Decoder.always Nothing

                _ ->
                    Decoder.lift inputToString <| Decoder.map Just <| d


emptyElement : Html msg
emptyElement =
    text ""



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
