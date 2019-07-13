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


type alias Input =
    { requiredCheck : Bool, value : String }


inputEmpty : Input
inputEmpty =
    Input False ""


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
        \{ requiredCheck, value } ->
            case value of
                "" ->
                    Decoder.fail err

                _ ->
                    Decoder.lift .value d


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
    = UpdateAge String
    | BlurAge
    | SubmitForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { registerForm } =
            model
    in
    case msg of
        UpdateAge age ->
            let
                newRegisterForm =
                    { registerForm | age = Input registerForm.age.requiredCheck age }
            in
            ( { model | registerForm = newRegisterForm }, Cmd.none )

        BlurAge ->
            let
                newRegisterForm =
                    { registerForm | age = Input True registerForm.age.value }
            in
            ( { model | registerForm = newRegisterForm }, Cmd.none )

        SubmitForm ->
            let
                newRegisterForm =
                    { registerForm
                        | age = Input True registerForm.age.value
                    }

                hasError =
                    not <| List.isEmpty <| Decoder.errors decoder registerForm

                newPageState =
                    if hasError then
                        Registering

                    else
                        ShowGoats
            in
            ( { model | registerForm = newRegisterForm, pageState = newPageState }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Form"
    , body =
        case model.pageState of
            Registering ->
                [ div [ class "main" ]
                    [ registerFormView model
                    ]
                ]

            ShowGoats ->
                [ h1 [] [ text "Submitted!" ] ]
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
    div [ class "form" ]
        [ inputErrorField
            ageErrorField
            ageDecoder_
            registerForm.age
        , requiredField registerForm.age.requiredCheck <| hasError AgeRequired
        , input
            [ placeholder "age"
            , onInput UpdateAge
            , onBlur BlurAge
            , value <| registerForm.age.value
            ]
            []
        , button [ onClick SubmitForm ] [ text "submit" ]
        ]


inputErrorField : (err -> List String) -> Decoder String err a -> Input -> Html Msg
inputErrorField f d input =
    case decodeField d input of
        Ok _ ->
            emptyElement

        Err errs ->
            errorField <|
                List.map f errs


requiredField : Bool -> Bool -> Html Msg
requiredField requiredCheck hasRequiredError =
    if requiredCheck && hasRequiredError then
        p [] [ text "(required)" ]

    else
        emptyElement


errorField : List (List String) -> Html msg
errorField errs =
    List.map
        (wrap2
            << List.map (\s -> p [] [ text s ])
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
        \{ requiredCheck, value } ->
            case value of
                "" ->
                    Decoder.always Nothing

                _ ->
                    Decoder.lift .value <| Decoder.map Just <| d


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
