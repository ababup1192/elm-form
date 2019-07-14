module Main exposing (Model, Msg(..), PageState(..), emptyElement, enableRequrieCheck, errorField, init, inputErrorField, main, registerFormView, setAge, setValue, update, view)

import Browser
import Form.Decoder as Decoder exposing (Decoder)
import FormUtil as FormUtil
import Goat.Age as Age
import Goat.Goat as Goat exposing (Error(..), RegisterForm)
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { registerForm =
            { age = FormUtil.empty
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
            ( { model
                | registerForm =
                    setAge (setValue age registerForm.age) registerForm
              }
            , Cmd.none
            )

        BlurAge ->
            ( { model
                | registerForm =
                    setAge (enableRequrieCheck registerForm.age) registerForm
              }
            , Cmd.none
            )

        SubmitForm ->
            let
                newRegisterForm =
                    { registerForm
                        | age = enableRequrieCheck registerForm.age
                    }

                hasError =
                    not <| List.isEmpty <| Decoder.errors Goat.decoder registerForm

                newPageState =
                    if hasError then
                        Registering

                    else
                        ShowGoats
            in
            ( { model | registerForm = newRegisterForm, pageState = newPageState }, Cmd.none )


setAge : FormUtil.Input -> RegisterForm -> RegisterForm
setAge age registerForm =
    { registerForm | age = age }


setValue : String -> FormUtil.Input -> FormUtil.Input
setValue newValue input =
    { requiredCheck = input.requiredCheck, value = newValue }


enableRequrieCheck : FormUtil.Input -> FormUtil.Input
enableRequrieCheck input =
    { requiredCheck = True, value = input.value }



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

        hasError : Goat.Error -> Bool
        hasError err =
            case Decoder.run Goat.decoder registerForm of
                Ok _ ->
                    False

                Err errs ->
                    List.member err errs
    in
    div [ class "form" ]
        [ inputErrorField
            Age.errorField
            Age.decoder
            registerForm.age
            (hasError AgeRequired)
        , input
            [ placeholder "age"
            , onInput UpdateAge
            , onBlur BlurAge
            , value <| registerForm.age.value
            ]
            []
        , button [ onClick SubmitForm ] [ text "submit" ]
        ]


inputErrorField : (err -> String) -> Decoder String err a -> FormUtil.Input -> Bool -> Html Msg
inputErrorField f d input hasRequiredErr =
    case FormUtil.decodeField d input of
        Ok _ ->
            emptyElement

        Err errs ->
            if hasRequiredErr then
                errorField "(required)"

            else
                case List.head errs of
                    Just err ->
                        errorField <| f err

                    Nothing ->
                        emptyElement


errorField : String -> Html msg
errorField err =
    div
        [ class "errorField"
        ]
        [ p [] [ text err ]
        ]


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
