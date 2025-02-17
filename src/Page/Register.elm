module Page.Register exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (register)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session, storeUser, changes, navKey)
import User exposing (User, userDecoder, minPasswordChars)


-- MODEL

type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }


type alias Form =
    { email : String
    , name : String
    , password : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = []
      , form =
            { email = ""
            , name = ""
            , password = ""
            }
      }
    , Cmd.none
    )



-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                        , p [ class "text-xs-center" ]
                            [ a [ Route.href Route.Login ]
                                [ text "Have an account?" ]
                            ]
                        , ul [ class "error-messages" ]
                            (List.map viewProblem model.problems)
                        , viewForm model.form
                        ]
                    ]
                ]
            ]
    }


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , placeholder "Name"
                , onInput EnteredName
                , value form.name
                ]
                []
            ]
        , fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , placeholder "Email"
                , onInput EnteredEmail
                , value form.email
                ]
                []
            ]
        , fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , type_ "password"
                , placeholder "Password"
                , onInput EnteredPassword
                , value form.password
                ]
                []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign up" ]
        ]


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    li [] [ text errorMessage ]



-- UPDATE

type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredName String
    | EnteredPassword String
    | CompletedRegister (Result Http.Error User)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Http.send CompletedRegister (register validForm)
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedRegister (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            ( { model | problems = List.append model.problems serverErrors }
            , Cmd.none
            )

        CompletedRegister (Ok user) ->
            ( model
            , storeUser user  -- now storing the User instead of a Viewer
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (navKey session) Route.Home
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    changes GotSession (navKey model.session)



-- EXPORT

toSession : Model -> Session
toSession model =
    model.session



-- FORM

{-| Indicates that we've trimmed the form's fields.
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Name
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Name
    , Email
    , Password
    ]


{-| Trim the form and validate its fields.
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Name ->
                if String.isEmpty form.name then
                    [ "name can't be blank." ]
                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]
                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]
                else if String.length form.password < minPasswordChars then
                    [ "password must be at least " ++ String.fromInt minPasswordChars ++ " characters long." ]
                else
                    []


{-| Only trim on submit (so that typing isn’t interrupted).
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { name = String.trim form.name
        , email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP

register : TrimmedForm -> Http.Request User
register (Trimmed form) =
    let
        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                , ( "passwordConfirm", Encode.string form.password )
                ]
                |> Http.jsonBody
    in
    Api.register body userDecoder
