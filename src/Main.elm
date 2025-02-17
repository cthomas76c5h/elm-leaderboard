module Main exposing (main)

import Api exposing (Cred)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode exposing (Value)
import Page
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)


type Model
    = Redirect Session
    | Home Home.Model
    | NotFound Session
    | Login Login.Model
    | Register Register.Model


init : Maybe Cred -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeCred url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromCred navKey maybeCred))


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.viewer (toSession model)

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view viewer page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view viewer Page.Other Blank.view
        
        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        NotFound _ ->
            Page.view viewer Page.Other NotFound.view

        Login login ->
            viewPage Page.Other GotLoginMsg (Login.view login)

        Register register ->
            viewPage Page.Other GotRegisterMsg (Register.view register)


type Msg
    = ChangedUrl Url
    | GotHomeMsg Home.Msg
    | ClickedLink Browser.UrlRequest
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        Register register ->
            Register.toSession register


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg model



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Register register ->
            Sub.map GotRegisterMsg (Register.subscriptions register)


main : Program Value Model Msg
main =
    Api.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
