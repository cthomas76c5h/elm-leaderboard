module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view, scrollToTop)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article.Tag as Tag exposing (Tag)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Leaderboard exposing (Salesman, leaderboardDecoder, leaderboardView)
import Loading
import Log
import Page
import Session exposing (Session)
import Task exposing (Task)
import Time


-- MODEL

type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , feedTab : FeedTab
    , feedPage : Int

      -- Loaded independently from server
    , tags : Status (List Tag)
    , leaderboard : Status (List Salesman)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type FeedTab
    = YourFeed Cred
    | GlobalFeed


-- MESSAGES

type Msg
    = CompletedTagsLoad (Result Http.Error (List Tag))
    | CompletedLeaderboardLoad (Result Http.Error (List Salesman))
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


-- INIT

init : Session -> ( Model, Cmd Msg )
init session =
    let
        feedTab =
            case Session.cred session of
                Just cred ->
                    YourFeed cred

                Nothing ->
                    GlobalFeed

        leaderboardStatus =
            case Session.cred session of
                Just _ ->
                    Loading

                Nothing ->
                    Failed
    in
    ( { session = session
      , timeZone = Time.utc
      , feedTab = feedTab
      , feedPage = 1
      , tags = Loading
      , leaderboard = leaderboardStatus
      }
    , Cmd.batch
        [ Tag.list
            |> Http.send CompletedTagsLoad
        , (case Session.cred session of
                Just cred ->
                    Api.get Endpoint.leaderboard (Just cred) leaderboardDecoder
                        |> Http.send CompletedLeaderboardLoad

                Nothing ->
                    Cmd.none
          )
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedTagsLoad (Ok tags) ->
            ( { model | tags = Loaded tags }, Cmd.none )

        CompletedTagsLoad (Err _) ->
            ( { model | tags = Failed }
            , Log.error
            )

        CompletedLeaderboardLoad (Ok lb) ->
            ( { model | leaderboard = Loaded lb }, Cmd.none )

        CompletedLeaderboardLoad (Err _) ->
            ( { model | leaderboard = Failed }
            , Log.error
            )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        PassedSlowLoadThreshold ->
            let
                tagsStatus =
                    case model.tags of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | tags = tagsStatus }, Cmd.none )


-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Conduit"
    , content =
        div [ class "home-page" ]
            [ viewBanner
            , if Session.cred model.session /= Nothing then
                div [ class "container" ]
                    [ div [ class "leaderboard-section" ]
                        [ h2 [] [ text "Leaderboard" ]
                        , case model.leaderboard of
                              Loaded lb ->
                                  leaderboardView lb

                              Loading ->
                                  text "Loading leaderboard..."

                              LoadingSlowly ->
                                  Loading.icon

                              Failed ->
                                  text "Failed to load leaderboard"
                        ]
                    ]
              else
                text ""
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ]
                        [ Loading.error "feed" ]
                    , div [ class "col-md-3" ]
                        (case model.tags of
                            Loaded _ ->
                                [ div [ class "sidebar" ]
                                    [ p [] [ text "Popular Tags" ]
                                    ]
                                ]

                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "tags" ]
                        )
                    ]
                ]
            ]
    }


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]


-- SCROLL TO TOP

scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        |> Task.onError (\_ -> Task.succeed ())


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
