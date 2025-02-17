module Api.Endpoint exposing (Endpoint, article, articles, comment, comments, favorite, feed, login, request, tags, user, users, leaderboard)

import Article.Slug as Slug exposing (Slug)
import CommentId exposing (CommentId)
import Http
import Url.Builder exposing (QueryParameter)


request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }


-- TYPES


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "https://api.jackpot.answeringlegal.com"
        ("api" :: paths)
        queryParams
        |> Endpoint


-- ENDPOINTS


login : Endpoint
login =
    url [ "collections", "users", "auth-with-password" ] []


leaderboard : Endpoint
leaderboard =
    url [ "leaderboard" ] []

user : Endpoint
user =
    url [ "user" ] []


users : Endpoint
users =
    url [ "collections", "users", "records" ] []


-- ARTICLE ENDPOINTS


article : Slug -> Endpoint
article slug =
    url [ "articles", Slug.toString slug ] []


comments : Slug -> Endpoint
comments slug =
    url [ "articles", Slug.toString slug, "comments" ] []


comment : Slug -> CommentId -> Endpoint
comment slug commentId =
    url [ "articles", Slug.toString slug, "comments", CommentId.toString commentId ] []


favorite : Slug -> Endpoint
favorite slug =
    url [ "articles", Slug.toString slug, "favorite" ] []


articles : List QueryParameter -> Endpoint
articles params =
    url [ "articles" ] params


feed : List QueryParameter -> Endpoint
feed params =
    url [ "articles", "feed" ] params


tags : Endpoint
tags =
    url [ "tags" ] []
