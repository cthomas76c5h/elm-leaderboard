module Page exposing (Page(..), view, viewErrors)

import Api exposing (Cred)
import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, i, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Username exposing (Username)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.
-}
type Page
    = Other
    | Home
    | Login
    | Register
    | Settings
    | Profile Username
    | NewArticle


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current credentials so we can display in either
"signed in" (rendering username and avatar) or "signed out" mode.
-}
view : Maybe Cred -> Page -> { title : String, content : Html msg } -> Document msg
view maybeCred page { title, content } =
    { title = title ++ " - Conduit"
    , body = [ viewHeader page maybeCred, content, viewFooter ]
    }


viewHeader : Page -> Maybe Cred -> Html msg
viewHeader page maybeCred =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ]
                ( navbarLink page Route.Home [ text "Home" ]
                    :: viewMenu page maybeCred
                )
            ]
        ]


viewMenu : Page -> Maybe Cred -> List (Html msg)
viewMenu page maybeCred =
    let
        linkTo =
            navbarLink page
    in
    case maybeCred of
        Just _ ->
            [ linkTo Route.Home [ i [ class "ion-compose" ] [], text "\u{00A0}New Post" ]
            , linkTo Route.Home [ i [ class "ion-gear-a" ] [], text "\u{00A0}Settings" ]
            , linkTo Route.Logout [ text "Sign out" ]
            ]

        Nothing ->
            [ linkTo Route.Login [ text "Sign in" ]
            , linkTo Route.Home [ text "Sign up" ]
            ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]


navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        _ ->
            False


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
            (List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]
            )
