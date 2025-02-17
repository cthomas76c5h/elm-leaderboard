port module Api exposing 
    ( Cred
    , addServerError
    , application
    , decodeErrors
    , delete
    , get
    , login
    , logout
    , post
    , put
    , register
    , settings
    , storeCredWith
    , username
    , viewerChanges
    , decode
    , cacheStorageKey
    , credStorageKey
    )

import Api.Endpoint as Endpoint exposing (Endpoint)
import Avatar exposing (Avatar)
import Auth exposing (AuthData, authDataDecoder, storeAuth, storeAuthInLocalStorage)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body)
import Json.Decode as Decode exposing (Decoder, Value, field)
import Json.Encode as Encode
import Url exposing (Url)
import User exposing (User, userDecoder)
import Username exposing (Username)


-- CRED

{-| The authentication credentials for the Viewer (that is, the currently logged‐in user).

Now the entire auth information is provided by the Auth module:
  - A token
  - A complete user record
-}
type alias Cred =
    AuthData


username : Cred -> Username
username authData =
    -- Here we assume that the "name" field in the user record represents the username.
    Username.fromString authData.record.name


credHeader : Cred -> Http.Header
credHeader authData =
    Http.header "authorization" ("Bearer " ++ authData.token)


{-| Use the new Auth module’s decoder for our Cred type.
-}
credDecoder : Decoder Cred
credDecoder =
    authDataDecoder



-- PERSISTENCE

{-| Decode stored auth data from local storage.

It expects that local storage contains a JSON string that decodes to an AuthData.
-}
decode : Value -> Result Decode.Error Cred
decode value =
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString authDataDecoder str)


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe Cred -> msg) -> Sub msg
viewerChanges toMsg =
    onStoreChange (\value -> toMsg (decodeFromChange value))


decodeFromChange : Value -> Maybe Cred
decodeFromChange val =
    Decode.decodeValue Decode.string val
        |> Result.andThen (\str -> Decode.decodeString authDataDecoder str)
        |> Result.toMaybe



{-| Instead of encoding a limited user record plus token into a field "user"
we now update the AuthData’s user record via the Auth module’s port.
If you want to update (for example) the avatar you can “patch” the record.
-}
storeCredWith : Cred -> Avatar -> Cmd msg
storeCredWith authData avatar =
    let
        currentUser = authData.record
        updatedUser = { currentUser | avatar = Avatar.toString avatar }
        updatedAuthData = { authData | record = updatedUser }
    in
    storeAuth updatedAuthData


{-| Log out by “clearing” the stored auth.
Here we send an empty object; you might instead create a dedicated
port (say, `clearAuth`) if you prefer.
-}
logout : Cmd msg
logout =
    storeAuthInLocalStorage (Encode.object [])



-- APPLICATION

{-| Initialize your Browser.application using the stored auth data.
We assume that the flags contain a JSON string that decodes to an AuthData.
-}
application :
    { init : Maybe Cred -> Url -> Nav.Key -> ( model, Cmd msg )
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    }
    -> Program Value model msg
application config =
    let
        init flags url navKey =
            let
                maybeCred =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (\str -> Decode.decodeString authDataDecoder str)
                        |> Result.toMaybe
            in
            config.init maybeCred url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }



-- HTTP

get : Endpoint -> Maybe Cred -> Decoder a -> Http.Request a
get url maybeCred decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }


put : Endpoint -> Cred -> Body -> Decoder a -> Http.Request a
put url cred body decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


post : Endpoint -> Maybe Cred -> Body -> Decoder a -> Http.Request a
post url maybeCred body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


delete : Endpoint -> Cred -> Body -> Decoder a -> Http.Request a
delete url cred body decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


{-| Since our API responses wrap auth data under a "user" field,
we transform a Decoder (Cred -> a) into a Decoder a.
-}
decoderFromAuth : Decoder (Cred -> a) -> Decoder a
decoderFromAuth decoder =
    Decode.map2 (\fromCred cred -> fromCred cred) decoder credDecoder


login : Http.Body -> Decode.Decoder AuthData -> Http.Request AuthData
login body decoder =
    post Endpoint.login Nothing body decoder


register : Http.Body -> Decode.Decoder User -> Http.Request User
register body decoder =
    post Endpoint.users Nothing body decoder


settings : Cred -> Http.Body -> Decoder (Cred -> a) -> Http.Request a
settings cred body decoder =
    put Endpoint.user cred body (Decode.field "user" (decoderFromAuth decoder))



-- ERRORS

addServerError : List String -> List String
addServerError list =
    "Server error" :: list


{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        Http.BadStatus response ->
            let
                result =
                    Decode.decodeString (Decode.field "errors" errorsDecoder) response.body
            in
            case result of
                Ok errors ->
                    errors

                Err err ->
                    let
                        _ = Debug.log "decodeErrors: Failed to decode errors:" (Debug.toString err)
                    in
                    [ "Server error" ]

        err ->
            let
                _ = Debug.log "decodeErrors: Failed to decode errors:" (Debug.toString err)
            in
            [ "Unknown Server error" ]



errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- LOCALSTORAGE KEYS

cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"
