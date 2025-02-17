port module Session exposing (Session, changes, cred, fromCred, navKey, viewer, storeUser)

import Api exposing (Cred)
import Browser.Navigation as Nav
import Json.Encode as Encode
import Platform.Cmd exposing (Cmd)
import User exposing (User)


-- TYPES

type Session
    = LoggedIn Nav.Key Cred
    | Guest Nav.Key


-- INFO

{-| Returns the Cred for a LoggedIn session; otherwise Nothing.
    (The name "viewer" is retained for backwards compatibility.)
-}
viewer : Session -> Maybe Cred
viewer session =
    case session of
        LoggedIn _ myCred ->
            Just myCred

        Guest _ ->
            Nothing


{-| An alias for `viewer`. -}
cred : Session -> Maybe Cred
cred =
    viewer


{-| Extracts the navigation key from the session. -}
navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key



-- CHANGES

{-| Listens for changes to the stored authentication data (now a Cred)
    and produces a new Session based on the updated value.
-}
changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeCred -> toMsg (fromCred key maybeCred))


{-| Given a navigation key and a possible Cred, returns a new Session.
    If there is a Cred then the session is LoggedIn; otherwise it is Guest.
-}
fromCred : Nav.Key -> Maybe Cred -> Session
fromCred key maybeCred =
    case maybeCred of
        Just newCred ->
            LoggedIn key newCred

        Nothing ->
            Guest key


-- PORTS

{-| A port to send the user data to JavaScript (for example, to store in localStorage). -}
port storeUserPort : Encode.Value -> Cmd msg


{-| Given a User record, encode it as JSON and send it out the port.
    This function lets you “store” the user on the client.
-}
storeUser : User -> Cmd msg
storeUser user =
    storeUserPort (encodeUser user)


{-| Helper function to encode a User record as JSON.
    This encoder should match the JSON response from your server:
    {
        "avatar": "",
        "collectionId": "_pb_users_auth_",
        "collectionName": "users",
        "created": "2025-02-17 20:35:08.782Z",
        "emailVisibility": false,
        "id": "xcn1q90665dybc5",
        "name": "john doe",
        "role": "",
        "updated": "2025-02-17 20:35:08.782Z",
        "verified": false
    }
-}
encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "avatar", Encode.string user.avatar )
        , ( "collectionId", Encode.string user.collectionId )
        , ( "collectionName", Encode.string user.collectionName )
        , ( "created", Encode.string user.created )
        , ( "emailVisibility", Encode.bool user.emailVisibility )
        , ( "id", Encode.string user.id )
        , ( "name", Encode.string user.name )
        , ( "role", Encode.string user.role )
        , ( "updated", Encode.string user.updated )
        , ( "verified", Encode.bool user.verified )
        ]
