module Session exposing (Session, changes, cred, fromCred, navKey, viewer)

import Api exposing (Cred)
import Browser.Navigation as Nav



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
