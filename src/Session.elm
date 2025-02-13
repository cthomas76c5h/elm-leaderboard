module Session exposing (Session, changes, cred, fromViewer, navKey, viewer)

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
    Api.viewerChanges (\maybeCred -> toMsg (fromViewer key maybeCred))


{-| Given a navigation key and a possible Cred, returns a new Session.
    If there is a Cred then the session is LoggedIn; otherwise it is Guest.
-}
fromViewer : Nav.Key -> Maybe Cred -> Session
fromViewer key maybeCred =
    case maybeCred of
        Just credVal ->
            LoggedIn key credVal

        Nothing ->
            Guest key
