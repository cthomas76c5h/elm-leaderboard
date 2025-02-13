module Avatar exposing (Avatar, decoder, defaultAvatar, encode, src, toMaybeString, toString, fromString)

import Asset
import Html exposing (Attribute)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


-- TYPES


type Avatar
    = Avatar (Maybe String)



-- CREATE


decoder : Decoder Avatar
decoder =
    Decode.map Avatar (Decode.nullable Decode.string)



-- TRANSFORM

fromString : String -> Avatar
fromString url =
    Avatar (Just url)


toString : Avatar -> String
toString (Avatar maybeUrl) =
    case maybeUrl of
        Just url ->
            url
        Nothing ->
            "assets/images/default-avatar.png"


encode : Avatar -> Value
encode (Avatar maybeUrl) =
    case maybeUrl of
        Just url ->
            Encode.string url

        Nothing ->
            Encode.null


src : Avatar -> Attribute msg
src (Avatar maybeUrl) =
    case maybeUrl of
        Nothing ->
            Asset.src Asset.defaultAvatar

        Just "" ->
            Asset.src Asset.defaultAvatar

        Just url ->
            Html.Attributes.src url


toMaybeString : Avatar -> Maybe String
toMaybeString (Avatar maybeUrl) =
    maybeUrl

defaultAvatar : Avatar
defaultAvatar =
    Avatar (Just "assets/images/default-avatar.png")
