port module Auth exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Platform.Cmd exposing (Cmd)


{-| Our user record type.
-}
type alias User =
    { avatar : String
    , email : String
    , id : String
    , name : String
    }


{-| The complete auth data type containing the token and the user record.
-}
type alias AuthData =
    { token : String
    , record : User
    }


{-| JSON decoder for the User type.
-}
userDecoder : Decoder User
userDecoder =
    Decode.map4 User
        (Decode.field "avatar" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


{-| JSON decoder for the complete AuthData.
-}
authDataDecoder : Decoder AuthData
authDataDecoder =
    Decode.map2 AuthData
        (Decode.field "token" Decode.string)
        (Decode.field "record" userDecoder)


{-| Port to send the auth data to JavaScript, which should handle saving it to local storage.
-}
port storeAuthInLocalStorage : Encode.Value -> Cmd msg


{-| JSON encoder for the User type.
-}
encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "avatar", Encode.string user.avatar )
        , ( "email", Encode.string user.email )
        , ( "id", Encode.string user.id )
        , ( "name", Encode.string user.name )
        ]


{-| Helper function to convert our AuthData into JSON and send it over the port.
-}
storeAuth : AuthData -> Cmd msg
storeAuth authData =
    let
        json =
            Encode.object
                [ ( "token", Encode.string authData.token )
                , ( "record", encodeUser authData.record )
                ]
    in
    storeAuthInLocalStorage json


{-| Example function that takes a JSON string, decodes it into AuthData,
    and then sends it to the port to be stored in local storage.
-}
decodeAndStoreAuthData : String -> Cmd msg
decodeAndStoreAuthData jsonString =
    case Decode.decodeString authDataDecoder jsonString of
        Ok authData ->
            storeAuth authData

        Err _ ->
            -- Handle your error here (for example, logging or a no-op)
            Cmd.none
