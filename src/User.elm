module User exposing (User, userDecoder, minPasswordChars)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias User =
    { avatar : String
    , collectionId : String
    , collectionName : String
    , created : String
    , emailVisibility : Bool
    , id : String
    , name : String
    , role : String
    , updated : String
    , verified : Bool
    }


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "avatar" Decode.string
        |> required "collectionId" Decode.string
        |> required "collectionName" Decode.string
        |> required "created" Decode.string
        |> required "emailVisibility" Decode.bool
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "role" Decode.string
        |> required "updated" Decode.string
        |> required "verified" Decode.bool
