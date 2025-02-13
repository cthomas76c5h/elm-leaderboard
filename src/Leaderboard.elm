module Leaderboard exposing (Salesman, leaderboardDecoder, leaderboardView)

import Html exposing (Html, li, ul, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


{-| Represents the weekly statistics.
-}
type alias WeeklyStats =
    { currentWeek : Int
    , lastWeek : Int
    , today : Int
    }


weeklyStatsDecoder : Decoder WeeklyStats
weeklyStatsDecoder =
    Decode.succeed WeeklyStats
        |> required "currentWeek" Decode.int
        |> required "lastWeek" Decode.int
        |> required "today" Decode.int


{-| Represents a single leaderboard entry.
-}
type alias Salesman =
    { ownerId : String
    , name : String
    , lastQuarterDeals : Int
    , quarterDeals : Int
    , pendingQuarterDeals : Int
    , weeklySales : WeeklyStats
    , weeklyDeals : WeeklyStats
    , bonusEarnings : Float
    , lastBonusEarnings : Float
    , pendingBonusEarnings : Float
    , lastDealPaid : String
    , yearlyPaidDeals : Int
    }


salesmanDecoder : Decoder Salesman
salesmanDecoder =
    Decode.succeed Salesman
        |> required "ownerId" Decode.string
        |> required "name" Decode.string
        |> required "lastQuarterDeals" Decode.int
        |> required "quarterDeals" Decode.int
        |> required "pendingQuarterDeals" Decode.int
        |> required "weeklySales" weeklyStatsDecoder
        |> required "weeklyDeals" weeklyStatsDecoder
        |> required "bonusEarnings" Decode.float
        |> required "lastBonusEarnings" Decode.float
        |> required "pendingBonusEarnings" Decode.float
        |> required "lastDealPaid" Decode.string
        |> required "yearlyPaidDeals" Decode.int


{-| The leaderboard decoder expects an array of Salesman entries.
-}
leaderboardDecoder : Decoder (List Salesman)
leaderboardDecoder =
    Decode.list salesmanDecoder



{-| Render the leaderboard as an unordered list.
-}
leaderboardView : List Salesman -> Html msg
leaderboardView salesmen =
    let
        sortedSalesmen =
            List.sortBy (\s -> (0 - s.quarterDeals, s.lastDealPaid)) salesmen
    in
    ul [ class "leaderboard" ]
        (List.map viewSalesman sortedSalesmen)


{-| Render a single Salesman entry.
    You can customize this to show more details as needed.
-}
viewSalesman : Salesman -> Html msg
viewSalesman salesman =
    li []
        [ text (salesman.name ++ " – Deals this quarter: " ++ String.fromInt salesman.quarterDeals)
        ]
