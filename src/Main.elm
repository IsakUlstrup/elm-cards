module Main exposing (..)

import Browser
import Deck exposing (Deck)
import Html exposing (Attribute, Html, button, div, h1, li, p, text, ul)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Plant exposing (Plant)
import Random



---- CARD ----


type alias Card =
    { name : String
    , description : String
    , operations : List CardOperation
    }


type CardOperation
    = Water Float
    | Fertilize Float
    | Light Float
    | Temperature Float
    | Grow Float


rainCard : Card
rainCard =
    Card "🌧️" "It's raining, water plant" [ Water 1, Light 40, Temperature 20 ]


droughtCard : Card
droughtCard =
    Card "☀️" "It's super hot, water evaporates" [ Water -1, Light 80, Temperature 60 ]


monsoonCard : Card
monsoonCard =
    Card "⛈️" "A monsoon!, lots of water" [ Water 5, Light 40, Temperature 30 ]


cowCard : Card
cowCard =
    Card "🐄" "A cow passes by and poops everywhere" [ Fertilize 2 ]


pandaCard : Card
pandaCard =
    Card "🐼" "A panda nibbles on your plant before falling asleep" [ Grow -1 ]


passTimeCard : Card
passTimeCard =
    Card "⏳" "Time passes" [ Grow 1 ]



---- PLAYER ----


type alias Player =
    Plant


applyCard : Card -> Player -> Player
applyCard card player =
    let
        applyOperation : CardOperation -> Player -> Player
        applyOperation op plr =
            case op of
                Water n ->
                    plr |> Plant.water n

                Fertilize n ->
                    plr |> Plant.fertilize n

                Light n ->
                    plr |> Plant.setLight n

                Temperature n ->
                    plr |> Plant.setTemperature n

                Grow n ->
                    plr |> Plant.grow n
    in
    List.foldr applyOperation player card.operations



---- MODEL ----


environmentCards : List Card
environmentCards =
    [ droughtCard, droughtCard, passTimeCard, droughtCard, monsoonCard, pandaCard, droughtCard, droughtCard, monsoonCard ]


playerCards : List Card
playerCards =
    [ rainCard, passTimeCard, cowCard, passTimeCard, rainCard, rainCard, cowCard, cowCard ]


type HandState
    = Draw (Deck Card)
    | Select Int (Deck Card)
    | Play Int (Deck Card)


type alias Model =
    { hands : List ( Int, List String )
    , index : Int
    , player : Player
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ ( 0, [ "hei" ] ) ]
        1
        Plant.new
        (Random.initialSeed 42)
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NextHand


maybeDraw : Deck Card -> Maybe (Deck Card)
maybeDraw deck =
    Just (Deck.discardDraw 3 deck)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextHand ->
            ( { model | hands = List.take 2 (( model.index, [ "hei", "hei", "hei" ] ) :: model.hands), index = model.index + 1 }, Cmd.none )



---- VIEW ----


viewHands : List ( Int, List String ) -> Html msg
viewHands hands =
    Html.Keyed.node "ul" [ Html.Attributes.class "hands" ] (List.map viewKeyedHand hands)


viewKeyedHand : ( Int, List String ) -> ( String, Html msg )
viewKeyedHand ( i, hand ) =
    ( String.fromInt i, Html.Lazy.lazy viewHand ( i, hand ) )


viewHand : ( Int, List String ) -> Html msg
viewHand ( i, strings ) =
    let
        card c =
            p [] [ text (String.fromInt i ++ c) ]
    in
    li [ Html.Attributes.class "hand" ] (List.map card strings)


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "app" ]
        [ button [ Html.Events.onClick NextHand ] [ text "next hand" ]
        , viewHands model.hands
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
