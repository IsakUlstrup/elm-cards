module Main exposing (..)

import Browser
import Deck exposing (Deck)
import Html exposing (Html, button, div, h1, li, p, text, ul)
import Html.Attributes
import Html.Events
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
    [ droughtCard, passTimeCard, droughtCard, passTimeCard, droughtCard, monsoonCard, pandaCard, droughtCard, droughtCard, passTimeCard, monsoonCard ]


playerCards : List Card
playerCards =
    [ rainCard, cowCard, rainCard, rainCard, cowCard, cowCard ]


type alias Model =
    { decks : List (Deck Card)
    , activeDeckIndex : Int
    , player : Player
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ Deck.new "Player deck" playerCards
        , Deck.new "Environment" environmentCards
        ]
        0
        Plant.new
        (Random.initialSeed 42)
        |> newDeckSelection
        |> activeDeckDraw
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectCard Card


{-| Select index of random deck from list
-}
randomDeckIndex : List (Deck Card) -> Random.Generator Int
randomDeckIndex decks =
    Random.int 0 (List.length decks - 1)


activeDeckDraw : Model -> Model
activeDeckDraw model =
    let
        -- If activeIndex matches index, draw cards from deck
        drawHelper : Int -> Int -> Deck Card -> Deck Card
        drawHelper activeIndex index deck =
            if activeIndex == index then
                deck |> Deck.discardDraw 3

            else
                deck
    in
    { model | decks = List.indexedMap (drawHelper model.activeDeckIndex) model.decks }


newDeckSelection : Model -> Model
newDeckSelection model =
    let
        ( deckIndex, seed ) =
            Random.step (randomDeckIndex model.decks) model.seed
    in
    { model
        | seed = seed
        , activeDeckIndex = deckIndex
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCard card ->
            ( { model | player = applyCard card model.player } |> newDeckSelection |> activeDeckDraw, Cmd.none )



---- VIEW ----


viewCard : Card -> Html Msg
viewCard card =
    button [ Html.Events.onClick (SelectCard card), Html.Attributes.class "card" ]
        [ h1 [] [ text card.name ]
        , p [] [ text card.description ]
        ]


viewDeck : Deck Card -> Html Msg
viewDeck deck =
    let
        hand =
            if List.length deck.hand > 0 then
                div [ Html.Attributes.class "hand" ] (List.map viewCard deck.hand)

            else
                div [ Html.Attributes.class "hand" ] [ text "no cards" ]
    in
    div [ Html.Attributes.class "deck" ]
        [ hand
        , p [] [ text ("deck: " ++ deck.name) ]
        , p [] [ text ("draw: " ++ String.fromInt (List.length deck.drawPile)) ]
        , p [] [ text ("discard: " ++ String.fromInt (List.length deck.discardPile)) ]
        ]


viewPlayer : Player -> Html msg
viewPlayer player =
    let
        temp t =
            if t < 0 then
                "❄️"

            else if t < 40 then
                "😎"

            else
                "🔥"

        light l =
            if l < 5 then
                "🌙"

            else if l < 20 then
                "☁️"

            else if l < 40 then
                "⛅"

            else if l < 60 then
                "🌤️"

            else
                "☀️"
    in
    div [ Html.Attributes.id "player" ]
        [ h1 [] [ text ("Plant " ++ String.fromInt (round player.growth) ++ "/100") ]
        , ul []
            [ li [] [ text ("💧 " ++ String.fromFloat player.water) ]
            , li [] [ text ("💩 " ++ String.fromFloat player.fertilizer) ]
            ]
        , ul []
            [ li [] [ text (light player.light) ]
            , li [] [ text (temp player.temperature) ]
            ]
        ]


isActiveDeck : Int -> Int -> Deck Card -> Maybe (Deck Card)
isActiveDeck activeIndex index deck =
    if activeIndex == index then
        Just deck

    else
        Nothing


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "app" ]
        [ viewPlayer model.player
        , div []
            (List.indexedMap (isActiveDeck model.activeDeckIndex) model.decks
                |> List.filterMap identity
                |> List.map viewDeck
            )
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
