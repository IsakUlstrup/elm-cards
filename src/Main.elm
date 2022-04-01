module Main exposing (..)

import Browser
import Deck exposing (Deck)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes
import Html.Events
import Plant exposing (Plant)
import Random



---- CARD ----


type alias Card =
    { name : String
    , description : String
    , operation : CardOperation
    }


type CardOperation
    = Water Float
    | Fertilize Float
    | Grow


rainCard : Card
rainCard =
    Card "🌧️" "It's raining, add water to plant" (Water 1)


droughtCard : Card
droughtCard =
    Card "☀️" "It's super hot, water evaporates" (Water -1)


cowCard : Card
cowCard =
    Card "🐄" "A cow passes by and poops everywhere" (Fertilize 2)


passTimeCard : Card
passTimeCard =
    Card "⏳" "Time passes" Grow



---- PLAYER ----


type alias Player =
    Plant


applyCard : Card -> Player -> Player
applyCard card player =
    case card.operation of
        Water n ->
            player |> Plant.water n

        Fertilize n ->
            player |> Plant.fertilize n

        Grow ->
            player |> Plant.grow



---- MODEL ----


environmentCards : List Card
environmentCards =
    [ droughtCard, passTimeCard, droughtCard, passTimeCard, droughtCard, droughtCard, droughtCard, passTimeCard ]


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
    div [ Html.Attributes.id "player" ]
        [ h1 [] [ text "Plant" ]
        , p [] [ text ("water: " ++ String.fromFloat player.water) ]
        , p [] [ text ("fertilizer: " ++ String.fromFloat player.fertilizer) ]
        , p [] [ text ("growth: " ++ String.fromFloat player.growth) ]
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
