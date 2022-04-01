module Main exposing (..)

import Browser
import Deck exposing (Deck)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes
import Html.Events
import Random



---- CARD ----


type alias Card =
    { name : String
    , description : String
    , operation : CardOperation
    , positive : Bool
    }


type CardOperation
    = Add Float
    | Subtract Float
    | Multiply Float


addCard : Float -> Card
addCard n =
    Card "➕" ("+" ++ String.fromFloat n) (Add n) True


subtractCard : Float -> Card
subtractCard n =
    Card "➖" ("-" ++ String.fromFloat n) (Subtract n) False


multiplyCard : Bool -> Float -> Card
multiplyCard positive n =
    Card "✖️" ("*" ++ String.fromFloat n) (Multiply n) positive



---- PLAYER ----


type alias Player =
    Float


applyCard : Card -> Player -> Player
applyCard card player =
    case card.operation of
        Add n ->
            player + n

        Subtract n ->
            player - n

        Multiply n ->
            player * n



---- MODEL ----


environmentCards : List Card
environmentCards =
    [ subtractCard 1
    , subtractCard 2
    , subtractCard 3
    , multiplyCard False 0.9
    , multiplyCard False 0.8
    , multiplyCard False 0.7
    ]


playerCards : List Card
playerCards =
    [ addCard 1
    , addCard 2
    , addCard 3
    , multiplyCard True 1.1
    , multiplyCard True 1.2
    , multiplyCard True 2
    ]


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
        0
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
    div [ Html.Attributes.class "deck" ]
        [ p [] [ text ("deck: " ++ deck.name) ]
        , p [] [ text ("draw: " ++ String.fromInt (List.length deck.drawPile)) ]
        , p [] [ text ("discard: " ++ String.fromInt (List.length deck.discardPile)) ]
        , div [ Html.Attributes.class "hand" ] (List.map viewCard deck.hand)
        ]


viewPlayer : Player -> Html msg
viewPlayer player =
    div [ Html.Attributes.id "player" ]
        [ h1 [ Html.Attributes.id "player" ] [ text (String.fromFloat player) ]
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
