module Main exposing (..)

import Browser
import Content.Cards as Cards
import Engine.Card exposing (Card, CardOperation(..))
import Engine.Deck exposing (Deck)
import Engine.Plant exposing (Plant)
import Html exposing (Html, button, div, h1, h3, li, p, section, text, ul)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Random



---- MODEL ----


type alias GameDeck =
    Deck Card


type alias GameHand =
    List Card


type alias Model =
    { decks : List GameDeck
    , hands : List ( Int, GameHand )
    , handCount : Int
    , plant : Plant
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ Cards.playerDeck
        , Cards.environmentDeck
        ]
        [ ( 0, [] )
        ]
        1
        Engine.Plant.new
        (Random.initialSeed 42)
        |> newHand
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NextHand Card


{-| draw cards from first deck, and put it at the back of deck list
-}
newHand : Model -> Model
newHand model =
    let
        -- Draw a new hand on first deck
        firstDeckDraw : List GameDeck -> List GameDeck
        firstDeckDraw ds =
            List.indexedMap
                (\i d ->
                    if i == 0 then
                        Engine.Deck.discardDraw 3 d

                    else
                        d
                )
                ds

        -- get hand from first deck
        firstDeckHand : List GameDeck -> Maybe GameHand
        firstDeckHand ds =
            List.head ds |> Maybe.andThen (\deck -> Just deck.hand)

        -- move first deck to the bottom of deck list
        moveDeckBack : List GameDeck -> List GameDeck
        moveDeckBack ds =
            case ds of
                [] ->
                    []

                d :: dss ->
                    dss ++ [ d ]

        newHands : Int -> List GameDeck -> List ( Int, GameHand ) -> List ( Int, GameHand )
        newHands index decks hands =
            case firstDeckHand decks of
                Just hand ->
                    List.take 2 (( index, hand ) :: model.hands)

                Nothing ->
                    hands

        newDecks =
            firstDeckDraw model.decks
    in
    { model
        | decks = newDecks |> moveDeckBack
        , hands = newHands model.handCount newDecks model.hands
        , handCount = model.handCount + 1
    }


applyCard : Card -> Model -> Model
applyCard card model =
    let
        applyOperation : CardOperation -> Plant -> Plant
        applyOperation op plr =
            case op of
                Water n ->
                    plr |> Engine.Plant.water n

                Fertilize n ->
                    plr |> Engine.Plant.fertilize n

                Light n ->
                    plr |> Engine.Plant.setLight n

                Temperature n ->
                    plr |> Engine.Plant.setTemperature n

                Grow n ->
                    plr |> Engine.Plant.grow n
    in
    { model | plant = List.foldr applyOperation model.plant card.operations }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextHand card ->
            model |> applyCard card |> newHand



---- VIEW ----


viewHands : List ( Int, GameHand ) -> Html Msg
viewHands hands =
    Html.Keyed.node "section" [ Html.Attributes.class "hand-container" ] (List.map viewKeyedHand hands)


viewKeyedHand : ( Int, GameHand ) -> ( String, Html Msg )
viewKeyedHand hand =
    ( String.fromInt (Tuple.first hand), Html.Lazy.lazy viewHand hand )


viewHand : ( Int, GameHand ) -> Html Msg
viewHand ( i, hand ) =
    ul [ Html.Attributes.class "hand" ] (List.map (viewCard i) hand)


viewCard : Int -> Card -> Html Msg
viewCard handIndex card =
    li [ Html.Attributes.class "card" ]
        [ h3 [ Html.Attributes.class "title" ] [ text card.name ]
        , h1 [ Html.Attributes.class "icon" ] [ text card.icon ]
        , button [ Html.Events.onClick (NextHand card) ] [ text "Play card" ]
        , div [ Html.Attributes.class "body" ]
            [ p [] [ text ("Hand #" ++ String.fromInt handIndex) ]
            , p [] [ text card.description ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "app" ]
        [ section [ Html.Attributes.class "hud" ]
            []
        , viewHands model.hands
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always Sub.none
        }
