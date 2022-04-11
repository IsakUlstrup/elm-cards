module Main exposing (..)

import Browser
import Content.Cards as Cards
import Engine.Card exposing (Card, CardOperation(..))
import Engine.Deck exposing (Deck)
import Engine.Plant exposing (Plant)
import Html exposing (Html, div, h1, h2, li, p, span, sup, text, ul)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Random


{-| Compare a maybe value and a value of the same type, return true is equal
-}
maybeEq : Maybe a -> a -> Bool
maybeEq ma a =
    case Maybe.map ((==) a) ma of
        Just j ->
            j

        Nothing ->
            False



---- MODEL ----


type alias GameDeck =
    Deck Card


{-| Selected card index, list of cards
-}
type alias GameHand =
    { played : Maybe Int
    , cards : List Card
    }


type alias Model =
    { decks : List GameDeck
    , hands : List GameHand
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
        [ GameHand Nothing []
        ]
        0
        Engine.Plant.new
        (Random.initialSeed 42)
        |> newHand
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NextHand Int Card


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
            List.head ds |> Maybe.andThen (\deck -> Just (GameHand Nothing deck.hand))

        -- move first deck to the bottom of deck list
        moveDeckBack : List GameDeck -> List GameDeck
        moveDeckBack ds =
            case ds of
                [] ->
                    []

                d :: dss ->
                    dss ++ [ d ]

        newHands : Int -> List GameDeck -> List GameHand -> List GameHand
        newHands _ decks hands =
            case firstDeckHand decks of
                Just hand ->
                    List.take 2 (hand :: model.hands)

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


{-| Set played card index
-}
setPlayed : Int -> GameHand -> GameHand
setPlayed i hand =
    { hand | played = Just i }


{-| Run a function on the first hand in model.hands
-}
updateCurrentHand : (GameHand -> GameHand) -> Model -> Model
updateCurrentHand f model =
    { model
        | hands =
            List.indexedMap
                (\i h ->
                    if i == 0 then
                        f h

                    else
                        h
                )
                model.hands
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextHand ci card ->
            model |> updateCurrentHand (setPlayed ci) |> applyCard card |> newHand



---- VIEW ----


viewPlant : Plant -> Html msg
viewPlant plant =
    div [ Html.Attributes.class "plant" ]
        [ h1 [] [ text "🌱" ]
        , ul []
            [ li [] [ text ("water: " ++ String.fromFloat plant.water) ]
            , li [] [ text ("fertilizer: " ++ String.fromFloat plant.fertilizer) ]
            , li [] [ text ("light: " ++ String.fromFloat plant.light) ]
            , li [] [ text ("temp: " ++ String.fromFloat plant.temperature) ]
            , li [] [ text ("growth: " ++ String.fromFloat plant.growth) ]
            ]
        ]


viewHands : Int -> List GameHand -> Html Msg
viewHands handCount hands =
    Html.Keyed.node "section" [ Html.Attributes.class "hand-container" ] (List.map (viewKeyedHand handCount) hands)


viewKeyedHand : Int -> GameHand -> ( String, Html Msg )
viewKeyedHand handCount hand =
    ( String.fromInt handCount, Html.Lazy.lazy viewHand hand )


viewHand : GameHand -> Html Msg
viewHand hand =
    ul [ Html.Attributes.class "hand" ] (List.indexedMap (viewCard hand) hand.cards)


viewCard : GameHand -> Int -> Card -> Html Msg
viewCard hand index card =
    let
        selectedAttr =
            if maybeEq hand.played index then
                [ Html.Attributes.class "played" ]

            else
                []
    in
    li
        ([ Html.Attributes.class "card"
         , Html.Events.onClick (NextHand index card)
         ]
            ++ selectedAttr
        )
        [ h1 [ Html.Attributes.class "title" ] [ text card.name ]
        , h2 [ Html.Attributes.class "icon" ] [ text card.icon ]
        , div [ Html.Attributes.class "cost" ]
            [ span []
                [ text "💩"
                , sup [] [ text "2" ]
                ]
            , span []
                [ text "💧"
                , sup [] [ text "4" ]
                ]
            ]
        , div [ Html.Attributes.class "body" ]
            [ p [] [ text card.description ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "app" ]
        [ viewPlant model.plant
        , viewHands model.handCount model.hands
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
