module Main exposing (..)

import Browser
import Content.Cards as Cards
import Engine.Card exposing (Card, CardOperation(..))
import Engine.Deck exposing (Deck)
import Engine.Plant exposing (Plant)
import Html exposing (Html, button, div, h1, h3, li, p, text, ul)
import Html.Attributes exposing (selected)
import Html.Events
import Html.Keyed
import Html.Lazy
import Json.Decode exposing (succeed)
import Random



---- MODEL ----


type alias GameDeck =
    Deck Card


type alias GameHand =
    ( Maybe Int, List Card )


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
        [ ( Nothing, [] )
        ]
        0
        Engine.Plant.new
        (Random.initialSeed 42)
        |> newHand
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NextHand Card
    | SelectCard Int


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
            List.head ds |> Maybe.andThen (\deck -> Just ( Nothing, deck.hand ))

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


selectCard : Int -> Model -> Model
selectCard index model =
    let
        setSelected : Int -> GameHand -> GameHand
        setSelected i (( s, _ ) as h) =
            case s of
                Just sel ->
                    if sel == index then
                        Tuple.mapFirst (always Nothing) h

                    else
                        Tuple.mapFirst (always (Just i)) h

                Nothing ->
                    Tuple.mapFirst (always (Just i)) h
    in
    { model
        | hands =
            List.indexedMap
                (\i h ->
                    if i == 0 then
                        setSelected index h

                    else
                        h
                )
                model.hands
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextHand card ->
            model |> applyCard card |> newHand

        SelectCard ci ->
            model |> selectCard ci



---- VIEW ----


viewPlant : Plant -> Html msg
viewPlant plant =
    div [ Html.Attributes.class "plant" ]
        [ h1 [] [ text "🌱" ]
        , p [] [ text ("water: " ++ String.fromFloat plant.water) ]
        , p [] [ text ("fertilizer: " ++ String.fromFloat plant.fertilizer) ]
        , p [] [ text ("light: " ++ String.fromFloat plant.light) ]
        , p [] [ text ("temp: " ++ String.fromFloat plant.temperature) ]
        , p [] [ text ("growth: " ++ String.fromFloat plant.growth) ]
        ]


viewHands : Int -> List GameHand -> Html Msg
viewHands handCount hands =
    Html.Keyed.node "section" [ Html.Attributes.class "hand-container" ] (List.map (viewKeyedHand handCount) hands)


viewKeyedHand : Int -> GameHand -> ( String, Html Msg )
viewKeyedHand handCount hand =
    ( String.fromInt handCount, Html.Lazy.lazy (viewHand handCount) hand )


viewHand : Int -> GameHand -> Html Msg
viewHand handCount ( s, hand ) =
    ul [ Html.Attributes.class "hand" ] (List.indexedMap (viewCard handCount s) hand)


viewCard : Int -> Maybe Int -> Int -> Card -> Html Msg
viewCard _ selected index card =
    let
        selectedFlag =
            case selected of
                Just si ->
                    if si == index then
                        True

                    else
                        False

                Nothing ->
                    False

        selectedAttr =
            if selectedFlag then
                [ Html.Attributes.class "selected" ]

            else
                []
    in
    li
        ([ Html.Attributes.class "card"
         , Html.Events.onClick (SelectCard index)
         ]
            ++ selectedAttr
        )
        [ h3 [ Html.Attributes.class "title" ] [ text card.name ]
        , h1 [ Html.Attributes.class "icon" ] [ text card.icon ]
        , button [ Html.Events.stopPropagationOn "click" (succeed ( NextHand card, True )), Html.Attributes.disabled (not selectedFlag) ] [ text "Play card" ]
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
