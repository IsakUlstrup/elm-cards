module Main exposing (..)

import Browser
import Content.Cards as Cards
import Content.Plants as Plants
import Engine.BinaryTree exposing (Tree(..))
import Engine.Card exposing (Card, CardColor(..), CardOperation(..))
import Engine.Deck exposing (Deck)
import Engine.Plant exposing (Node(..), Plant, PlantTree)
import Html exposing (Html, div, h1, li, sup, text, ul)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Random
import Svg exposing (svg)
import Svg.Attributes


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
        (Engine.Plant.new Plants.simpleFlower)
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
                        Engine.Deck.discardDraw d

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


passTime : Model -> Model
passTime model =
    { model | plant = Engine.Plant.grow 10 model.plant }


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
            model |> updateCurrentHand (setPlayed ci) |> applyCard card |> passTime |> newHand



---- VIEW ----


viewPlantTree : PlantTree -> Html msg
viewPlantTree tree =
    let
        nodeString e =
            case e of
                Leaf ->
                    "🌿"

                Berry ->
                    "🥭"

                Flower ->
                    "🌺"

                Empty ->
                    ""

        renderNode : PlantTree -> Html msg
        renderNode n =
            let
                line x =
                    Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 (String.fromFloat x)
                        , Svg.Attributes.y2 "-14"
                        , Svg.Attributes.stroke "green"
                        , Svg.Attributes.strokeWidth "2"
                        ]
                        []
            in
            case n of
                End a ->
                    Svg.text_ [ Svg.Attributes.textAnchor "middle", Svg.Attributes.y "5" ] [ Svg.text (nodeString a) ]

                Node t1 t2 ->
                    Svg.g []
                        (case ( t1, t2 ) of
                            -- No chidren
                            ( End Empty, End Empty ) ->
                                []

                            -- Left child
                            ( l, End Empty ) ->
                                [ line 0
                                , Svg.g [ Svg.Attributes.transform "translate(0, -14)" ] [ renderNode l ]
                                ]

                            -- Right child
                            ( End Empty, r ) ->
                                [ line 0
                                , Svg.g [ Svg.Attributes.transform "translate(0, -14)" ] [ renderNode r ]
                                ]

                            -- Both children
                            ( l, r ) ->
                                [ line -10
                                , line 10
                                , Svg.g [ Svg.Attributes.transform "translate(-10, -14)" ] [ renderNode l ]
                                , Svg.g [ Svg.Attributes.transform "translate(10, -14)" ] [ renderNode r ]
                                ]
                        )
    in
    svg [ Svg.Attributes.viewBox "-50 -100 100 100" ] [ renderNode tree ]


viewPlant : Plant -> Html msg
viewPlant plant =
    div [ Html.Attributes.class "plant" ]
        [ viewPlantTree plant.tree
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


viewCardOperations : List CardOperation -> Html msg
viewCardOperations operations =
    let
        operationText op =
            case op of
                Water n ->
                    li [] [ text "💧", sup [] [ text (String.fromFloat n) ] ]

                Fertilize n ->
                    li [] [ text "💩", sup [] [ text (String.fromFloat n) ] ]

                Light n ->
                    li [] [ text "☀️", sup [] [ text (String.fromFloat n) ] ]

                Temperature n ->
                    li [] [ text "🌡️", sup [] [ text (String.fromFloat n) ] ]

                Grow n ->
                    li [] [ text "grw", sup [] [ text (String.fromFloat n) ] ]
    in
    ul [ Html.Attributes.class "operations" ] (List.map operationText operations)


viewCard : GameHand -> Int -> Card -> Html Msg
viewCard hand index card =
    let
        selectedAttr =
            if maybeEq hand.played index then
                [ Html.Attributes.class "played" ]

            else
                []

        backgroundColorClass =
            case card.color of
                Cyan ->
                    Html.Attributes.class "cbg-cyan"

                Magenta ->
                    Html.Attributes.class "cbg-magenta"

                Yellow ->
                    Html.Attributes.class "cbg-yellow"

                Black ->
                    Html.Attributes.class "cbg-black"
    in
    li
        ([ Html.Attributes.class "card"
         , Html.Attributes.class "cbg-red"
         , Html.Events.onClick (NextHand index card)
         ]
            ++ selectedAttr
            ++ [ backgroundColorClass ]
        )
        [ h1 [ Html.Attributes.class "icon" ] [ text card.icon ]
        , viewCardOperations card.operations
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
