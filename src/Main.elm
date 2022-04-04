module Main exposing (..)

import Browser
import Deck exposing (Deck)
import Html exposing (Attribute, Html, button, div, h1, li, p, text, ul)
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


type HandState
    = Init
    | Draw (Deck Card)
    | Select Card (Deck Card)
    | Play Card (Deck Card)


type alias Model =
    { decks : List (Deck Card)
    , handState : HandState
    , player : Player
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ Deck.new "Player" playerCards
        , Deck.new "Environment" environmentCards
        ]
        Init
        Plant.new
        (Random.initialSeed 42)
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = DrawCards
    | SelectCard Card (Deck Card)
    | PlayCard Card (Deck Card)


maybeDraw : Deck Card -> Maybe (Deck Card)
maybeDraw deck =
    Just (Deck.discardDraw 3 deck)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DrawCards ->
            let
                deck =
                    List.head model.decks |> Maybe.andThen maybeDraw
            in
            case deck of
                Just d ->
                    ( { model | handState = Draw d }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SelectCard card deck ->
            ( { model | handState = Select card deck }, Cmd.none )

        PlayCard card deck ->
            ( { model | handState = Play card deck }, Cmd.none )



---- VIEW ----


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


viewCard : List (Attribute Msg) -> Deck Card -> Card -> Html Msg
viewCard attrs deck card =
    div attrs
        [ button [ Html.Events.onClick (SelectCard card deck) ]
            [ h1 [] [ text card.name ]
            , p [] [ text card.description ]
            ]
        , button [ Html.Events.onClick (PlayCard card deck), Html.Attributes.class "play-button" ] [ text "play" ]
        ]


viewCardWithState : HandState -> Card -> Html Msg
viewCardWithState state card =
    case state of
        Init ->
            div [] [ button [ Html.Events.onClick DrawCards ] [ text "Draw cards" ] ]

        Draw deck ->
            viewCard [ Html.Attributes.class "card", Html.Attributes.class "enter" ] deck card

        Select c deck ->
            if c == card then
                viewCard [ Html.Attributes.class "card", Html.Attributes.class "selected" ] deck card

            else
                viewCard [ Html.Attributes.class "card" ] deck card

        Play c deck ->
            if c == card then
                viewCard [ Html.Attributes.class "card", Html.Attributes.class "played", Html.Attributes.class "selected" ] deck card

            else
                viewCard [ Html.Attributes.class "card", Html.Attributes.class "leave" ] deck card


viewHandState : Model -> Html Msg
viewHandState model =
    case model.handState of
        Init ->
            div []
                [ div [ Html.Attributes.class "hand-actions" ]
                    [ button [ Html.Events.onClick DrawCards ] [ text "Draw cards" ]
                    ]
                , div
                    [ Html.Attributes.class "hand" ]
                    []
                ]

        Draw deck ->
            div []
                [ div [ Html.Attributes.class "hand-actions" ] []
                , div [ Html.Attributes.class "hand" ] (List.map (viewCardWithState model.handState) deck.hand)
                , p [ Html.Attributes.class "deck-meta" ] [ text ("draw: " ++ String.fromInt (List.length deck.drawPile) ++ ", discard: " ++ String.fromInt (List.length deck.discardPile)) ]
                ]

        Select _ deck ->
            div []
                [ div [ Html.Attributes.class "hand-actions" ] []
                , div [ Html.Attributes.class "hand" ] (List.map (viewCardWithState model.handState) deck.hand)
                , p [ Html.Attributes.class "deck-meta" ] [ text ("draw: " ++ String.fromInt (List.length deck.drawPile) ++ ", discard: " ++ String.fromInt (List.length deck.discardPile)) ]
                ]

        Play _ deck ->
            div []
                [ div [ Html.Attributes.class "hand-actions" ]
                    [ button [ Html.Events.onClick DrawCards ] [ text "Draw cards" ]
                    ]
                , div [ Html.Attributes.class "hand" ] (List.map (viewCardWithState model.handState) deck.hand)
                , p [ Html.Attributes.class "deck-meta" ] [ text ("draw: " ++ String.fromInt (List.length deck.drawPile) ++ ", discard: " ++ String.fromInt (List.length deck.discardPile)) ]
                ]


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "app" ]
        [ viewPlayer model.player
        , viewHandState model
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
