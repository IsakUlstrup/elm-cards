module Main exposing (..)

import Browser
import Content.Cards as Cards
import Engine.Card exposing (Card, CardOperation(..))
import Engine.Deck exposing (Deck)
import Engine.Plant exposing (Plant)
import Html exposing (Html, button, div, h3, li, p, section, text, ul)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Random



---- PLAYER ----


applyCard : Card -> Plant -> Plant
applyCard card plant =
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
    List.foldr applyOperation plant card.operations



---- MODEL ----


type alias GameDeck =
    Deck Card


type alias Model =
    { decks : List GameDeck
    , hands : List ( Int, List String )
    , index : Int
    , player : Plant
    , seed : Random.Seed
    }


dummyCards : Int -> ( Int, List String )
dummyCards index =
    ( index, [ "Card 1", "Card 2", "Card 3" ] )


init : ( Model, Cmd Msg )
init =
    ( Model
        [ Cards.playerDeck
        , Cards.environmentDeck
        ]
        [ dummyCards 1
        , ( 0, [] )
        ]
        2
        Engine.Plant.new
        (Random.initialSeed 42)
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NextHand


newHand : Model -> Model
newHand model =
    { model
        | hands = List.take 2 (dummyCards model.index :: model.hands)
        , index = model.index + 1
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextHand ->
            ( newHand model
            , Cmd.none
            )



---- VIEW ----


viewHands : List ( Int, List String ) -> Html Msg
viewHands hands =
    Html.Keyed.node "section" [ Html.Attributes.class "hand-container" ] (List.map viewKeyedHand hands)


viewKeyedHand : ( Int, List String ) -> ( String, Html Msg )
viewKeyedHand ( i, hand ) =
    ( String.fromInt i, Html.Lazy.lazy viewHand ( i, hand ) )


viewHand : ( Int, List String ) -> Html Msg
viewHand ( i, strings ) =
    let
        card : String -> Html Msg
        card c =
            li [ Html.Attributes.class "card" ]
                [ h3 [] [ text c ]
                , p [] [ text ("Hand #" ++ String.fromInt i) ]
                , button [ Html.Events.onClick NextHand ] [ text "Play card" ]
                , p [] [ text "Card description" ]
                ]
    in
    ul [ Html.Attributes.class "hand" ] (List.map card strings)


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "app" ]
        [ section [ Html.Attributes.class "hud" ]
            [ p [] [ text "WIP" ]
            ]
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
