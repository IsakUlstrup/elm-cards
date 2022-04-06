module Main exposing (..)

import Browser
import Deck exposing (Deck)
import Html exposing (Html, button, div, h3, li, p, section, text, ul)
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


type alias Model =
    { decks : List (Deck Card)
    , hands : List ( Int, List String )
    , index : Int
    , player : Player
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ Deck.new "Player" playerCards, Deck.new "Environment" environmentCards ]
        [ ( 1, [ "hei", "hei", "hei" ] ), ( 0, [] ) ]
        2
        Plant.new
        (Random.initialSeed 42)
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NextHand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextHand ->
            ( { model | hands = List.take 2 (( model.index, [ "Card 1", "Card 2", "Card 3" ] ) :: model.hands), index = model.index + 1 }, Cmd.none )



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
