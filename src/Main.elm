module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h3, p, text)
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
    Card "Add" ("+" ++ String.fromFloat n) (Add n) True


subtractCard : Float -> Card
subtractCard n =
    Card "Subtract" ("-" ++ String.fromFloat n) (Subtract n) False


multiplyCard : Bool -> Float -> Card
multiplyCard positive n =
    Card "Multiply" ("*" ++ String.fromFloat n) (Multiply n) positive



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


enemyDeck : List Card
enemyDeck =
    [ subtractCard 1, subtractCard 2, subtractCard 3, multiplyCard False 0.9, multiplyCard False 0.8, multiplyCard False 0.7 ]


playerDeck : List Card
playerDeck =
    [ addCard 1, addCard 2, addCard 3, multiplyCard True 1.1, multiplyCard True 1.2, multiplyCard True 2 ]


randomCard : List a -> Random.Generator (Maybe a)
randomCard xs =
    Random.int 0 (List.length xs - 1)
        |> Random.andThen (\index -> List.drop index xs |> List.head |> Random.constant)


randomCards : Int -> List Card -> List Card -> Random.Generator (List (Maybe Card))
randomCards numCards deck0 deck1 =
    Random.int 0 1
        |> Random.andThen
            (\deckIndex ->
                if deckIndex == 0 then
                    Random.list numCards (randomCard deck0)

                else
                    Random.list numCards (randomCard deck1)
            )


cardSelection : Random.Seed -> List Card -> List Card -> ( List Card, Random.Seed )
cardSelection seed enemyCards playerCards =
    let
        ( maybeCards, s ) =
            Random.step (randomCards 3 enemyCards playerCards) seed
    in
    ( List.filterMap identity maybeCards, s )


type alias Model =
    { enemyCards : List Card
    , playerCards : List Card
    , cardSelection : List Card
    , player : Player
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    let
        ( cardSelect, seed ) =
            cardSelection (Random.initialSeed 42) enemyDeck playerDeck
    in
    ( Model
        enemyDeck
        playerDeck
        cardSelect
        0
        seed
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectCard Card


newCardSelection : Model -> Model
newCardSelection model =
    let
        ( cards, newSeed ) =
            cardSelection model.seed enemyDeck playerDeck
    in
    { model | cardSelection = cards, seed = newSeed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCard card ->
            ( { model | player = applyCard card model.player } |> newCardSelection, Cmd.none )



---- VIEW ----


viewCard : Card -> Html Msg
viewCard card =
    let
        positiveClass =
            if card.positive then
                Html.Attributes.class "positive"

            else
                Html.Attributes.class "negative"
    in
    button [ Html.Events.onClick (SelectCard card), Html.Attributes.class "card", positiveClass ]
        [ h3 [] [ text card.name ]
        , p [] [ text card.description ]
        ]


viewPlayer : Player -> Html msg
viewPlayer player =
    div []
        [ h1 [] [ text (String.fromFloat player) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ Html.Attributes.id "card-selection" ] (List.map viewCard model.cardSelection)
        , viewPlayer model.player
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
