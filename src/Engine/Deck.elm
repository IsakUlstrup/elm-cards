module Engine.Deck exposing (Deck, addDiscardToDraw, discardDraw, discardHand, draw, new, randomDeck)

import Random


type alias Deck c =
    { name : String
    , drawPile : List c
    , discardPile : List c
    , hand : List c
    , handSize : Int
    }


{-| Create new deck with given cards in draw pile
-}
new : String -> Int -> List c -> Deck c
new name handSize cards =
    Deck name cards [] [] handSize


{-| Shuffle a list of cards, not yet implemented
-}
shuffle : List c -> List c
shuffle deck =
    deck


{-| Draw cards

If we need more cards than available in draw pile, shuffle discard pile and add to draw pile

-}
draw : Int -> Deck c -> Deck c
draw cards deck =
    if List.length deck.drawPile < cards then
        let
            newDraw =
                deck.drawPile ++ shuffle deck.discardPile
        in
        -- If we do not have enough cards in draw pile, add discard pile to draw pile and draw
        { deck
            | drawPile = newDraw |> List.drop cards
            , discardPile = []
            , hand = List.take cards newDraw ++ deck.hand
        }

    else
        -- Add cards from draw pile to hand
        { deck
            | hand = List.take cards deck.drawPile ++ deck.hand
            , drawPile = List.drop cards deck.drawPile
        }


{-| Add discrd pile to draw pile
-}
addDiscardToDraw : Deck c -> Deck c
addDiscardToDraw deck =
    { deck
        | drawPile = deck.drawPile ++ shuffle deck.discardPile
        , discardPile = []
    }


{-| Move all cards in hand to discard pile
-}
discardHand : Deck c -> Deck c
discardHand deck =
    { deck | discardPile = deck.hand ++ deck.discardPile, hand = [] }


{-| Discard hand and draw cards

This is just the discardHand and draw functions combined

-}
discardDraw : Deck c -> Deck c
discardDraw deck =
    deck |> discardHand |> draw deck.handSize


{-| Given a list of decks, select one at random
-}
randomDeck : List (Deck c) -> Random.Generator (Maybe (Deck c))
randomDeck decks =
    Random.int 0 (List.length decks - 1)
        |> Random.andThen (\index -> List.drop index decks |> List.head |> Random.constant)
