module Deck exposing (Deck, addDiscardToDraw, discardDraw, discardHand, draw, new)


type alias Deck c =
    { name : String
    , drawPile : List c
    , discardPile : List c
    , hand : List c
    }


{-| Create new deck with given cards in draw pile
-}
new : String -> List c -> Deck c
new name cards =
    Deck name cards [] []


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
        -- If we do not have enough cards in draw pile, add discard pile to draw pile and draw
        addDiscardToDraw deck |> draw cards

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
discardDraw : Int -> Deck c -> Deck c
discardDraw cards deck =
    deck |> discardHand |> draw cards
