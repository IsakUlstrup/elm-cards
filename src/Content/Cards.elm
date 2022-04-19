module Content.Cards exposing (..)

import Engine.Card exposing (Card, CardOperation(..))
import Engine.Deck exposing (Deck)



---- CARDS ----


rainCard : Card
rainCard =
    Engine.Card.new "Rain" "🌧️" "It's raining, water plant" [ Water 1, Light 40, Temperature 20 ]


droughtCard : Card
droughtCard =
    Engine.Card.new "Drought" "☀️" "It's super hot, water evaporates" [ Water -1, Light 80, Temperature 60 ]


monsoonCard : Card
monsoonCard =
    Engine.Card.new "Monsoon" "⛈️" "A monsoon!, lots of water" [ Water 5, Light 40, Temperature 30 ]


cowCard : Card
cowCard =
    Engine.Card.new "Cow" "🐄" "A cow passes by and poops everywhere" [ Fertilize 2 ]


waterBucket : Card
waterBucket =
    Engine.Card.new "Bucket" "\u{1FAA3}" "water bucket" [ Water 2 ]



---- DECKS ----


environmentDeck : Deck Card
environmentDeck =
    Engine.Deck.new "Environment" 1 [ droughtCard, monsoonCard, droughtCard, droughtCard, monsoonCard, droughtCard, droughtCard, monsoonCard ]


playerDeck : Deck Card
playerDeck =
    Engine.Deck.new "Player" 3 [ waterBucket, cowCard, waterBucket, waterBucket, cowCard, cowCard ]
