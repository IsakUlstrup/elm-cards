module Content.Cards exposing (..)

import Engine.Card exposing (Card, CardColor(..), CardOperation(..))
import Engine.Deck exposing (Deck)



---- CARDS ----


rainCard : Card
rainCard =
    Engine.Card.new Cyan "Rain" "🌧️" "It's raining, water plant" [ Water 1, Light 40 ]


sunny : Card
sunny =
    Engine.Card.new Cyan "Sunny" "☀️" "Sunny weather" [ Light 80, Temperature 60 ]


monsoonCard : Card
monsoonCard =
    Engine.Card.new Cyan "Monsoon" "⛈️" "A monsoon!, lots of water" [ Water 5, Light 40 ]


cowCard : Card
cowCard =
    Engine.Card.new Magenta "Cow" "🐄" "A cow passes by and poops everywhere" [ Fertilize 2 ]


waterBucket : Card
waterBucket =
    Engine.Card.new Magenta "Bucket" "\u{1FAA3}" "water bucket" [ Water 2 ]



---- DECKS ----


environmentDeck : Deck Card
environmentDeck =
    Engine.Deck.new "Environment" 1 [ sunny, monsoonCard, sunny, sunny, monsoonCard, sunny, sunny, monsoonCard ]


playerDeck : Deck Card
playerDeck =
    Engine.Deck.new "Player" 3 [ waterBucket, cowCard, waterBucket, waterBucket, cowCard, cowCard ]
