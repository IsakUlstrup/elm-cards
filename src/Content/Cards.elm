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


pandaCard : Card
pandaCard =
    Engine.Card.new "Panda" "🐼" "A panda nibbles on your plant before falling asleep" [ Grow -1 ]


passTimeCard : Card
passTimeCard =
    Engine.Card.new "Time" "⏳" "Time passes" [ Grow 1 ]



---- DECKS ----


environmentDeck : Deck Card
environmentDeck =
    Engine.Deck.new "Environment" [ droughtCard, droughtCard, passTimeCard, droughtCard, monsoonCard, pandaCard, droughtCard, droughtCard, monsoonCard ]


playerDeck : Deck Card
playerDeck =
    Engine.Deck.new "Player" [ rainCard, passTimeCard, cowCard, passTimeCard, rainCard, rainCard, cowCard, cowCard ]
