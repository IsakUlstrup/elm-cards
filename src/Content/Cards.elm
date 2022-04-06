module Content.Cards exposing (..)

import Engine.Card exposing (Card, CardOperation(..))



---- CARDS ----


rainCard : Card
rainCard =
    Engine.Card.new "🌧️" "It's raining, water plant" [ Water 1, Light 40, Temperature 20 ]


droughtCard : Card
droughtCard =
    Engine.Card.new "☀️" "It's super hot, water evaporates" [ Water -1, Light 80, Temperature 60 ]


monsoonCard : Card
monsoonCard =
    Engine.Card.new "⛈️" "A monsoon!, lots of water" [ Water 5, Light 40, Temperature 30 ]


cowCard : Card
cowCard =
    Engine.Card.new "🐄" "A cow passes by and poops everywhere" [ Fertilize 2 ]


pandaCard : Card
pandaCard =
    Engine.Card.new "🐼" "A panda nibbles on your plant before falling asleep" [ Grow -1 ]


passTimeCard : Card
passTimeCard =
    Engine.Card.new "⏳" "Time passes" [ Grow 1 ]



---- DECKS ----


environmentCards : List Card
environmentCards =
    [ droughtCard, droughtCard, passTimeCard, droughtCard, monsoonCard, pandaCard, droughtCard, droughtCard, monsoonCard ]


playerCards : List Card
playerCards =
    [ rainCard, passTimeCard, cowCard, passTimeCard, rainCard, rainCard, cowCard, cowCard ]
