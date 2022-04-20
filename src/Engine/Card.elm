module Engine.Card exposing (Card, CardColor(..), CardOperation(..), new)


type alias Card =
    { name : String
    , icon : String
    , description : String
    , operations : List CardOperation
    , color : CardColor
    }


type CardColor
    = Cyan
    | Magenta
    | Yellow
    | Black


type CardOperation
    = Water Float
    | Fertilize Float
    | Light Float
    | Temperature Float
    | Grow Float


new : CardColor -> String -> String -> String -> List CardOperation -> Card
new color name icon description operations =
    Card name icon description operations color
