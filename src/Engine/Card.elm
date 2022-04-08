module Engine.Card exposing (Card, CardOperation(..), new)


type alias Card =
    { name : String
    , icon : String
    , description : String
    , operations : List CardOperation
    }


type CardOperation
    = Water Float
    | Fertilize Float
    | Light Float
    | Temperature Float
    | Grow Float


new : String -> String -> String -> List CardOperation -> Card
new name icon description operations =
    Card name icon description operations
