module Engine.Card exposing (Card, CardOperation(..), new)


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


new : String -> String -> List CardOperation -> Card
new name description operations =
    Card name description operations
