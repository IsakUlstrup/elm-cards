module Engine.Plant exposing (Plant, fertilize, grow, new, setLight, setTemperature, water)

import Engine.BinaryTree exposing (Tree(..))


type alias Stem =
    { length : Float, thickness : Float }


type Terminus
    = Flower
    | Leaf
    | Berry


type alias Plant =
    { water : Float
    , fertilizer : Float
    , light : Float
    , temperature : Float
    , growth : Float
    , tree : Tree Terminus Stem
    }


{-| Create a new plant
-}
new : Plant
new =
    Plant 0 0 0 0 0 (Node { length = 1, thickness = 1 } (End Leaf) (End Berry))


{-| Water plant
-}
water : Float -> Plant -> Plant
water amount plant =
    { plant | water = max 0 (plant.water + amount) }


{-| Fertilize plant
-}
fertilize : Float -> Plant -> Plant
fertilize amount plant =
    { plant | fertilizer = max 0 (plant.fertilizer + amount) }


{-| Set light value
-}
setLight : Float -> Plant -> Plant
setLight light plant =
    { plant | light = max 0 light }


{-| Set temperature value
-}
setTemperature : Float -> Plant -> Plant
setTemperature temp plant =
    { plant | temperature = temp }


{-| Grow plant. Grow amount is based on supply of water/fertilizer
-}
grow : Float -> Plant -> Plant
grow amount plant =
    if plant.water >= 1 && plant.fertilizer >= 1 then
        { plant
            | growth = clamp 0 100 (plant.growth + amount)
            , water = plant.water - 1
            , fertilizer = plant.fertilizer - 1
        }

    else
        plant
