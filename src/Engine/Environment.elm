module Engine.Environment exposing (Environment, Node(..), PlantTree, fertilize, grow, new, setLight, setTemperature, water)

import Engine.BinaryTree exposing (Tree(..))


type Node
    = Leaf
    | Berry
    | Flower
    | Empty


type alias PlantTree =
    Tree Node


type alias Environment =
    { water : Float
    , fertilizer : Float
    , light : Float
    , temperature : Float
    , growth : Float
    , tree : PlantTree
    }


{-| Create a new plant
-}
new : PlantTree -> Environment
new plantTree =
    Environment 0
        0
        0
        0
        0
        plantTree


{-| Water plant
-}
water : Float -> Environment -> Environment
water amount env =
    { env | water = max 0 (env.water + amount) }


{-| Fertilize plant
-}
fertilize : Float -> Environment -> Environment
fertilize amount env =
    { env | fertilizer = max 0 (env.fertilizer + amount) }


{-| Set light value
-}
setLight : Float -> Environment -> Environment
setLight light env =
    { env | light = max 0 light }


{-| Set temperature value
-}
setTemperature : Float -> Environment -> Environment
setTemperature temp env =
    { env | temperature = temp }


{-| Grow plant. Grow amount is based on supply of water/fertilizer
-}
grow : Float -> Environment -> Environment
grow amount env =
    if env.water >= 1 && env.fertilizer >= 1 then
        { env
            | growth = clamp 0 100 (env.growth + amount)
            , water = env.water - 1
            , fertilizer = env.fertilizer - 1
        }

    else
        env
