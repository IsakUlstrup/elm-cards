module Plant exposing (Plant, fertilize, grow, new, water)


type alias Plant =
    { water : Float
    , fertilizer : Float
    , growth : Float
    }


{-| Create a new plant
-}
new : Plant
new =
    Plant 0 0 0


{-| Water plant
-}
water : Float -> Plant -> Plant
water amount plant =
    { plant | water = plant.water + amount }


{-| Fertilize plant
-}
fertilize : Float -> Plant -> Plant
fertilize amount plant =
    { plant | fertilizer = plant.fertilizer + amount }


{-| Grow plant. Grow amount is based on supply of water/fertilizer
-}
grow : Plant -> Plant
grow plant =
    if plant.water > 10 && plant.fertilizer > 10 then
        { plant
            | growth = plant.growth + 2
            , water = plant.water - 10
            , fertilizer = plant.fertilizer - 10
        }

    else
        plant
