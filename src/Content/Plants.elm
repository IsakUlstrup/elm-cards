module Content.Plants exposing (..)

import Engine.BinaryTree exposing (Tree(..))
import Engine.Plant exposing (Node(..))


flowerEnd : Tree Node
flowerEnd =
    Node Flower Empty Empty


leafEnd : Tree Node
leafEnd =
    Node Leaf Empty Empty


stem : Tree Node -> Tree Node -> Tree Node
stem =
    Node (Stem { length = 20, thickness = 3 })


empty : Tree a
empty =
    Empty


flower : Tree Node
flower =
    Node (Stem { length = 20, thickness = 3 }) Empty (Node (Stem { length = 20, thickness = 3 }) Empty (Node Leaf (Node Flower Empty Empty) Empty))


simpleFlower : Tree Node
simpleFlower =
    stem leafEnd (stem empty flowerEnd)
