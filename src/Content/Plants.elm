module Content.Plants exposing (..)

import Engine.BinaryTree exposing (Tree(..))
import Engine.Plant exposing (Node(..))


flowerEnd : Tree Node
flowerEnd =
    End Flower


leafEnd : Tree Node
leafEnd =
    End Leaf


empty : Tree Node
empty =
    End Empty


simpleFlower : Tree Node
simpleFlower =
    Node empty (Node leafEnd (Node (Node flowerEnd leafEnd) flowerEnd))
