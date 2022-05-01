module Content.Plants exposing (..)

import Engine.BinaryTree exposing (Tree(..))
import Engine.Plant exposing (Node(..))


flowerEnd : Tree Node
flowerEnd =
    End Flower


leafEnd : Tree Node
leafEnd =
    End Leaf


berryEnd : Tree Node
berryEnd =
    End Berry


empty : Tree Node
empty =
    End Empty


simpleFlower : Tree Node
simpleFlower =
    Node empty (Node leafEnd (Node (Node leafEnd berryEnd) flowerEnd))
