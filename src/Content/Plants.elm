module Content.Plants exposing (..)

import Engine.BinaryTree exposing (Tree(..))
import Engine.Plant exposing (Node(..))


flower : Tree Node
flower =
    Node (Stem { length = 20, thickness = 3 }) Empty (Node (Stem { length = 20, thickness = 3 }) Empty (Node Leaf (Node Flower Empty Empty) Empty))


simpleFlower : Tree Node
simpleFlower =
    Node (Stem { length = 20, thickness = 3 }) Empty (Node Flower Empty Empty)
