module Engine.BinaryTree exposing (..)


type Tree a
    = End a
    | Node (Tree a) (Tree a)
