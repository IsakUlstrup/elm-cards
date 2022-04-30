module Engine.BinaryTree exposing (..)


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)
