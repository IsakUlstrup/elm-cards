module Engine.BinaryTree exposing (..)


type Tree a b
    = End a
    | Node b (Tree a b) (Tree a b)
