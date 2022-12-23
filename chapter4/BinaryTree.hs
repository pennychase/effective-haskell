module BinaryTree where

-- BinaryTree exercises from Chapter
-- Generalized the exercises to use TypeClass constraints, so they all work for (Eq a, Ord a)

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
            deriving Show
    

addElement :: (Eq a, Ord a) => BinaryTree a -> a -> BinaryTree a
addElement btree e =
    case btree of
        Leaf -> Branch Leaf e Leaf
        Branch left v right -> 
            case compare e v of
                LT -> Branch (addElement left e) v right
                GT -> Branch left v (addElement right e)
                EQ -> btree

fromList :: (Eq a, Ord a) => [a] -> BinaryTree a
fromList = foldr (flip addElement) Leaf

doesValueExist :: (Eq a, Ord a) => BinaryTree a -> a -> Bool
doesValueExist btree e =
    case btree of
        Leaf -> False
        Branch left v right ->
            case compare e v of
                EQ -> True
                LT -> doesValueExist left e
                GT -> doesValueExist right e

showTree :: (Show a) => BinaryTree a -> String
showTree btree =
    case btree of
        Leaf -> "Leaf"
        Branch left v right -> "(Branch " <> showTree left <> " " <> show v <> " " <> showTree right <> ")"
