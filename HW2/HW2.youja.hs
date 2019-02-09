-- CS 381 Homework #2
-- Author1 : Jaehyung You
-- OSU ID: 932 - 447 - 918 / ONID ID: youja
-- Author2: ChiaYu Tang
-- OSU ID: 932 - 790 - 081 / ONID ID: tangchia
-- Date : 2018 - 01 - 24

module HW2 where
    -- http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html
    -- Source: http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#Alternative
    -- I've used this module for using <|>, which is an associative binary operation in class Applicative
    import Control.Applicative
    -- | Binary trees with nodes labeled by values of an arbitrary type.
    data Tree a
       = Node a (Tree a) (Tree a)
       | End
      deriving (Eq,Show)
    
    -- | One step in a path, indicating whether to follow the left subtree (L)
    --   or the right subtree (R).
    data Step = L | R
      deriving (Eq,Show)
    
    -- | A path is a sequence of steps. Each node in a binary tree can be
    --   identified by a path, indicating how to move down the tree starting
    --   from the root.
    type Path = [Step]
    
    -- | Create a leaf node.
    leaf :: a -> Tree a
    leaf x = Node x End End
    
    -- | An example tree.
    ex :: Tree Int
    ex = Node 4 (Node 3 (leaf 2) End)
                (Node 7 (Node 5 End (leaf 6))
                        (leaf 8))
    
    
    -- | Map a function over a tree. Applies the given function to every label
    --   in the tree, preserving the tree's structure.
    --   
    --   >>> mapTree odd End
    --   End
    --
    --   >>> mapTree even (Node 5 (leaf 2) End)
    --   Node False (Node True End End) End
    --
    --   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
    --   Node True End (Node False End End)
    --
    --   >>> mapTree (+10) ex
    --   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
    --
    --   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
    --   True
    --
    -- mapTree = undefined

    -- Reference for this entire assignment: 
    -- https://www.schoolofhaskell.com/user/byorgey/introduction-to-haskell/3-recursion-patterns-polymorphism-and-the-prelude

    -- Mapping function to the tree. 
    -- Recursively does left tree and right tree
    mapTree :: (a -> b) -> (Tree a) -> (Tree b)
    mapTree f End = End
    -- Recursively mapping the function over the tree until the leaf.
    mapTree f (Node a b c) = Node (f a) (mapTree f b) (mapTree f c)
    
    -- | Get the value at the node specified by a path. Returns 'Nothing' if
    --   the given path is invalid.
    --
    --   >>> valueAt [] ex
    --   Just 4
    --
    --   >>> valueAt [L,L] ex
    --   Just 2
    --
    --   >>> valueAt [L,R] ex
    --   Nothing
    --
    --   >>> valueAt [R,L,R] ex
    --   Just 6
    --
    --   >>> valueAt [L,L,L] ex
    --   Nothing
    --
    -- valueAt = undefined
    

    valueAt :: Path -> Tree a -> Maybe a
    valueAt f End = Nothing
    valueAt [] (Node a b c) = Just a
    -- if value is at Left, go left
    -- if value is at right, go right
    -- it will return Nothing if the value isn't in the tree or invalid.
    valueAt (x:y) (Node a b c) = 
      if (x == L) then valueAt y b 
      else if (x == R) then valueAt y c
      else Nothing

    
    -- | Find a path to a node that contains the given value.
    --
    --   >>> pathTo 3 (leaf 5)
    --   Nothing
    --
    --   >>> pathTo 5 ex
    --   Just [R,L]
    --
    --   >>> pathTo 6 ex
    --   Just [R,L,R]
    --
    --   >>> pathTo 4 ex
    --   Just []
    --
    --   >>> pathTo 10 ex
    --   Nothing
    --

    -- Reference1: https://stackoverflow.com/questions/21770559/trying-to-implement-path-record-for-haskell-binary-tree-search
    -- Reference2: http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Applicative.html#v%3a-60--124--62-

    -- Maybe value will be a Nothing
    -- If value is same, it will return Just + value
    -- If not, it will find the value recursively byw fmaping a function f until the value is found.
    pathTo :: Eq a => a -> Tree a -> Maybe Path
    pathTo f End = Nothing
    -- if value is there, finish it.
    -- other then, it will recursively try to find the value and add the current node.
    pathTo f (Node a b c) =
      if (f == a)
        then Just []
        else (<|>) (fmap (L:) (pathTo f b)) (fmap (R:) (pathTo f c))
    
    -- (<|>) is an associative binary operation in class Applicative
    -- type : f a -> f a -> f a