module Basics where

    -- Don't worry about this line. It's just hiding some functions that are
    -- usually imported by default, but which I'm defining my own versions of
    -- in this intro file.
    import Prelude hiding (length,sum,product,map,foldr)
    
    
    ---------------------
    -- Introduce Tools --
    ---------------------
    
    -- * GHCi commands
    --     :help, :load, :reload, :quit, :type, :info
    -- * Hoogle
    -- * doctest
    
    
    ---------------------
    -- Getting Started --
    ---------------------
    
    -- In GHCi:
    --  * basic data types (Bool, Int, Float)
    --  * numeric and boolean operators
    --  * if-then-else expressions
    --  * let-expressions
    
    
    ---------------------
    -- Basic Functions --
    ---------------------
    
    -- * defining and applying functions
    -- * pattern matching
    -- * partial application
    
    
    -- | Add an integer to itself.
    double :: Int -> Int
    double x = x + x
    
    -- | Is this integer zero?
    isZero :: Int -> Bool
    isZero 0 = True -- Order matters
    isZero _ = False
    -- isZero x = x == 0
    
    -- | Is this integer non-zero?
    isNonZero :: Int -> Bool
    
    --Option 4
    isNonZero = not . isZero
    
    --Option 3
    -- isNonZero x = not (isZero x)
    
    --Option 2
    --isNonZero 0 = False
    --isNonZero _ = True
    
    -- Option 1
    -- isNonZero x = x /= 0
    
    
    -- | Computes the average of two floating point numbers.
    avg :: Float -> Float -> Float
    avg x y = ( x + y) / 2.0
    
    -- | Uses avg to compute half of a floating point number.
    half :: Float -> Float
    half = \x -> avg 0 x
    --half = avg 0
    --half x = avg 0 x 
    
    
    -- In GHCi:
    --  * infix vs. prefix application: operators are just functions!
    --    * (+) x y = x + y
    --    * avg x y = x `avg` y
    -- * anonymous functions
    
    
    ----------------------
    -- Basic Data Types --
    ----------------------
    
    -- * a data type definition consists of:
    --   * a new type name
    --   * a set of cases, each with:
    --     * a data constructor
    --     * zero or more arguments
    -- * more pattern matching
    --   * top-level and case-expressions
    
    -- | An example data type with two cases.
    data Result = OK Int | Error
      deriving (Eq,Show)
    
    -- | Safely divide two integers.
    safeDiv :: Int -> Int -> Result
    safeDiv _ 0 = Error
    safeDiv x y = OK (x `div` y)
    
    -- | Get the integer from an OK result, or return 0 on an Error.
    fromResult :: Result -> Int
    fromResult (OK i) = i
    fromResult Error  = 0
    
    -- | Add two results.
    addResults :: Result -> Result -> Result
    addResults (OK i) (OK j) = OK (i+j)
    addResults _      _      = Error
    
    
    -- The definition of Bool in the Haskell Prelude looks like this:
    --   
    --   data Bool = False | True
    
    
    -- The type Result is similar to the Maybe type in the Prelude:
    --
    --   data Maybe a = Just a | Nothing
    
    
    
    ---------------
    -- Recursion --
    ---------------
    
    -- * recursive data type definitions
    -- * recursive functions
    
    -- | An example of a recursive data type.
    data List
       = Nil
       | Cons Int List
      deriving (Eq,Show)
    
    -- | The empty list.
    empty :: List
    empty = undefined
    
    -- | The list: [2,3,4]
    exList :: List
    exList = undefined
    
    -- | Compute the length of a list.
    listLength :: List -> Int
    listLength = undefined
    
    -- | Compute the sum of the integers in a list.
    listSum :: List -> Int
    listSum = undefined
    
    
    -- Example evaluation:
    --
    -- listSum (Cons 3 (Cons 4 Nil))
    -- => ...
    
    
    -- Use a *type parameter* to define lists that contain elements of any type:
    --
    --    data List a
    --       = Nil
    --       | Cons a (List a)   -- all elements must be of the *same* type
    --
    --    listLength :: List a -> Int
    --    listSum    :: List Int -> Int
    --    listSum'   :: Num a => List a -> a
    
    
    
    -------------------
    -- Haskell Lists --
    -------------------
    
    -- * Haskell's built-in list and string types
    --   * cons, nil, and syntactic sugar
    --   * more recursive functions
    
    -- data [a]
    --    = []         -- Nil
    --    | a : [a]    -- Cons
    
    -- The definition of String in the Haskell Prelude looks like this:
    --
    --   type String = [Char]
    
    
    -- | Compute the length of a list.
    length :: [a] -> Int
    length = undefined
    
    -- | Compute the sum of an integer list.
    sum :: [Int] -> Int
    sum = undefined
    
    -- | Compute the product of the elements in a list.
    product :: [Int] -> Int
    product [] = 1
    product (h:t) = h * product t 
    
    -- | Double all the elements in an integer list.
    doubleAll :: [Int] -> [Int]
    doubleAll [] = []
    doubleAll (h:t) = (2*h) : doubleAll t
    
    -- | Flip all of the boolean values in a boolean list.
    notAll :: [Bool] -> [Bool]
    notAll [] = []
    notAll (h:t) = not h : notAll t
    
    -- | Apply the even function to all elements in the list.
    evenAll :: [Int] -> [Bool]
    evenAll [] = []
    evenAll (h:t) = even h : evenAll t
    
    ----------------------------
    -- Higher-Order Functions --
    ----------------------------
    
    -- * map and foldr
    
    
    -- | Map a function over the elements in a list.
    map :: (a -> b) -> [a] -> [b]
    map f [] = []
    map f (h:t) = f h : map f t
    
    -- | Reimplement doubleAll using map.
    doubleAll' :: [Int] -> [Int]
    doubleAll' = map (2*)
    
    -- | Reimplement notAll using map.
    notAll' :: [Bool] -> [Bool]
    notAll' = map not
    
    -- | Reimplement evenAll using map
    evenAll' :: [Int] -> [Bool]
    evenAll' = map even

    -- | Fold an accumulator function over the elements in a list.
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr = undefined
    
    -- | Reimplement sum using foldr.
    sum' :: [Int] -> Int
    sum' = undefined
    
    -- | Reimplement product using foldr.
    product' :: [Int] -> Int
    product' = undefined
    
    -- | Reimplement length using foldr.
    length' :: [a] -> Int
    length' = undefined
    
    -- | Use foldr to count the True values in a list of Bools.
    countTrues :: [Bool] -> Int
    countTrues = undefined

    -- Live Coding Part 2: