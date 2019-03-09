-- CS 381 Homework #4
-- Author1 : Jaehyung You
-- ONID ID: youja
-- Date : 2019 - 02 - 26

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

-- | Valuation function for Test.
-- The Syntax is from karelSyntax.hs
-- data Test = Not    Test   -- boolean negation
--           | Facing Card   -- am I facing the given cardinal direction?
--           | Clear  Dir    -- can I move in the given relative direction?
--           | Beeper        -- is there a beeper here?
--           | Empty         -- is my beeper bag empty?
--   deriving (Eq,Show)

-- Functions in KarelState.hs in KarelState.hs
-- isClear
-- hasBeeper
-- incBeeper
-- decBeeper
-- ...
-- getPos
-- relativePos
-- setPos
-- updatePos
-- getFacing
-- setFacing
-- updateFacing
-- ...
-- getBag
-- isEmpty
-- incBag
-- decBag
-- ...
-- onOK

test :: Test -> World -> Robot -> Bool
-- a: Test, b: World, c: Robot
test a b c = case a of 
            Not a    -> not (test a b c)
            Facing f -> f == (getFacing c)
            Clear cl -> isClear (relativePos cl c) b
            Beeper   -> hasBeeper (getPos c) b
            Empty    -> isEmpty c
 

-- | Valuation function for Stmt. 
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
-- a: Stmt, b: Defs, c: World, d: Robot
stmt a b c d = case a of 
                Move          -> let x = (relativePos Front d) -- Assign x (relativePos :: Dir -> Robot -> Pos)
                                 in if isClear x c -- Check there is nothing in Front.
                                    then OK c (setPos x d)
                                    else Error ("Blocked at: " ++ show x) -- get the exact string from Test to pass the test
                PutBeeper     -> let x = getPos d  
                                  in if isEmpty d
                                    then Error ("No beeper to put.") -- get the exact string from Test to pass the test
                                    else OK (incBeeper x c) (decBag d) --Increase beeper in the world, and decrease the beeper from the bag
                -- Turn: getFacing -> cardTurn -> setFacing 
                Turn dir      -> OK (c) (setFacing (cardTurn dir (getFacing d)) d)
                Call macro    -> case lookup macro b of
                                  (Just value) -> stmt value b c d
                                  (Nothing)    -> Error ("Undefined macro: " ++ macro) --not show, because test case shows it is not string....
                                  -- get the exact string from Test to pass the test
                --Reference: While.hs from the course material.
                Iterate int st-> if int <= 0 -- Stops when int is less or equal to zero
                                  then OK c d
                                  else case stmt st b c d of
                                    (OK world robot) -> stmt (Iterate (int - 1) st) b world robot -- Iterate with 1 less value than the current value. 
                                    (Error err)      -> Error err
                                    (Done robot)     -> Done robot
                If t st1 st2  -> if (test t c d)
                                  then (stmt st1 b c d)
                                  else (stmt st2 b c d)
                While t st    -> if (test t c d)
                                  then case stmt st b c d of
                                    (OK world robot) -> stmt (While t st) b world robot
                                    (Error err)      -> Error err
                                    (Done robot)     -> Done robot
                                  else OK c d
                Block x       -> case x of
                                  []     -> OK c d -- Base case
                                  (y:ys) -> case stmt y b c d of -- Otherwise....
                                            (OK world robot) -> stmt (Block ys) b world robot -- Recurse for the rest values of the list.
                                            (Error err)      -> Error err
                                            (Done robot)     -> Done robot 
                      
                                  

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
