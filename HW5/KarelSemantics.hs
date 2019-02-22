-- CS 381 Homework #4
-- Author1 : Jaehyung You
-- ONID ID: youja
-- Date : 2019 - 02 - 22

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
test :: Test -> World -> Robot -> Bool
test (Not a) x y = not (test a x y)
 

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
