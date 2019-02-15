-- CS 381 Homework #4
-- Author1 : Jaehyung You
-- ONID ID: youja
-- Date : 2019 - 02 - 13

module HW3 where

    import MiniMiniLogo
    import Render
    
    --
    -- * Semantics of MiniMiniLogo
    --
    
    -- NOTE:
    --  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
    --    functions for generating MiniMiniLogo programs. It contains the type
    --    definitions for Mode, Cmd, and Prog.
    --  * Render.hs contains code for rendering the output of a MiniMiniLogo
    --    program in HTML5. It contains the types definitions for Point and Line.
    
    -- | A type to represent the current state of the pen.
    type State = (Mode,Point)
    
    -- | The initial state of the pen.
    start :: State
    start = (Up,(0,0))
    
    -- | A function that renders the image to HTML. Only works after you have
    --   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
    --   produce an HTML file named MiniMiniLogo.html, which you can load in
    --   your browswer to view the rendered image.
    draw :: Prog -> IO ()
    draw p = let (_,ls) = prog p start in toHTML ls
    
    
    -- Semantic domains:
    --   * Cmd:  State -> (State, Maybe Line)
    --   * Prog: State -> (State, [Line])
    
    
    -- | Semantic function for Cmd.
    --   
    --   >>> cmd (Pen Down) (Up,(2,3))
    --   ((Down,(2,3)),Nothing)
    --
    --   >>> cmd (Pen Up) (Down,(2,3))
    --   ((Up,(2,3)),Nothing)
    --
    --   >>> cmd (Move 4 5) (Up,(2,3))
    --   ((Up,(4,5)),Nothing)
    --
    --   >>> cmd (Move 4 5) (Down,(2,3))
    --   ((Down,(4,5)),Just ((2,3),(4,5)))
    --
    cmd :: Cmd -> State -> (State, Maybe Line)
    -- If s -> up, then s -> up. 
    -- If s -> down, then s -> down
    cmd (Pen s) (a,b)                = ((s, b), Nothing)

    -- Down -> Just, and Up -> Nothing
    cmd (Move x2 y2) (Down, (x1, y1)) = ((Down, (x2, y2)), 
                                          Just ((x1, y1), (x2, y2)))
    cmd (Move x2 y2) (Up, (x1, y1))   = ((Up, (x2, y2)),
                                          Nothing)
    
    
    -- | Semantic function for Prog.
    --
    --   >>> prog (nix 10 10 5 7) start
    --   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
    --
    --   >>> prog (steps 2 0 0) start
    --   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
    prog :: Prog -> State -> (State, [Line])
    -- It is the base case.
    prog [] s = (s, []) 
    -- prog Prog State,
    -- If Nothing -> it recures with other value in the list
    -- If Just value -> it recurses with the value as a new list (Using lambda)
    -- cmd :: Cmd -> State -> (State, Maybe Line)
    prog (x:xs) s = case (cmd x s) of
                    (states, Nothing)  -> prog xs states
                    -- Reference: https://stackoverflow.com/questions/33091532/using-lambda-to-implement-list-function-in-haskell
                    (states, Just a) -> (\(s, (xs)) -> (s, a:xs)) (prog xs states)
                    -- -> (State, list xs) -> (State, value will be prepended to the list xs) -> (State , [Line])
    
    --
    -- * Extra credit
    --

    -- | This should be a MiniMiniLogo program that draws an amazing picture.
    --   Add as many helper functions as you want.
    amazing :: Prog
    amazing = undefined