-- CS 381 Homework #3
-- Author1 : Jaehyung You
-- ONID ID: youja
-- Date : 2019 - 02 - 05

module HW3 where
    import Prelude hiding (Num) -- Hiding Num in prelude -- since I will define Num in this program.
    -- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
    -- Source: http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.OldList.html#intercalate
    import Data.List -- For using intercalate 
    -- Define the data type

    -- mode	::=	down   |   up
    data Mode = Up | Down
                deriving (Eq,Show)


    -- expr	::=	var	        variable reference
    --  | num	        literal number
    --  | expr + expr	addition expression

    data Expr = Vr Var
                | Ln Num
                | Ae Expr Expr
                deriving (Eq,Show)

    -- cmd	::=	pen mode	                    change pen mode
    --       | move ( expr , expr )	            move pen to a new position
    --       | define macro ( var* ) { prog }  	define a macro
    --       | call macro ( expr* )	            invoke a macro
   
    data Cmd = Pen Mode
            | Move (Expr, Expr)
            | Define Macro [Var] Prog
            | Call Macro [Expr]
            deriving (Eq,Show)
                
    -- Define the types

    -- num	    ::=	Int	
    -- var	    ::=	String
    -- macro	::=	String
    -- prog     ::= [Cmd] -- Sequence of cmd
    
    type Num = Int
    type Var = String
    type Macro = String
    type Prog = [Cmd]

    -- Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas) draws a line segment from (x1,y1) to (x2,y2).
    
    -- Concrete Syntax
    -- define line (x1, y1, x2, y2)
    -- {
    --  pen up; move (x1, y1);
    --  pen down; move (x2, y2);
    -- }

    line :: Cmd
    line = Define "line" ["x1","y1","x2","y2"] 
                [Pen Up, Move (Vr "x1", Vr "y1"),
                Pen Down, Move (Vr "x2", Vr "y2")]

    -- Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) that draws a big “X” of width w and height h, starting from position (x,y). 
    -- Your definition should not contain any move commands
  
    -- Concrete Syntax
    -- define nix (x, y, w, h)
    -- {
    --  line(x, y, x+w, y+h);
    --  line(x, y+h, x+w, y);
    -- }
    nix :: Cmd
    nix = Define "nix" ["x","y","w","h"] 
                    [Call "line" 
                    [Vr "x", Vr "y", Ae (Vr "x") (Vr "w"), Ae (Vr "y") (Vr "h")],
                    Call "line"
                    [Vr "x", Ae (Vr "y") (Vr "h"), Ae (Vr "x") (Vr "w"), Vr "y"]]
    
    -- Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program that draws a staircase of n steps starting from (0,0). 
    -- Below is a visual illustration of what the generated program should draw for a couple different applications of steps.
    -- It expends the loop until the loop hits 0.
    steps :: Int -> Prog
    steps 0 = [Pen Up]  ++ [Move (Ln 0, Ln 0)] ++  [Pen Down] -- base case
    steps i = steps (i-1) ++ [Move (Ln i, Ln (i-1))] ++ [Move (Ln i, Ln i)]

    -- Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names of all of the macros that are defined anywhere in a given MiniLogo program. 
    -- Don’t worry about duplicates—if a macro is defined more than once, the resulting list may include multiple copies of its name
    macros :: Prog -> [Macro]
    macros [] = []  -- Base case
    macros ((Define x y z): xs) = x : macros xs
    macros ( _ : xs) = macros xs -- For other data types: Pen, Call, Move

    -- Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program. 
    pretty :: Prog -> String
    pretty [] = "" -- base case
    pretty (Pen Up:xs) = "Pen Up;\n" ++ pretty xs
    pretty (Pen Down:xs) = "Pen Down;\n" ++ pretty xs
    -- From helper function:
    -- intercalate : http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:intercalate
    -- It inserts the list xs in between the lists in xss and concatenates the result.
    -- Reference: https://stackoverflow.com/questions/5735687/pretty-printing-a-syntax-tree-in-haskell
    pretty ((Move (x,y)):xs) = "Move (" ++ (getExpr x) ++ "," ++ (getExpr y) ++ "); \n" ++ pretty xs
    pretty ((Define x y z):xs) = "Define " ++ x ++ "(" ++ intercalate "," y ++ ") { \n" ++ pretty z ++ "}; \n" ++ pretty xs
    pretty ((Call x y):xs) = "Call " ++ x ++ "(" ++ intercalate "," (map getExpr y) ++ "); \n" ++ pretty xs

    -- Helper function for pretty (move)
    getExpr :: Expr -> String
    getExpr (Vr a) = a -- Variable Reference
    -- Ref: http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:show
    getExpr (Ln a) = show a -- Literal Numbers to String
    getExpr (Ae a b) = getExpr a ++ "+" ++ getExpr b