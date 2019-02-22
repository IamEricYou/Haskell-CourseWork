
module Type where

    data Expr = LitI Int -- literal integer
                | LitS String -- literal string
                | Add Expr Expr -- add two integers
                | Cat Expr Expr -- concatenate two strings
                | Show Expr -- convert an integer into a string


    data Type = Newint | Newstring | Newerror

    types :: Expr -> Type
    types (LitI a) = Newint
    types (LitS a) = Newstring
    types (Add l r) = case (types l, types r) of
                    (Newint, Newint) -> Newint
                    _ -> Newerror
    types (Cat l r) = case (types l, types r) of
                    (Newstring, Newstring) -> Newstring
                    _ -> Newerror

    types (Show a) = case (types a) of
                    Newint -> Newstring
                    _ -> Newerror
