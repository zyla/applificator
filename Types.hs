module Types where

import Data.List (intercalate)

-- | Lambda calculus expression with embedded portions of pure SKI.
data Lambda = LVar String | LApp Lambda Lambda | Lambda String Lambda | SKI SKI deriving (Eq)

instance Show Lambda where
    showsPrec p (LApp (LApp (LVar "(<*>)") x) y) =
        showParen (p > 2) $ showsPrec 2 x . showString " <*> " . showsPrec 3 y
    showsPrec p (LApp (LVar "(<*>)") x) = showParen True $ showsPrec 2 x . showString " <*>"
    showsPrec p (LApp (SKI f) x) = showParen (p > 2) $ showsPrec 2 f . showsPrec 3 x
    showsPrec p (LApp f x) = showParen (p > 2) $ showsPrec 2 f . showString " " . showsPrec 3 x
    showsPrec _ (LVar s) = showString s
    showsPrec p (Lambda v f) =
        let (vars, body) = dissectLambda f in
        showParen (p > 2) $ showString ("\\" ++ intercalate " " (v:vars) ++  " -> ")
                          . showsPrec 2 body
      where
        dissectLambda (Lambda v f) = let (vars, body) = dissectLambda f in (v:vars, body)
        dissectLambda body = ([], body)
    showsPrec p (SKI ski) = showsPrec p ski

-- | SKI expression
data SKI = SApp SKI SKI | S | K | I deriving (Eq)

instance Show SKI where
    showsPrec p (SApp f x) = showParen (p > 2) $ showsPrec 2 f . showsPrec 3 x
    showsPrec _ S = showString "S"
    showsPrec _ K = showString "K"
    showsPrec _ I = showString "I"

-- | Helpers for pattern matching on nested applications - much more readable.
pattern A1 f x = SApp f x
pattern A2 f x y = SApp (SApp f x) y
pattern A3 f x y z = SApp (SApp (SApp f x) y) z
