{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- | Convert any lambda calculus expression to SKI, and then back to Haskell.
-- > S = (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
-- > K = pure  :: a -> (r -> a)
-- > I = id    :: a -> a
module Main where

import Control.Applicative
import Data.List

-- | Lambda calculus expression with embedded portions of pure SKI.
data Lambda = LVar String | LApp Lambda Lambda | Lambda String Lambda | SKI SKI

-- | SKI expression
data SKI = SApp SKI SKI | S | K | I deriving (Eq)

-- | Helpers for pattern matching on nested applications - much more readable.
pattern A1 f x = SApp f x
pattern A2 f x y = SApp (SApp f x) y
pattern A3 f x y z = SApp (SApp (SApp f x) y) z

skiSimpl :: Lambda -> Lambda
skiSimpl (Lambda v f@(Lambda _ _)) =
    case skiSimpl f of
        SKI f' -> SKI $ SApp K f'
        f' -> Lambda v f'

skiSimpl (Lambda v (LApp f x)) = LApp (LApp (SKI S) (Lambda v f)) (Lambda v x)

skiSimpl (Lambda v (LVar s))
    | v == s    = SKI I
    | otherwise = LApp (SKI K) (LVar s)

skiSimpl (Lambda v (SKI f)) = SKI $ SApp K f

skiSimpl (LVar v) = error $ "free variable " ++ v

skiSimpl (LApp f x) =
    case (skiSimpl f, skiSimpl x) of
        (SKI f', SKI x') -> SKI $ SApp f' x'
        (f', x') -> LApp f' x'

skiSimpl (SKI ski) = SKI ski

skiSimpls (SKI ski) = [SKI ski]
skiSimpls l = l : skiSimpls (skiSimpl l)

id' :: Lambda
id' = Lambda "x" $ LVar "x"

apply :: Lambda
apply = Lambda "f" $ Lambda "x" $ LApp (LVar "f") (LVar "x")

compose :: Lambda
compose = Lambda "f" $ Lambda "g" $ Lambda "x" $ LApp (LVar "f") (LApp (LVar "g") (LVar "x"))

flip' :: Lambda
flip' = Lambda "f" $ Lambda "x" $ Lambda "y" $ LApp (LApp (LVar "f") (LVar "y")) (LVar "x")

simplSKI :: SKI -> SKI
simplSKI (A1 I x) = x
simplSKI (A2 K _ x) = x
simplSKI (A2 S (A1 K f) (A1 K x)) = A1 K (A1 f x)
simplSKI (A1 f x) = A1 (simplSKI f) (simplSKI x)
simplSKI ski = ski

backToLambda :: SKI -> Lambda
backToLambda (SApp f x) = LApp (backToLambda f) (backToLambda x)
backToLambda S = LVar "(<*>)"
backToLambda K = LVar "pure"
backToLambda I = LVar "id"

instance Show Lambda where
    showsPrec p (LApp (LApp (LVar "(<*>)") x) y) =
        showParen (p > 2) $ showsPrec 2 x . showString " <*> " . showsPrec 3 y
    showsPrec p (LApp (LVar "(<*>)") x) = showParen True $ showsPrec 2 x . showString " <*>"
    showsPrec p (LApp (SKI f) x) = showParen (p > 2) $ showsPrec 2 f . showsPrec 3 x
    showsPrec p (LApp f x) = showParen (p > 2) $ showsPrec 2 f . showString " " . showsPrec 3 x
    showsPrec _ (LVar s) = showString s
    showsPrec p (Lambda v f) = let (vars, body) = dissectLambda (Lambda v f) in
        showParen (p > 2) $ showString ("\\" ++ intercalate " " vars ++  " -> ") . showsPrec 2 body
    showsPrec p (SKI ski) = showsPrec p ski


dissectLambda (Lambda v f) = let (vars, body) = dissectLambda f in (v:vars, body)
dissectLambda body = ([], body)


instance Show SKI where
    showsPrec p (SApp f x) = showParen (p > 2) $ showsPrec 2 f . showsPrec 3 x
    showsPrec _ S = showString "S"
    showsPrec _ K = showString "K"
    showsPrec _ I = showString "I"

ski l = let SKI expr = last (skiSimpls l) in expr

whileNE [] = []
whileNE [x] = [x]
whileNE (x:y:xs)
    | x == y = [x]
    | otherwise = x : whileNE (y:xs)

main =
    let input = flip'
        solutions = whileNE $ iterate simplSKI $ ski input
    in mapM_ print (skiSimpls input) >> mapM_ print solutions >> print (backToLambda $ last solutions)

-- Random shit

-- pure (<*>)
--  <*> (pure (pure (<*>) <*>)
--       <*> (pure (<*>)
--            <*> (pure (pure (<*>) <*>)
--                 <*> (pure (pure pure <*>)
--                      <*> (pure pure <*> id)
--                     )
--                )
--            <*> (pure (pure id))
--           )
--      )
--  <*> (pure (pure pure <*> id))
--

-- \f -> \x -> f (f (f x))
-- \f -> S (\x -> f) (\x -> f (f x))
-- \f -> S (K f) (S (\x -> f) (\x -> f x))
-- \f -> S (K f) (S (K f) (S (\x -> f) (\x -> x)))
-- \f -> S (K f) (S (K f) (S (K f) I))
-- S (\f -> S (K f)) (\f -> (S (K f) (S (K f) I)))
-- S (S (\f -> S) (\f -> K f)) (S (\f -> S (K f)) (\f -> S (K f) I)))
-- S (S (K S) (\f -> K f)) (S (\f -> S (K f)) (\f -> S (K f) I)))

-- \f -> \x -> f (f (f x))
-- S (\f -> S (\x -> f)) (\f x -> f (f x))
-- S (S (\f -> S) (\f x -> f)) (\f -> S (\x -> f) (\x -> f x))
-- S (S (K S) (\f -> K f)) (\f -> S (\x -> f) (\x -> f x))

-- \x -> (SKx) (KIx)
-- \x -> (\y -> (Ky) (xy)) I
-- \x -> (KI) (xI)
--
-- S(S(KS)(S(S(KS))(S(S(KK))(S(KK)I))))(S(S(KS)(S(S(KS))(S(S(KK))(KI))))(K(KI)))
--
--
-- (b -> c) -> (a -> b) -> (a -> c)
