-- | Convert any lambda calculus expression to SKI, and then back to Haskell.
-- > S = (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
-- > K = pure  :: a -> (r -> a)
-- > I = id    :: a -> a
module Main where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

import Types
import Parser

-- | One step of lambda -> SKI conversion.
-- WARNING: This is a partial function (errors out on free variables).
lambdaToSKIStep :: Lambda -> Lambda
lambdaToSKIStep (Lambda v f@(Lambda _ _)) =
    case lambdaToSKIStep f of
        SKI f' -> SKI $ SApp K f'
        f' -> Lambda v f'

lambdaToSKIStep (Lambda v (LApp f x)) = LApp (LApp (SKI S) (Lambda v f)) (Lambda v x)

lambdaToSKIStep (Lambda v (LVar s))
    | v == s    = SKI I
    | otherwise = LApp (SKI K) (LVar s)

lambdaToSKIStep (Lambda v (SKI f)) = SKI $ SApp K f

lambdaToSKIStep (LVar v) = error $ "free variable " ++ v

lambdaToSKIStep (LApp f x) =
    case (lambdaToSKIStep f, lambdaToSKIStep x) of
        (SKI f', SKI x') -> SKI $ SApp f' x'
        (f', x') -> LApp f' x'

lambdaToSKIStep (SKI ski) = SKI ski

-- Iterate a given function until output equals input, i.e. a fixed point.
toFixedPoint :: Eq a => (a -> a) -> a -> [a]
toFixedPoint f x = x : xs
  where
    xs = if x == next then [] else toFixedPoint f next
    next = f x

-- | Convert lambda to SKI (without simplifications).
-- Returns a tuple of (conversion steps, final SKI).
lambdaToSKI :: Lambda -> ([Lambda], SKI)
lambdaToSKI (SKI end) = ([], end)
lambdaToSKI l =
    let (steps, expr) = lambdaToSKI $ lambdaToSKIStep l
    in (l : steps, expr)

-- | One step of SKI simplitication
simplSKIStep :: SKI -> SKI
simplSKIStep (A1 I x) = x
simplSKIStep (A2 K _ x) = x
simplSKIStep (A2 S (A1 K f) (A1 K x)) = A1 K (A1 f x)
simplSKIStep (A2 S (A1 K K) I) = K
simplSKIStep (A1 f x) = A1 (simplSKIStep f) (simplSKIStep x)
simplSKIStep ski = ski

-- | Render a SKI expression in Haskell
backToLambda :: SKI -> Lambda
backToLambda (SApp f x) = LApp (backToLambda f) (backToLambda x)
backToLambda S = LVar "(<*>)"
backToLambda K = LVar "pure"
backToLambda I = LVar "id"

-- | Convert a lambda expression to simplified SKI, returning all intermediate steps.
--
-- Returns a tuple of 3 elements:
--  * lambda-to-SKI steps
--  * SKI simplification steps
--  * final SKI expression converted back to lambda
process :: Lambda -> ([Lambda], [SKI], Lambda)
process input = (lambdaSteps, skiSteps, finalLambda)
  where
    (lambdaSteps, ski) = lambdaToSKI input
    skiSteps = toFixedPoint simplSKIStep ski
    finalLambda = backToLambda $ last skiSteps

main = do
    input <- getInputStr >>= parseOrExit
    let (lambdaSteps, skiSteps, finalLambda) = process input
    mapM_ print lambdaSteps
    mapM_ print skiSteps
    print finalLambda

parseOrExit str = case parseLambda str of
    Left error -> print error >> exitWith (ExitFailure 1)
    Right val -> return val

getInputStr = do
    args <- getArgs
    case args of
        [] -> getLine
        xs -> return $ intercalate " " xs

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
