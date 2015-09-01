Convert any lambda calculus expression to SKI, and then back to Haskell.

This is based on the fact that all three combinators of SKI already exist in
Haskell standard library (`Control.Applicative` needs to be imported though).

    S = (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
    K = pure  :: a -> (r -> a)
    I = id    :: a -> a

Usage:

    applificator <expression>

Example: see [example.txt](./example.txt).
