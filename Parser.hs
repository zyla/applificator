module Parser where

import Text.Parsec
import Text.Parsec.String

import Types

identifier = many1 lower

lambdaExpr :: Parser Lambda
lambdaExpr = apply

term =  (lambda <?> "lambda abstraction")
    <|> (var <?> "variable")
    <|> parens

var = LVar <$> identifier

lambda :: Parser Lambda
lambda = do
    char '\\'
    vars <- many1 (identifier <* spaces)
    string "->" >> spaces
    body <- lambdaExpr
    return $ mkLambda vars body

mkLambda [] body = body
mkLambda (x:xs) body = Lambda x (mkLambda xs body)

parens :: Parser Lambda
parens = char '(' *> spaces *> lambdaExpr <* spaces <* char ')'

apply :: Parser Lambda
apply = do
    exprs <- sepBy1 term spaces
    return $ foldl1 LApp exprs

parseLambda :: String -> Either ParseError Lambda
parseLambda = parse (spaces *> lambdaExpr <* spaces <* eof) "(expression)"
