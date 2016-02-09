module ParserTypes where

data Test = Test {
    white :: String,
    name :: Expr,
    setups :: [Expr],
    type_ :: Expr,
    args :: Expr
    } deriving (Eq, Show)

type Expr = [Atom]

data Atom = ASymbol String
          | AString String
          | AList [Expr]
          | ATuple [Expr]
    deriving (Show, Eq)
