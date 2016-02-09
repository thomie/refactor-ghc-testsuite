module LexerTypes where

data Tok p -- AlexPosn
    = Tok TokenType p String
    | EOF
    deriving (Eq, Show)

data TokenType
    = AsIs
    | T
    | Symbol
    | Comma
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | String
    | Whitespace
    | Comment
    deriving (Eq, Show)
