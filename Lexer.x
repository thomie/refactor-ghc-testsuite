{
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Lexer (
  Token,
  AlexPosn(..),
  scanner,
  main,
  module LexerTypes,
  ) where
import System.Environment (getArgs)

import LexerTypes
import Utils (unfoldWhileM)
}

%wrapper "monad" -- Only for alexSetInput

$special = [\(\)\[\]\,\#\'\"]
$symbol = [^ $special $white]
$any = [. \n]

token :-

<0>    $white*             { tok Whitespace }
<0>    ^test               { tok T `andBegin` test }
<test> $symbol+            { tok Symbol }

<test> \,                  { tok Comma }
<test> \(                  { tok OpenParen }
<test> \)                  { tok CloseParen }
<test> \[                  { tok OpenBracket }
<test> \]                  { tok CloseBracket }

<test> \" ($any # \")* \"  { tok String }
<test> \' ($any # \')* \'  { tok String }

<test> \#.*                { tok Comment }
<test> $white+             { tok Whitespace }

{
type Token = Tok AlexPosn

tok :: TokenType -> AlexAction (Token)
tok tokentype (p,_,_,input) len =
  return (Tok tokentype p (take (fromIntegral len) input))

alexEOF :: Alex Token
alexEOF = return EOF

scanner :: AlexPosn -> String -> Either String [Token]
scanner posn str = runAlex str $ do
    -- Assign correct position to tokens and error messages.
    (_, c, inp, bpos) <- alexGetInput
    alexSetInput (posn, c, inp, bpos)
    unfoldWhileM (/= EOF) alexMonadScan

main :: IO ()
main = do
    [filename] <- getArgs
    s <- readFile filename
    putStrLn ""
    case (scanner alexStartPos s) of
        Left msg -> error msg
        Right result -> mapM_ print result
    putStrLn ""
}
