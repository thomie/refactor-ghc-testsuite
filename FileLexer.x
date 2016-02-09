{
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module FileLexer (
  Token,
  AlexPosn(..),
  scanner,
  main,
  module LexerTypes,
  ) where
import System.Environment (getArgs)
import Prelude hiding (length)

import LexerTypes
import Utils (unfoldWhileM)
}

%wrapper "monadUserState"

$any = [. \n]
$space = [\ \t]
@test = test $space* \(

token :-

-- Start
<0>       $white* @test               { startFirstToken test }
<0>       $any                        { startFirstToken notTest }

-- Test
<test>    \) ^ $white* \n @test       { startNewToken test }
<test>    \) ^ $white* \n [^ $space]  { startNewToken notTest }

-- Comments and multiline strings not allowed in Tests.
<test>    \#                          { addToToken `andBegin` notTest }
<test>    .^ \'\'\'                   { addToToken `andBegin` notTest }
<test>    .^ \"\"\"                   { addToToken `andBegin` notTest }
<test>    $any                        { addToToken }

-- AsIs
<notTest> $white* \n @test            { startNewToken test }
<notTest> $any                        { addToToken }

{
type Token = Tok AlexPosn

startFirstToken :: Int -> AlexAction (Token)
startFirstToken sc (p,_,_,s) n = do
    alexSetStartCode sc
    setPosn p >> setString s >> setLength n
    alexMonadScan

startNewToken :: Int -> AlexAction (Token)
startNewToken sc (p,_,_,s) n = do
    t <- getPreviousToken  -- Should be done before `alexSetStartCode`.
    alexSetStartCode sc
    setPosn p >> setString s >> setLength n
    return t

addToToken :: AlexAction (Token)
addToToken _ n = get length >>= setLength . (+n) >> alexMonadScan

getPreviousToken :: Alex (Token)
getPreviousToken = do
    s <- take . fromIntegral <$> get length <*> get string
    p <- get posn
    sc <- alexGetStartCode
    if sc == test
        then return $ Tok T p s
        else return $ Tok AsIs p s

alexEOF :: Alex Token
alexEOF = return EOF

data AlexUserState = AlexUserState
                   { string :: String
                   , length :: !Int
                   , posn :: AlexPosn
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   { string = ""
                   , length = 0
                   , posn = alexStartPos
                   }

get :: (AlexUserState -> a) -> Alex a
get f = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, f ust)

setString :: String -> Alex ()
setString ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){string=ss}}, ())

setLength :: Int -> Alex ()
setLength n = Alex $ \s -> Right (s{alex_ust=(alex_ust s){length=n}}, ())

setPosn :: AlexPosn -> Alex ()
setPosn p = Alex $ \s -> Right (s{alex_ust=(alex_ust s){posn=p}}, ())

scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
    tokens <- unfoldWhileM (/= EOF) alexMonadScan
    finaltoken <- getPreviousToken
    return $ tokens ++ [finaltoken]

main :: IO ()
main = do
    [filename] <- getArgs
    s <- readFile filename
    putStrLn ""
    case (scanner s) of
        Left msg -> error msg
        Right result -> mapM_ print result
    putStrLn ""
}
