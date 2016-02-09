{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Control.Monad
import Debug.Trace
import System.Environment

import qualified FileLexer as File
import qualified Lexer
import qualified Parser
import ParserTypes

import Format (showTest)

parseTest :: File.Token -> Test
parseTest (File.Tok File.T p s) =
    either error Parser.parse (Lexer.scanner (filePos2Pos p) s)
  where
    filePos2Pos :: File.AlexPosn -> Lexer.AlexPosn
    filePos2Pos (File.AlexPn i j k) = Lexer.AlexPn i j k
parseTest _                = error "Not a Test"

deleteSetups :: Test -> Test
deleteSetups test = test{setups = filter (not . exclude) (setups test)}
  where
    exclude :: Expr -> Bool
    exclude (ASymbol "only_compiler_types":_) = True
    exclude (ASymbol "when" : ATuple ((ASymbol "compiler_lt":_) :_) :_) = True
    exclude _ = False

postProcessTest :: Test -> Test
postProcessTest test | [[ASymbol "normal"]] <- setups test = test {setups = []}
postProcessTest test = test

processTest :: Test -> Test
processTest test = deleteSetups test

showToken :: Bool -> File.Token -> String
showToken reformat token@(File.Tok File.T _ s) =
    if reformat || test /= test' then showTest (postProcessTest test') else s
  where
    test' = processTest test
    test = parseTest token
showToken _ (File.Tok _ _ s) = s
showToken _ File.EOF = "EOF"

doIt :: String -> IO ()
doIt filename = do
    s <- readFile filename

    let reformat = filename == "test.T"

    let tokens = either error id (File.scanner s)
        strings = map (showToken reformat) tokens
        string = concat strings

    let outfile = if filename == "test.T" then "test.stdout" else filename
    print outfile

    string `seq` writeFile outfile string
    when (last string /= '\n') $ appendFile outfile "\n"

main :: IO ()
main = getArgs >>= mapM_ doIt
