{
module Parser(parse) where

import ParserTypes
import qualified Lexer as L
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
  'test'   { L.Tok L.T            _ $$ }
  SYMBOL   { L.Tok L.Symbol       _ $$ }
  ','      { L.Tok L.Comma        _ $$ }
  '('      { L.Tok L.OpenParen    _ $$ }
  ')'      { L.Tok L.CloseParen   _ $$ }
  '['      { L.Tok L.OpenBracket  _ $$ }
  ']'      { L.Tok L.CloseBracket _ $$ }
  STRING   { L.Tok L.String       _ $$ }
  WHITE    { L.Tok L.Whitespace   _ $$ }

%%

test :: { Test }
  : white 'test' white '(' expr ',' setups ',' expr ',' expr comma ')' white
    { Test $1 (reverse $5) $7 (reverse $9) (reverse $11) }

-- Normalise setup function: either a list of or a single expression.
setups :: { [Expr] }
  : expr                    { case $1 of
                                [AList xs] -> xs
                                atoms      -> [reverse atoms] }

expr :: { Expr }
  -- Handle all whitespace here.
  : white atom white        { [$2] }
  | expr atom white         { $2 : $1 }

atom :: { Atom }
  : SYMBOL                  { ASymbol $1 }
  | STRING                  { AString $1 }
  | list                    { AList $1 }
  | tuple                   { ATuple $1 }

list :: { [Expr] }
  : '[' sequence comma ']'  { reverse $2 }

tuple :: { [Expr] }
  : '(' sequence comma ')'  { reverse $2 }

sequence :: { [Expr] }
  : sequence ',' expr       { reverse $3 : $1 }
  | expr                    { [reverse $1] }
  | white                   { [] }

white :: { String }
  : WHITE                   { $1 }
  | {- empty -}             { "" }

comma :: { }
  : ',' white               { }
  | {- empty -}             { }

{
parseError :: [L.Token] -> a
parseError (L.Tok tokentype (L.AlexPn _ line col) b : _)
  = error $ "Parse error at line " ++ show line ++ " column " ++ show col ++
          ": " ++ show tokentype ++ " " ++ show b
parseError (L.EOF : _) = error $ "Parse error: end of file?"
parseError [] = error "Parse error, no tokens left?"
}
