{
module Parser(main) where
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  A        { A }
  B        { B }
  C        { C }
  D        { D }
  E        { E }

%%

top :: { String }
    : A x C D { "first" }
    | A B C E { "second" }

x : B { }

{
data Token = A | B | C | D | E

parseError :: [Token] -> a
parseError _ = error "Parse error"

main = print $ parse $ [A, B, C, D]

{- Results in "ShiftReduce.hs: Parse error", which I found surprising. -}
}
