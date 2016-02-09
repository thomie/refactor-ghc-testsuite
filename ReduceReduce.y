{
module ReduceReduce(main) where
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  A        { A }

%%

top :: { String }
    : x { "first" }
    | y { "second" }

x : A { }
y : A { }

{
data Token = A

parseError :: [Token] -> a
parseError _ = error "Parse error"

main = print $ parse $ [A]

{- Prints "second", wherease happy documentation says in
   https://www.haskell.org/happy/doc/html/sec-conflict-tips.html:
   "A reduce/reduce conflict implies that a certain sequence of tokens on the
   input can represent more than one non-terminal, and the parser is
   uncertain as to which reduction rule to use.
   It will select the reduction rule uppermost in the grammar file, so if
   you really must have a reduce/reduce conflict you can select which rule
   will be used by putting it first in your grammar file."
 -}
}
