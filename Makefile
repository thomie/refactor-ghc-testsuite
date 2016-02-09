all: test

pwd=$(/bin/pwd)

test : Main
	./Main test.T

run1 : FileLexer
	./$< test.T

run2 : Lexer
	./$< test.T

Main : FileLexer.hs Lexer.hs Parser.hs Main.hs ParserTypes.hs LexerTypes.hs Format.hs
	ghc801 -dynamic -O -Wall Main.hs

FileLexer :: FileLexer.hs Utils.hs LexerTypes.hs
	ghc801 -O -Wall -hidir=/tmp -odir=/tmp -main-is FileLexer $^

Lexer :: Lexer.hs Utils.hs LexerTypes.hs
	ghc801 -O -Wall -hidir=/tmp -odir=/tmp -main-is Lexer $^

%.hs : %.x
	alex -g $<

Parser.hs : Parser.y
	happy -g -a -c Parser.y

clean:
	rm -f *.o *.hi *.info
	rm -f Parser.hs FileLexer.hs Lexer.hs
	rm -f Main FileLexer Lexer
