{-# LANGUAGE OverloadedStrings #-}
module Format(showTest) where

import Text.PrettyPrint
import ParserTypes

formatExpr :: Expr -> Doc
formatExpr [] = ""
formatExpr (x:xs) = fst $ foldl combine (formatAtom x, x) xs
  where
    --      previous atom ----vvvv     vvvv---- current attom
    combine :: (Doc, Atom) -> Atom -> (Doc, Atom)
    combine (s, (ASymbol _)) t@(ATuple _) = (s <> formatAtom t, t) -- funcall
    combine (s, _          ) a            = (s <+> formatAtom a, a)

formatAtom :: Atom -> Doc
formatAtom (ASymbol s) = text s
formatAtom (AString s) = text s
formatAtom (AList exps) = brackets $ fsep $ punctuate comma (map formatExpr exps)
formatAtom (ATuple exps) = parens $ fsep $ punctuate comma (map formatExpr exps)

formatTest :: Test -> Doc
formatTest test = doc
  where
    doc = "test" <> parens (fsep (punctuate comma [
        formatExpr (name test),
        setups',
        formatExpr (type_ test),
        formatExpr (args test)
        ]))
    -- Magic: when only one setup function, don't enclose it in brackets.
    -- TODO. Remove this?
    --setups' | [x] <- (setups test) = formatExpr x
    --        | otherwise = setups''
    setups' = brackets $ fsep $ punctuate comma $ map formatExpr (setups test)

showTest :: Test -> String
showTest test = white test ++ renderStyle sty (formatTest test)
  where
    -- Lower ribbons -> prefer longer lines.
    -- PageMode ZigZagMode LeftMode OneLineMode
    sty = Style PageMode 80 1
