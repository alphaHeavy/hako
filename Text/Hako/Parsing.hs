{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Text.Hako.Parsing
( parseTemplateFromString
) where

import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Text.Parsec.String
import Text.Hako.Html

-- | Hako's main parser, suitable as a quoteExpr.
parseTemplateFromString :: String -> ExpQ
parseTemplateFromString s = either (error . show) return $ parse template [] s

data Template = Template [Dec] [Exp]

tjoin :: Template -> Template -> Template
tjoin (Template dl el) (Template dr er) =
    Template (dl ++ dr) (el ++ er)

tpack :: Template -> Exp
tpack (Template defs exps) =
    let body = if null exps
                then emptyLiteralExp
                else foldl1 expJoin exps
    in if null defs
        then body
        else LetE defs body

template :: Stream s m Char => ParsecT s u m Exp
template = do
    tfs <- many templateFragment
    return $ tpack $ foldl1 tjoin tfs

templateFragment :: Stream s m Char => ParsecT s u m Template
templateFragment = try templateDefFragment
                 <|> templateExpFragment
                 <|> templateLitFragment
                 <?> "template fragment"

templateDefFragment :: Stream s m Char => ParsecT s u m Template
templateDefFragment = do
    defs <- blockDef
    return $ Template defs []

templateExpFragment :: Stream s m Char => ParsecT s u m Template
templateExpFragment = do
    exp <- haskellExpr
    return $ Template [] [exp]

templateLitFragment :: Stream s m Char => ParsecT s u m Template
templateLitFragment = do
    exp <- literalText
    return $ Template [] [exp]

emptyLiteralExp :: Exp
emptyLiteralExp = AppE (ConE . mkName $ "Html") $ LitE $ StringL ""

expJoin :: Exp -> Exp -> Exp
expJoin a b = AppE (AppE (VarE . mkName $ "<++>") a) b

expWrap :: Exp -> Exp
expWrap a = AppE (VarE . mkName $ "toHtml") a

blockDef :: Stream s m Char => ParsecT s u m [Dec]
blockDef = do
    string "{def"
    space
    leader <- manyTill anyChar $ char '=' 
    inner <- template
    string "}"
    let decs = parseDecs $ leader ++ " = " ++ pprint inner
    case decs of
        Right d -> return d
        Left err -> error err

literalText :: Stream s m Char => ParsecT s u m Exp
literalText = do
    str <- many1 $ noneOf "{}"
    return $ AppE (ConE . mkName $ "Html") $ LitE $ StringL str

haskellExpr :: Stream s m Char => ParsecT s u m Exp
haskellExpr = do
    e <- haskellExpr'
    return $ expWrap e

haskellExpr' :: Stream s m Char => ParsecT s u m Exp
haskellExpr' = do
    _ <- char '{'
    src <- haskellText
    _ <- char '}'
    either fail return $ parseExp src

haskellText :: Stream s m Char => ParsecT s u m String
haskellText = do
    parts <- many1 haskellPart
    return $ concat parts

bracedText :: Stream s m Char => ParsecT s u m String
bracedText = do
    char '{'
    inner <- haskellText
    char '}'
    return $ "{" ++ inner ++ "}"

haskellPart :: Stream s m Char => ParsecT s u m String
haskellPart = quotedChar
            <|> quotedEscapedChar
            <|> quotedString
            <|> bracedText
            <|> haskellOther

haskellOther :: Stream s m Char => ParsecT s u m String
haskellOther = many1 $ noneOf "\"'{}"

quotedChar :: Stream s m Char => ParsecT s u m String
quotedChar = do
    char '\''
    c <- noneOf "\\"
    char '\''
    return ['\'', c, '\'']

quotedEscapedChar :: Stream s m Char => ParsecT s u m String
quotedEscapedChar = do
    char '\''
    char '\\'
    c <- anyChar
    char '\''
    return ['\'', '\\', c, '\'']

quotedString :: Stream s m Char => ParsecT s u m String
quotedString = do
    char '"'
    strs <- many quotedStringPart
    char '"'
    let str = concat strs
    return $ "\"" ++ str ++ "\""
    where quotedStringPart = singleChar <|> escapedChar
          singleChar = do { c <- noneOf "\"\\"; return [c] }
          escapedChar = do { char '\\'; c <- anyChar; return ['\\',c] }
