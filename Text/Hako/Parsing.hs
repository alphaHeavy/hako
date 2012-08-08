{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Text.Hako.Parsing
( parseTemplateFromString
) where

import Control.Monad.Trans.Class
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Text.Parsec
import Text.Parsec.String
import Text.Hako.Html

-- | Hako's main parser, suitable as a quoteExpr.
parseTemplateFromString :: String -> Q Exp
parseTemplateFromString s = do
  exp <- runParserT template () "Hako" s
  return $ either (error . show) id exp

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

template :: ParsecT String () Q Exp
template = do
    tfs <- many templateFragment
    return $ tpack $ foldl1 tjoin tfs

templateFragment :: ParsecT String () Q Template
templateFragment = try templateDefFragment
                 <|> try templateLoopFragment
                 <|> templateExpFragment
                 <|> templateLitFragment
                 <?> "template fragment"

templateDefFragment :: ParsecT String () Q Template
templateDefFragment = do
    def <- blockDef
    return $ Template [def] []

templateLoopFragment :: ParsecT String () Q Template
templateLoopFragment = do
    exp <- forLoop
    return $ Template [] [exp]

templateExpFragment :: ParsecT String () Q Template
templateExpFragment = do
    exp <- haskellExpr
    return $ Template [] [exp]

templateLitFragment :: ParsecT String () Q Template
templateLitFragment = do
    exp <- literalText
    return $ Template [] [exp]

emptyLiteralExp :: Exp
emptyLiteralExp = AppE (ConE 'Html) $ LitE $ StringL ""

expJoin :: Exp -> Exp -> Exp
expJoin a b = AppE (AppE (VarE '(<>)) a) b

expWrap :: Exp -> Exp
expWrap a = AppE (VarE 'toHtml) a

-- We might have a {def const = some fixed template}
-- Or we might have {def f x y = some dynamic template using {x} and {y}}
-- We could also have patterns {def f (x:_) = first {x}}
blockDef :: ParsecT String () Q Dec
blockDef = do
    string "{def"
    space
    leader <- manyTill anyChar (char '=')
    inner <- template
    string "}"
    case parseExp ("let " ++ leader ++ " = 42 in 42") of
      Right (LetE [ValD p _ _] _) -> return $ ValD p (NormalB inner) []
      Right (LetE [FunD n [Clause ps _ _]] _) -> return $ FunD n [Clause ps (NormalB inner) []]
      Right _ -> error "the definition leader did not parse into one of the expected constructs"
      Left err -> error err

-- Turn {for x in list: <a>{x}</a>} into: mconcat . map (\x -> TEMPLATE_EXPR) list
forLoop :: ParsecT String () Q Exp
forLoop = do
    string "{for"
    space
    var <- manyTill anyChar $ string " in "
    lst <- manyTill anyChar $ char ':'
    inner <- template
    string "}"
    let varPat = either error id $ parsePat var
        funExpr = LamE [varPat] inner
        lstExpr = parseExp lst
    case lstExpr of
      Right e -> return $ VarE 'mconcat `AppE` (VarE 'map `AppE` funExpr `AppE` e)
      Left err -> error err

literalText :: ParsecT String () Q Exp
literalText = do
    str <- many1 $ noneOf "{}"
    return $ AppE (ConE 'Html) $ LitE $ StringL str

haskellExpr :: ParsecT String () Q Exp
haskellExpr = do
    e <- haskellExpr'
    return $ expWrap e

haskellExpr' :: ParsecT String () Q Exp
haskellExpr' = do
    _ <- char '{'
    src <- haskellText
    _ <- char '}'
    either fail return $ parseExp src

haskellText :: ParsecT String () Q String
haskellText = do
    parts <- many1 haskellPart
    return $ concat parts

bracedText :: ParsecT String () Q String
bracedText = do
    char '{'
    inner <- haskellText
    char '}'
    return $ "{" ++ inner ++ "}"

haskellPart :: ParsecT String () Q String
haskellPart = quotedChar
            <|> quotedEscapedChar
            <|> quotedString
            <|> bracedText
            <|> haskellOther

haskellOther :: ParsecT String () Q String
haskellOther = many1 $ noneOf "\"'{}"

quotedChar :: ParsecT String () Q String
quotedChar = do
    char '\''
    c <- noneOf "\\"
    char '\''
    return ['\'', c, '\'']

quotedEscapedChar :: ParsecT String () Q String
quotedEscapedChar = do
    char '\''
    char '\\'
    c <- anyChar
    char '\''
    return ['\'', '\\', c, '\'']

quotedString :: ParsecT String () Q String
quotedString = do
    char '"'
    strs <- many quotedStringPart
    char '"'
    let str = concat strs
    return $ "\"" ++ str ++ "\""
    where quotedStringPart = singleChar <|> escapedChar
          singleChar = do { c <- noneOf "\"\\"; return [c] }
          escapedChar = do { char '\\'; c <- anyChar; return ['\\',c] }
