{-# LANGUAGE OverloadedStrings #-}

module Few.Parse
    ( parseFew
    , parseFile
    ) where

----------------------------------------

import              Control.Monad
import              Control.Applicative
import              Data.Functor 

import qualified    Text.Parsec             as P
import              Text.Parsec.Char        
import qualified    Data.Text               as T
import              Data.List
import              Data.Text.IO            as TIO

import              Few.Types

----------------------------------------

type FewParser = P.Parsec T.Text ()

parseFew :: FewParser Few
parseFew =
    fewParser
    <|> header1Parser 
    <|> header2Parser 
    <|> header3Parser 
    <|> header4Parser 
    <|> header5Parser
    <|> header6Parser 
    <|> boldParser 
    <|> italicParser
    <|> underlineParser
    <|> strikethroughParser
    <|> paraParser
    <|> breakParser
    <|> codeParser
    <|> orderedParser
    <|> unorderedParser
    <|> listItemParser
    <|> quoteParser
    <|> htmlParser
    <|> linkParser
    <|> mathParser
    <|> escapedParser
    <|> plainText

tstring = P.try . string

parseTill :: String -> FewParser [Few]
parseTill ending = 
    (tstring ending *> pure []) 
    <|> ( (:) <$> parseFew <*> parseTill ending)

parseAttrsAll :: FewParser [NodeAttr]
parseAttrsAll = P.option [] $ 
    (tstring "[" *> parseAttrs)

parseAttrs :: FewParser [NodeAttr]
parseAttrs = P.option [] $ ((:) <$> parseAttr <*> parseAttrs)

parseAttr :: FewParser NodeAttr
parseAttr = classAttr <|> idAttr <|> langAttr

alternativesOf :: [String] -> FewParser String
alternativesOf []        = error "No alternative!"
alternativesOf [x]       = (tstring x *> pure "")
alternativesOf (s:anyOf) = (tstring s *> pure "") <|> alternativesOf anyOf

readTill :: [String] -> FewParser String
readTill anyOf = 
    alternativesOf anyOf
    <|> ( (:) <$> anyChar <*> readTill anyOf)  

specificAttr :: Opening -> AttrName -> FewParser NodeAttr
specificAttr op attr = do
    tstring op
    s <- readTill [",", "]"]
    return $ NodeAttr (attr, s)

classAttr = specificAttr "." "class"
idAttr    = specificAttr "#" "id"
langAttr  = specificAttr "!" "class"

normalParser :: Opening -> Ending -> NodeType -> FewParser Few
normalParser op ed nt = do
    -- P.parserTrace $ show nt
    _           <- tstring op
    attrs       <- parseAttrsAll
    childlist   <- parseTill ed
    return $ FewNode 
        { node          = nt
        , attributes    = attrs
        , children      = childlist
        }

emptyParser :: Opening -> Ending -> NodeType -> FewParser Few
emptyParser op ed nt = do
    -- P.parserTrace $ show nt
    _       <- tstring op
    attrs   <- parseAttrsAll
    content <- readTill [ed]

    return $ FewNode 
        { node = nt
        , attributes = attrs
        , children = [PlainText content]
        }

escapeParser :: Opening -> NodeType -> FewParser Few
escapeParser op nt = do
    _       <- tstring op
    content <- anyChar

    return $ FewNode 
        { node = nt
        , attributes = []
        , children = [PlainText [content]]
        }

linkParser :: FewParser Few
linkParser = do
    _           <- tstring "@"
    attrs       <- parseAttrsAll
    linkText    <- readTill ["(->"]
    linkURL     <- readTill ["<-)"]
     
    return $ FewNode
        { node = Link
        , attributes = (NodeAttr ("href", linkURL)) : attrs
        , children = [PlainText linkText]
        }

flagParser :: Opening -> NodeType -> FewParser Few
flagParser op nt = do
    _           <- tstring op
    attrs       <- parseAttrsAll 
    
    return $ FewNode 
        { node = nt
        , attributes = attrs
        , children = []
        }

plainText :: FewParser Few
plainText = do
    c <- anyChar
    return $ PlainText [c]

fewParser       = normalParser "-few-"  "-few-"     FewContent 
boldParser      = normalParser "**"     "**"        Bold
italicParser    = normalParser "_"      "_"         Italic
underlineParser = normalParser "__"     "__"        Underline
strikethroughParser 
                = normalParser "---"    "---"       Strikethrough
codeParser      = emptyParser  "```"    "```"       Code
header1Parser   = emptyParser  "#1"     "\n"        Header1
header2Parser   = emptyParser  "#2"     "\n"        Header2
header3Parser   = emptyParser  "#3"     "\n"        Header3
header4Parser   = emptyParser  "#4"     "\n"        Header4
header5Parser   = emptyParser  "#5"     "\n"        Header5
header6Parser   = emptyParser  "#6"     "\n"        Header6
orderedParser   = normalParser "(12)"   "--"        OrderedList
unorderedParser = normalParser "(..)"   "--"        UnorderedList
listItemParser  = normalParser "%"      "\n"        ListItem
quoteParser     = normalParser ">>"     "<<"        Quote
htmlParser      = emptyParser  "-html-" "-html-"    HTML
mathParser      = emptyParser  "$$"     "$$"        Math
escapedParser   = escapeParser "\\"                 Escaped 
breakParser     = flagParser   "==="                Break
paraParser      = flagParser   "~~~"                Paragraph


parseFile :: FilePath -> IO Few
parseFile fp = do
    content <- TIO.readFile fp
    let result = P.runParser parseFew () fp content
    case result of 
        Left err    -> error $ show err
        Right few   -> return few

