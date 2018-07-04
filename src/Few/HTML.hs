{-# LANGUAGE OverloadedStrings, RecordWildCards, MultiWayIf #-}

module Few.HTML
    ( fewToHTML
    , writeFewAsHTML
    , convertFewFileToHTML
    ) where

----------------------------------------

import              Few.Types
import              Few.Parse

import qualified    Data.Text       as T
import qualified    Data.Text.IO    as TIO
import              Data.List

----------------------------------------

getTag :: NodeType -> String
getTag FewContent   =  "html"
getTag Bold         =  "b"
getTag Italic       =  "i"
getTag Underline    =  "u"
getTag Strikethrough = "del"
getTag Header1      =  "h1"
getTag Header2      =  "h2"
getTag Header3      =  "h3"
getTag Header4      =  "h4"
getTag Header5      =  "h5"
getTag Code         =  "code"
getTag OrderedList  =  "ol"
getTag UnorderedList=  "ul"
getTag Quote        =  "blockquote"
getTag Link         =  "a"
getTag Math         =  "script type=\"math/tex\""
getTag Paragraph    =  "p"
getTag Break        =  "hr"
getTag ListItem     =  "li"
getTag _            =  ""

tagHTML :: NodeType -> [NodeAttr] -> (T.Text, T.Text)
tagHTML node attrs = (\(x, y) -> (T.pack x, T.pack y)) $ 
    if 
        | node `elem` [ FewContent
                      , Bold
                      , Italic
                      , Underline
                      , Strikethrough
                      , Header1
                      , Header2
                      , Header3
                      , Header4
                      , Header5
                      , Header6
                      , OrderedList
                      , UnorderedList
                      , Quote
                      , Link
                      , Math
                      ]                 -> ("<" ++
                                            tag ++
                                            " " ++
                                            attrstr ++
                                            ">"
                                            ,
                                            "</" ++
                                            tag ++
                                            ">")
        | node `elem` [ Paragraph
                      , Break
                      , ListItem
                      ]                 -> ("<" ++
                                            tag ++
                                            " " ++
                                            attrstr ++
                                            ">"
                                            ,
                                            "")
        | node == Code                  -> ("<pre><code " ++
                                            attrstr ++
                                            ">"
                                            ,
                                            "</code></pre>")
        | otherwise                     -> ("", "")     -- HTML tag, Plaintext, and escaped

    where 
        tag = getTag node
        attrstr = concatMap (\(NodeAttr (attr, val)) -> attr ++ " = \"" ++ val ++ "\"") 
                    attrs


generateHTML :: Few -> T.Text
generateHTML (PlainText txt) = T.pack txt
generateHTML (FewNode {..}) = 
    let rest = T.concat $ map generateHTML children
        (opening, closing) = tagHTML node attributes

    in  T.concat [opening, rest, closing]


htmlReplacements = []
{--
    [ ("<",     "&gt;"  )
    , (">",     "&lt;"  )
    , (" ",     "&nbsp;")
    , ("\"",    "&quot;")
    , ("&",     "&amp;" )           -- it is necessary that &amp is at the end!
    ] 
--}

generateSafeHTML :: Few -> T.Text
generateSafeHTML few = 
    let html = generateHTML few
    in  foldl' (flip ($)) html $ map (\(a, b) -> T.replace a b) htmlReplacements

scripts :: T.Text
scripts = T.concat 
    [ "<script type=\"text/javascript\" async src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML\"></script>"
    , "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/solarized-light.min.css\">"
    , "<script type=\"text/javascript\" src = \"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js\"></script>"
    , "<script>hljs.initHighlightingOnLoad();</script>"
    ]
finishedHTML :: Maybe FilePath -> T.Text -> T.Text
finishedHTML stylesheet html
    = T.concat 
    [ "<!DOCTYPE html>"
    , style
    , scripts
    , html]
    where
        style = case stylesheet of 
                    Nothing     -> ""
                    Just href   -> T.concat 
                                   [ "<link rel=\"stylesheet\" href=\""
                                   , T.pack href 
                                   , "\" type=\"text/css\" />"]

fewToHTML :: Maybe FilePath -> Few -> T.Text
fewToHTML css = finishedHTML css . generateSafeHTML

writeFewAsHTML :: Maybe FilePath -> Few -> FilePath -> IO ()
writeFewAsHTML css few output = do
    let html = fewToHTML css few
    TIO.writeFile output html

convertFewFileToHTML :: Maybe FilePath -> FilePath -> FilePath -> IO ()
convertFewFileToHTML css input output = do
    few <- parseFile input
    writeFewAsHTML css few output

