{-# LANGUAGE OverloadedStrings #-}

module Few.Types
    ( Few (..)
    , NodeType (..)
    , NodeAttr (..)
    , Opening (..)
    , Ending (..)
    , AttrName (..)
    , Variable (..)
    ) where
 
----------------------------------------


----------------------------------------

type Opening = String
type Ending  = String

type Variable = (String, String)

data NodeType 
    = FewContent
    | Bold          
    | Italic
    | Underline
    | Strikethrough
    | Paragraph
    | Header1
    | Header2
    | Header3
    | Header4
    | Header5
    | Header6
    | Code
    | OrderedList
    | UnorderedList
    | ListItem
    | Quote
    | HTML
    | Break
    | Link
    | Math
    | Escaped

    deriving (Eq, Ord, Show, Read)

newtype NodeAttr = NodeAttr (String, String)
                    deriving (Eq, Ord, Show, Read)                

type AttrName = String

data Few 
    = FewNode 
        { node          :: NodeType  
        , attributes    :: [NodeAttr]
        , children      :: [Few]
        } 
    | PlainText String

    deriving (Eq, Ord, Show, Read)           
