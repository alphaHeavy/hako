{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
-- | Implements HTML-encoding for Hako.
-- The 'Html' type and the 'ToHtml' typeclass together take care of 
-- html-encoding appropriately and automatically inside Hako templates.
module Text.Hako.Html
( htmlEncode
, Html (..)
, ToHtml
, toHtml
, fromHtml
, (<++>)
) where

-- | Basic HTML-encoding: converts all special HTML characters into the
-- corresponding entities.
htmlEncode :: String -> Html
htmlEncode =
    Html . (foldl (++) []) . (map encodeChar)
    where encodeChar c =
            case c of
                '&' -> "&amp;"
                '<' -> "&lt;"
                '>' -> "&gt;"
                '"' -> "&quot;"
                '\'' -> "&apos;"
                otherwise -> [c]

-- | All expressions interpolated into Hako templates using @{}@ syntax
-- must satisfy 'ToHtml'. Any member of 'Show' automatically has a default
-- implementation through 'show'; additionally, suitable implementations
-- are provided for 'String' (skipping the quoting and escaping which 'show' 
-- would otherwise introduce), as well as 'List's, 'Maybe's and 'Either's of 
-- 'ToHtmls'.
class ToHtml a where
    toHtml :: a -> Html

-- | A piece of HTML source. Use 'fromHtml' to get the HTML source back out.
data Html = Html String
    deriving (Show, Eq)

-- | Get HTML source as String
fromHtml :: Html -> String
fromHtml (Html a) = a

-- | 'Html' itself is also a member of 'ToHtml'; converting from 'Html' to 
-- 'Html' is an identity.
instance ToHtml Html where
    toHtml = id

-- | Strings have their own instance of 'ToHtml', which performs HTML-encoding
-- but skips the call to 'show' which would otherwise introduce undesirable
-- quotes and escaping.
instance ToHtml [Char] where
    toHtml = htmlEncode

-- | Implement an instance for 'Maybe', so that 'Nothing' is leniently 
-- converted to an empty string, and 'Just's are unpacked.
instance ToHtml a => ToHtml (Maybe a) where
    toHtml (Just a) = toHtml a
    toHtml Nothing = Html ""

-- | 'Either' should work also, as long as both branches are 'ToHtml'
-- themselves.
instance (ToHtml a, ToHtml b) => ToHtml (Either a b) where
    toHtml (Left a) = toHtml a
    toHtml (Right a) = toHtml a

-- | Lists are automatically folded using straightforward concatenation.
instance ToHtml a => ToHtml [a] where
    toHtml [] = Html ""
    toHtml xs = foldl1 (<++>) $ map toHtml xs

-- | All other types in 'Show' default to HTML-encoding their 'show'
-- representation.
instance Show a => ToHtml a where
    toHtml = htmlEncode . show

-- | Concatenate two 'Html's together.
-- The 'Html' equivalent to list concatenation (@++@)
(<++>) :: Html -> Html -> Html
(Html a) <++> (Html b) = Html (a ++ b)
