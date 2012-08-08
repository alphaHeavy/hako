{-# LANGUAGE QuasiQuotes #-}
-- | A simplist template language using a quasi-quoting approach.
-- Hako quasi-quote expressions return a 'Html', which can
-- be converted back to a 'String' using 'fromHtml'.
-- Basic syntax rules:
-- * Text is kept as-is, without any encoding performed.
--
-- >>> [hako|<p>foobar</p>|]
-- Html "<p>foobar</p>"
--
-- * Curly braces designate Haskell expressions, which must return an instance
--   of typeclass 'ToHtml'; the 'toHtml' method is called automatically.
--
-- >>> [hako|This is {"<a>quoted</a>"}|]
-- Html "This is &lt;a&gt;quoted&lt;/a&gt;"
--
-- * If the opening curly brace is immediately followed by the keyword def,
--   then the expression is interpreted as a definition instead; it works 
--   pretty much exactly like a function or constant definition in plain old
--   Haskell, and the definition thus created can be called as a function from
--   anywhere within the template.
--
-- >>> [hako|{def a x =<a>{x}</a>}Here's a {a "link"}.|]
-- Html "Here's a <a>link</a>."
--
-- * A @def@ block can be used before it is defined.
--
-- >>> [hako|Here's a {a "link"}.{def a x =<a>{x}</a>]|]
-- Html "Here's a <a>link</a>."
--
-- * As is to be expected from a quasi-quoter, the current scope is carried
--   into the template.
--
-- >>> let txt = "Hello, world!" in [hako|<div>{txt}</div>|]
-- Html "<div>Hello, world!</div>"
--
-- * Since @{}@ expressions can contain any valid Haskell expression (as long
--   as its type implements 'ToHtml'), you can use any Haskell function that is
--   currently in scope.
--
-- >>> [hako|{def li x=<li>x</li>}<ul>{map li [1,2,3]}</ul>|]
-- Html "<ul><li>1</li><li>2</li><li>3</li></ul>"
module Text.Hako
( hako
, hakof
, HH.fromHtml
) where

import Text.Hako.Parsing
import Text.Hako.Html
import qualified Text.Hako.Html as HH
import Language.Haskell.TH
import Language.Haskell.TH.Quote

hako :: QuasiQuoter
hako = QuasiQuoter { quoteExp = parseTemplateFromString
                   , quotePat = error "Hako does not implement pattern quoting"
                   , quoteType = error "Hako does not implement type quoting"
                   , quoteDec = error "Hako does not implement dec quoting"
                   }

hakof = quoteFile hako
