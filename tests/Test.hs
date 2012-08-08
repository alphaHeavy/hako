{-# LANGUAGE QuasiQuotes #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Text.Hako
import Text.Hako.Html

prop_textOnly = [hako|asdf|] == Html "asdf"
prop_htmlEntities = [hako|<>&"'"|] == Html "&lt;&gt;&amp;&quot;&apo;&quot;"
prop_htmlEntitiesSane s =
    let r = [hako|{s}|]
        isSpecialChar c = c `elem` "<>'\""
    in not . any isSpecialChar $ fromHtml r
prop_stringInterpolation s = [hako|{Html s}|] == Html s
prop_let s = [hako|{def x ={Html s}}{s}|] == Html s

main = do
        quickCheck prop_textOnly
        quickCheck prop_htmlEntities
        quickCheck (prop_stringInterpolation :: String -> Bool)
        quickCheck (prop_htmlEntitiesSane :: String -> Bool)
        quickCheck (prop_let :: String -> Bool)
