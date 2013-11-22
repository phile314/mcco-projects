{-# LANGUAGE FlexibleInstances #-}

-- | This module defines the 'Printable' instance for 'HtmlTree'.
-- It deals with escaping rules and indentaion.

module Html.PrettyPrinter ( pp ) where

import CCO.Printing
import Data.Maybe (fromMaybe)
import Html.Tree

-- | Indentation spacing.
ind :: Int
ind = 4

--TODO escaping

-- | Converts an 'HtmlTree' to a `Doc`. Special characters are escaped properly
--   by this function (TODO).
instance Printable HtmlTree where
    pp (Text s) = escapedAll s
    pp (Elem t [] attrs) = closedTag $ text t >|< pp attrs
      where closedTag = enclose langle (text "/" >|< rangle)
    pp (Elem t es attrs) = tagOpen >-< content >-< tagClosed
      where tagOpen = angles (text t >|< pp attrs)
            tagClosed = enclose (langle >|< text "/") rangle (text t)
            content = indent ind (pp es)

-- I'm not really sure what are valid characters in xml names, hence we do not escape them for the time being...
instance Printable [(AttrName, AttrVal)] where
    pp [] = empty
    pp ((an, av):as) = besides [space, text an, text "=", value, pp as]
      where value = quoted $ escaped (=='\"') av

instance Printable [Node] where
    pp es = foldl (\a c -> a >|< (pp c)) empty es

-- | Returns the given 'Doc' enclosed in quotes.
quoted :: Doc -> Doc
quoted d = enclose q q d
    where q = text "\""

-- | @'escaped' p s@ returns a 'Doc' representing @s@ escaped using the predicate @p@.
escaped :: (Char -> Bool) -> String -> Doc
escaped p = text . (escape p)

-- | Escape all characters for which xml entities are definied.
escapedAll :: String -> Doc
escapedAll = escaped (const True)

-- | Escape those characters for which the predicate returns true. 
-- If the predicate is false or there is no escaping mechanism defined for a 
-- character, the original character is retained.
escape :: (Char -> Bool) -> String -> String
escape p = concatMap escape'
  where escape' c | p c = fromMaybe [c] $ lookup c escapeRules
        escape' c = [c]

-- | This associative lists contains html escaping rules.
escapeRules :: [(Char, String)]
escapeRules = [('<', "&lt;"), ('>', "&gt;"), ('&', "&amp;"), ('\'',"&quot;")]
