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

-- | Converts a `HtmlDoc` to a `Doc`.
instance Printable HtmlDoc where
    pp (HtmlDoc t) = text "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" >-< text "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" >-< pp t

-- | Converts an 'HtmlTree' to a `Doc`. Special characters in text and xml attribute values are escaped properly
--   by this function.
instance Printable HtmlTree where
    pp = ppNode False

-- | Converts a list of html tree to a `Doc`.
instance Printable [HtmlTree] where
    pp = ppNodes False

-- | Converts an attribute association list to a `Doc`. Please note that
--   the result of passing invalid xml attribute names is not defined.
instance Printable [(AttrName, AttrVal)] where
    pp [] = empty
    pp ((an, av):as) = besides [space, text an, text "=", value, pp as]
      -- it seems that the only invalid character in an attribute value is the quote character
      where value = quoted $ escaped (=='\"') av


-- | Converts a `Node` to a `Doc`. If the first parameter is true,
--   the result will be a one-line document.
ppNode _ (Text s) = escapedAll s
ppNode _ (Elem t attrs []) = closedTag $ text t >|< pp attrs
    where closedTag = enclose langle (text "/" >|< rangle)

ppNode fOneLine (Elem t attrs es) =
    if fOneLine then oneLine else oneLine >//< multiLine
    where oneLine = tagOpen >|< content True >|< tagClosed
          multiLine = tagOpen >-< content False >-< tagClosed
          tagOpen = angles (text t >|< pp attrs)
          tagClosed = enclose (langle >|< text "/") rangle (text t)
          content o = indent ind (ppNodes o es)

-- | Converts a list of nodes to a `Doc`. If the first paramter is true,
--   the result will be a one-line document.
ppNodes fOneLine es = pp' empty es
    where pp' ac (e@(Text _):es)   = pp' (ac >|< ppNode True e) es
          pp' ac (e:es) | fOneLine = pp' (ac >|< ppNode True e) es
          pp' ac (e:es)            = pp' (ac >-< ppNode False e) es
          pp' ac [] = ac

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
