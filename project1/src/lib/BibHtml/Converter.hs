-- | This module contains functions to convert from bibtex elements
-- to html representation.

module BibHtml.Converter (toHtml) where

import Html.Tree
import CCO.Feedback
import CCO.Printing
import Control.Monad (msum)
import Data.Char (isUpper, isSpace)
import Data.List (intersperse, intercalate)
import qualified Data.List as L (last)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Bibtex
import BibHtml.BibtexSpec
import BibHtml.Spec
import Prelude hiding (last)
import Text.Regex (mkRegex, splitRegex)

instance Html BibtexEntry where
  toHtml (Entry t _ xs) = Elem "span" [] cont
    where cont = (foldr merge [] xs) ++ [Text "."]
          merge e [] = [f2h e]
          merge e@(f,_) s = (f2h e):(Text (separatorOf t f ++ " ")):s
          f2h (f, s) = fieldToHtml t (f, replace "--" "—" s)

instance Html BibtexDb where
  toHtml (BibtexDb db) = Elem "html" [] [header, body]
    where header = "head" << "title" << Text "Bibliography"
          body = Elem "body" [] (toc ++ [line, table])
          toc  = summaryOf db
          line = Elem "hr" [] []
          table = tableOf db

-- | Converts an attribute to a html tree. The value of the field is trimmed.
fieldToHtml :: Type -> (Field, String) -> HtmlTree
fieldToHtml Inproceedings (Title, s) = Text (trim s)
fieldToHtml Inproceedings (Booktitle, s) = "em" << Text (trim s)
fieldToHtml _ (Title, s) = "em" << Text (trim s)
fieldToHtml _ (Editor, s) = Text ("In: " ++ trim s ++ ", editors")
fieldToHtml _ (Pages, s) = Text ("pages " ++ trim s)
fieldToHtml _ (Author, s) =
  case names of
    [n] -> Text n
    _ -> Text $ concat (intersperse ", " (init names)) ++ " and " ++ L.last names
  where names = map layout (retrieveNames s)
        layout n = unwords [first n, middle n, last n]
fieldToHtml _ (f, s) = Text s

-- | Retrieves the specific separator required for each 'Field'.
-- The default separator is comma.
separatorOf :: Type -> Field -> String
separatorOf _ Title = "."
separatorOf _ Pages = "."
separatorOf _ Author = "."
separatorOf _ _ = ","

-- | Produces a summary of the given bibtex entries.
-- More precisely it is an html object containing a list of 
-- anchors referring to the associated entry. 
summaryOf :: [BibtexEntry] -> [HtmlTree]
summaryOf db = intersperse (Text " | ") anchors
  where anchors = map anchor db

-- | Produces an html two columns table containing the given bibtex entries.
-- The first column contains the 'deriveKey's, whereas the second contains
-- the given html description of the correspondent bibtex entry.
tableOf :: [BibtexEntry] -> HtmlTree
tableOf db = Elem "table" [("border", "0")] $ map (row . process) db
  where row htmls = Elem "tr" [("valign", "top")] $ map ("td" <<) htmls
        process e = [reference e, toHtml e]

-- | Produces a reference html object, whose name is the key of the entry.
-- The name displayed will be the result of 'deriveKey' on the key entry.
reference :: BibtexEntry -> HtmlTree
reference e@(Entry _ k _) = Elem "a" [("name", k)] [Text key]
  where key = "[" ++ (deriveKey e) ++ "]"

-- | Produces an anchor html object that points to the entry key.
-- The visible name is the result of 'deriveKey' on the key entry.
anchor :: BibtexEntry -> HtmlTree
anchor e@(Entry _ k _) = Elem "a" [("href", '#':k)] [Text key]
  where key = "[" ++ (deriveKey e) ++ "]"

-- | Derives the key shown to the user when referring to an entry, eg "[LO98]".
-- If the author and the year fields are present in the entry the key is 
-- composed by the initials of the last name of the authors and the last two
-- digits of the year. Otherwise the first 4 characters of key are used.
deriveKey :: BibtexEntry -> String
deriveKey (Entry t k fs) = 
  case (lookup Author fs, lookup Year fs) of
    (Just a, Just y) -> initials a ++ last2 y
    _                -> take 4 k
  where initials a = concatMap ((take 1) . last) $ retrieveNames a
        last2 = reverse . take 2 . reverse

-- | Represents the complete name of an author.
data Name = Name { first :: String, middle :: String, last :: String}

-- | Retrieves first, middle and last names from a string that uses
-- latex conversion.
retrieveNames :: String -> [Name]
retrieveNames = map (getName . trim) . splitNames 
  where splitNames = splitOn "and"

-- | Retrieves a single author 'Name' from a string.
-- ('splitNames' is assumed to be already called).
-- Three names specifications are supported. If all fail
-- then the given string as last name is returned.
getName :: String -> Name 
getName s = fromMaybe (Name "" "" s) (msum [s1 s, s2 s])

-- | First specification strategy: von Last, First
s1 :: String -> Maybe Name
s1 s = 
  case splitRegex  (mkRegex ",") s of
  [left,first] -> 
    -- Simplification: von words are all lowercase
    let (middle, last) = span (not . isCapitalized) (words left)
        name = Name (trim first) (unwords middle) (unwords last) in
    if null last then Nothing else Just name
  _            -> Nothing

-- | Second specification strategy: First von Last
s2 :: String -> Maybe Name
s2 s = if null last then Nothing else Just name
  where name = Name (unwords first) (unwords middle) (unwords last)
        (first, left) = span isCapitalized (words s)
        (middle, last) = span (not . isCapitalized) left

-- | Checks whether a string starts with a capital letter.
-- The empty string is considered not capitalazied.
isCapitalized :: String -> Bool
isCapitalized [] = False
isCapitalized (c:_) = isUpper c

-- | @'replace' old new s@ replaces all the occurences of @old@ with @new@ in @s@.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

-- | Trims the withespace from the start and the end of a string
trim :: String -> String
trim = tr . tr
  where tr = reverse . dropWhile isSpace
