-- | This module contains the specifications about which fields are
-- required or optional for each entry 'Type' and the order in which they
-- should be printed out.

module BibHtml.BibtexSpec (spec) where

import BibHtml.Spec
import Bibtex
import Data.Map hiding (map)
import qualified Data.Map as M

-- local abbrevations
e = exactly
o = optional . exactly

-- | Either an 'Author' or 'Editor'
sAE :: SpecTree
sAE = Author <|> Editor

-- | Optionally either exactly 'Volume' or 'Number'
sVN :: SpecTree
sVN = optional (Volume <|> Number)

-- | Returns the specification tree for a given entry type. All entry types specified
--   in http://www.andy-roberts.net/res/writing/latex/bibentries.pdf are implemented as of 24.11.2013.
--   The order of the fields indicates the order in which they are to be printed. 
--   REMARK: Please note that the order is incorrect for many entry types, as this information
--   is hard to get by. To rectify the wrong ordering, the fields can just be moved to the proper location below.
spec :: Type -> SpecTree
spec Article = allOf [sAE, e Title, e Journal, e Year, o Volume, o Number, o Pages, o Month, o Note]
spec Book = allOf [sAE, e Title, e Publisher, e Year, sVN, o Series, o Address, o Edition, o Month, o Note]
spec Booklet = allOf [e Title, o Author, o HowPublished, o Address, o Month, o Year, o Note]
spec Conference = spec Inproceedings
spec Inbook = allOf [sAE, e Title, someOf (e Chapter) (e Pages), e Publisher, e Year, o Volume, sVN, o Series, o Type, o Address, o Edition, o Month, o Note]
spec Incollection = allOf [e Author, e Title, e Booktitle, e Year, o Editor, sVN, o Series, o Pages, o Address, o Month, o Organization, o Publisher, o Note]
spec Inproceedings = allOf [e Author, e Title, o Editor, e Booktitle, e Year, sVN, o Series, o Pages, o Address, o Month, o Organization, o Publisher, o Note]
spec Manual = allOf [e Title, o Author, o Organization, o Address, o Edition, o Month, o Year, o Note]
spec Mastersthesis = allOf [e Author, e Title, e School, e Year, o Type, o Address, o Month, o Note]
spec Misc = allOf [o Author, o Title, o HowPublished, o Month, o Year, o Note]
spec Phdthesis = allOf [e Author, e Title, e School, e Year, o Type, o Address, o Month, o Note]
spec Proceedings = allOf [e Title, e Year, o Editor, sVN, o Series, o Address, o Month, o Organization, o Publisher, o Note]
spec Techreport = allOf [e Author, e Title, e Institution, e Year, o Type, o Address, o Month, o Note]
spec Unpublished = allOf [e Author, e Title, e Note, o Month, o Year]
