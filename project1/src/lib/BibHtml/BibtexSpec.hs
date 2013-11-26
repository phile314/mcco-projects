module BibHtml.BibtexSpec where

import BibHtml.Spec
import Bibtex
import Data.Map hiding (map)
import qualified Data.Map as M

-- local abbrevations
e = sExactly
o = sOptional . sExactly
--
-- | Either an Author or Editor
sAE :: SpecTree
sAE = Author <|> Editor



-- | Returns the specification tree for a given entry type. All entry types specified
--   in http://www.andy-roberts.net/res/writing/latex/bibentries.pdf are implemented as of 24.11.2013.
--   The order of the fields indicates the order in which they are to be printed. 
--   REMARK: Please note that the order is incorrect for many entry types, as this information
--   is hard to get by. To rectify the wrong ordering, the fields can just be moved to the proper location below.
spec :: Type -> SpecTree
spec Article    = sAllOf [sAE, e Title, e Journal, e Year, o Volume, o Number, o Pages, o Month, o Note]
spec Book       = sAllOf [sAE, e Title, e Publisher, e Year, sOptional (Volume <|> Number), o Series, o Address, o Edition, o Month, o Note]
spec Booklet    = sAllOf [e Title, o Author, o HowPublished, o Address, o Month, o Year, o Note]
spec Conference = spec Inproceedings
spec Inbook     = sAllOf [sAE, e Title, sSomeOf (e Chapter) (e Pages), e Publisher, e Year, o Volume, sOptional (Volume <|> Number), o Series, o Type, o Address, o Edition, o Month, o Note]
spec Incollection  = sAllOf [e Author, e Title, e Booktitle, e Year, o Editor, sOptional (Volume <|> Number), o Series, o Pages, o Address, o Month, o Organization, o Publisher, o Note]
spec Inproceedings = sAllOf [e Author, e Title, o Editor, e Booktitle, e Year, sOptional (Volume <|> Number), o Series, o Pages, o Address, o Month, o Organization, o Publisher, o Note]
spec Manual     = sAllOf [e Title, o Author, o Organization, o Address, o Edition, o Month, o Year, o Note]
spec Mastersthesis = sAllOf [e Author, e Title, e School, e Year, o Type, o Address, o Month, o Note]
spec Misc       = sAllOf [o Author, o Title, o HowPublished, o Month, o Year, o Note]
spec Phdthesis  = sAllOf [e Author, e Title, e School, e Year, o Type, o Address, o Month, o Note]
spec Proceedings = sAllOf [e Title, e Year, o Editor, sOptional (Volume <|> Number), o Series, o Address, o Month, o Organization, o Publisher, o Note]
spec Techreport = sAllOf [e Author, e Title, e Institution, e Year, o Type, o Address, o Month, o Note]
spec Unpublished = sAllOf [e Author, e Title, e Note, o Month, o Year]

