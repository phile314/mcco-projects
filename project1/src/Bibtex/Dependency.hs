-- | This module defines the dependencies between entry 'Type's and 'Field's,
-- More precisely which fields are required and which optional for each entry
-- type. Note that for simplicity we are not going to deal with mutually
-- exlusive fields.

module Bibtex.Dependency where

import Bibtex.Basic
import qualified Data.Map as M (Map, fromList)

data Dependency = D { required :: [Field], optional :: [Field] }

dependency :: M.Map Type Dependency
dependency = M.fromList $ [
  (Article,       D [Author, Title, Journal, Year] 
                    [Volume, Number, Pages, Month, Note]),
  (Book,          D [Author, Editor, Title, Publisher, Year]
                    [Volume, Number, Series, Address, Edition, Month, Note]),
  (Booklet,       D [Title] [Author, HowPublished, Address, Month, Year, Note]),
  (Conference,    D [Author, Title, Booktitle, Year]
                    [Editor, Volume , Number, Series, Pages, Address,
                     Month, Organization, Publisher, Note]),
  (Inbook,        D [Author , Editor, Title, Chapter, Pages, Publisher, Year]
                    [Volume , Number, Series, Type, Address, Edition, Month, Note]),
  (Incollection,  D [Author, Title, Booktitle, Publisher, Year] 
                    [Editor, Volume , Number, Series, Type, Chapter, Pages, 
                     Address, Edition, Month, Note]),
  (Inproceedings, D [Author, Title, Booktitle, Year] 
                    [Editor, Volume , Number, Series, Pages, Address, Month,
                     Organization, Publisher, Note]),
  (Manual,        D [Title] [Author, Organization, Address, Edition, Month, Year, Note]),
  (Mastersthesis, D [Author, Title, School, Year] [ Type, Address, Month, Note]),
  (Misc,          D [] [Author, Title, HowPublished, Month, Year, Note]),
  (Phdthesis,     D [Author, Title, School, Year] [Type, Address, Month, Note]),
  (Proceedings,   D [Title, Year]
                    [Editor, Volume , Number, Series, Address, Month,
                     Organization, Publisher, Note]),
  (Techreport,    D [Author, Title, Institution, Year]
                    [Type, Address, Month, Note]),
  (Unpublished,   D [Author, Title, Note] [Month, Year])]
