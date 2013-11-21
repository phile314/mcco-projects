module Bib2Html where

import HtmlTree
import Bibtex

class Html a where
    toHtml :: a -> Node


