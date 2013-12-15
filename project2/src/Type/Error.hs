-- | This module exports data types and functions for dealing with errors.

module Type.Error (
    TypeError(..)
  , ScopeError(..)
  , executeError
  , compileError
  , scopeError
  ) where

import Type.Error.EType
import Type.Error.EScope
import Type.Error.Internal
