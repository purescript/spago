module Options.Applicative.Help.Pretty
  ( module Text.PrettyPrint.Leijen
  , (.$.)
  ) where

import Text.PrettyPrint.Leijen hiding ((<$>),columns)
import Text.PrettyPrint.Leijen as PP



infixr 5 PP.appendWithLine as .$.
