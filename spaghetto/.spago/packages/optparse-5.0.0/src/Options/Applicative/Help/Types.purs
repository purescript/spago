module Options.Applicative.Help.Types (
    ParserHelp (..)
  , renderHelp
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Options.Applicative.Help.Chunk (Chunk, extractChunk, vsepChunks)
import Options.Applicative.Help.Pretty (Doc, displayS, renderPretty)

newtype ParserHelp = ParserHelp
  { helpError :: Chunk Doc
  , helpSuggestions :: Chunk Doc
  , helpHeader :: Chunk Doc
  , helpUsage :: Chunk Doc
  , helpBody :: Chunk Doc
  , helpFooter :: Chunk Doc
  }
derive instance newtypeParserHelp :: Newtype ParserHelp _
instance parserHelpShow :: Show ParserHelp where
  show = show <<< renderHelp 80
derive newtype instance parserHelpSemigroup :: Semigroup ParserHelp
derive newtype instance parserHelpMonoid :: Monoid ParserHelp

helpText :: ParserHelp -> Doc
helpText (ParserHelp h) = extractChunk $ vsepChunks
  [ h.helpError
  , h.helpSuggestions
  , h.helpHeader
  , h.helpUsage
  , h.helpBody
  , h.helpFooter
  ]

-- | Convert a help text to 'String'.
renderHelp :: Int -> ParserHelp -> String
renderHelp cols
  = displayS
  <<< renderPretty 1.0 cols
  <<< helpText
