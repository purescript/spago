-- | This is an empty module which re-exports
-- | the help text system for optparse.
module Options.Applicative.Help
  -- | Pretty printer. Reexports most combinators
  --   from Text.PrettyPrint.ANSI.Leijen
  ( module Options.Applicative.Help.Pretty

  -- | A free monoid over Doc with helpers for
  --   composing help text components.
  , module Options.Applicative.Help.Chunk

  -- | Types required by the help system.
  , module Options.Applicative.Help.Types

  -- | Core implementation of the help text
  --   generator.
  , module Options.Applicative.Help.Core

  -- | Edit distance calculations for suggestions
  , module Options.Applicative.Help.Levenshtein
  ) where

import Options.Applicative.Help.Chunk (Chunk(..), chunkBeside, chunkBesideOrBelow, chunked, extractChunk, isEmpty, listToChunk, paragraph, stringChunk, tabulate, vcatChunks, vsepChunks, (<<+>>), (<</>>))
import Options.Applicative.Help.Core (ParserHelp(..), bodyHelp, briefDesc, cmdDesc, errorHelp, fold_tree, footerHelp, fullDesc, headerHelp, missingDesc, parserHelp, parserUsage, suggestionsHelp, usageHelp)
import Options.Applicative.Help.Levenshtein (editDistance)
import Options.Applicative.Help.Pretty (Doc(..), Docs(..), SimpleDoc(..), align, angles, appendWithLine, appendWithLinebreak, appendWithSoftbreak, appendWithSoftline, appendWithSpace, backslash, beside, bool, braces, brackets, cat, char, colon, column, comma, displayS, dot, dquote, dquotes, empty, enclose, encloseSep, equals, fill, fillBreak, fillCat, fillSep, fits1, fitsR, flatAlt, flatten, group, hang, hardline, hcat, hsep, indent, indentation, int, langle, lbrace, lbracket, line, linebreak, list, lparen, nest, nesting, number, parens, punctuate, rangle, rbrace, rbracket, renderCompact, renderFits, renderPretty, renderSmart, rparen, semi, semiBraces, sep, softbreak, softline, space, spaces, squote, squotes, string, text, tupled, vcat, vsep, width, (.$.), (<$$>), (<+>), (<//>), (</>))
import Options.Applicative.Help.Types (ParserHelp(..), renderHelp)
