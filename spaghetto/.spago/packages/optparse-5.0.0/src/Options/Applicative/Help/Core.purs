module Options.Applicative.Help.Core
  ( cmdDesc
  , briefDesc
  , missingDesc
  , fold_tree
  , fullDesc
  , errorHelp
  , headerHelp
  , suggestionsHelp
  , usageHelp
  , bodyHelp
  , footerHelp
  , parserHelp
  , parserUsage
  , module Reexport
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (foldr)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function (on)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (class Newtype, over, un)
import Data.Tuple (Tuple(..), fst, snd)
import Options.Applicative.Common (mapParser, optionNames, showOption, treeMapParser)
import Options.Applicative.Help.Chunk (Chunk(..), chunked, extractChunk, isEmpty, listToChunk, stringChunk, tabulate, vcatChunks, vsepChunks, (<<+>>), (<</>>))
import Options.Applicative.Help.Pretty (Doc, align, brackets, char, hsep, parens, string, (.$.), (<+>), (</>))
import Options.Applicative.Types (OptHelpInfo(..), OptReader(..), OptTree(..), OptVisibility(..), Option(..), Parser, ParserHelp(..), ParserInfo(..), ParserPrefs(..), optDescMod, optHelp, optMetaVar, optShowDefault, optVisibility)
import Options.Applicative.Types (ParserHelp(..)) as Reexport

-- | Style for rendering an option.
newtype OptDescStyle = OptDescStyle
  { descSep :: Doc
  , descHidden :: Boolean
  , descOptional :: Boolean
  , descSurround :: Boolean }

derive instance newtypeOptDescStyle :: Newtype OptDescStyle _

-- | Generate description for a single option.
optDesc :: forall a. ParserPrefs -> OptDescStyle -> OptHelpInfo -> Option a -> Chunk Doc
optDesc pprefs style info opt =
  let ns = optionNames (un Option opt).optMain
      mv = stringChunk $ optMetaVar opt
      descs = map (string <<< showOption) (Array.sort ns)
      desc' = listToChunk (intersperse (un OptDescStyle style).descSep descs) <<+>> mv
      show_opt
        | (un OptHelpInfo info).hinfoDefault && not (un OptDescStyle style).descOptional
        = false
        | optVisibility opt == Hidden
        = (un OptDescStyle style).descHidden
        | otherwise
        = optVisibility opt == Visible
      suffix
        | (un OptHelpInfo info).hinfoMulti
        = stringChunk $ (un ParserPrefs pprefs).prefMultiSuffix
        | otherwise
        = mempty
      render chunk
        | not show_opt
        = mempty
        | isEmpty chunk || not (un OptDescStyle style).descSurround
        = append chunk suffix
        | (un OptHelpInfo info).hinfoDefault
        = append (map brackets chunk) suffix
        | Array.null (Array.drop 1 descs)
        = append chunk suffix
        | otherwise
        = append (map parens chunk) suffix
  in maybe identity map (optDescMod opt) (render desc')



intersperse :: forall a. a -> Array a -> Array a
intersperse sep = Array.fold <<< Array.mapWithIndex \idx e -> if idx == 0 then [e] else [sep, e]


-- | Generate descriptions for commands.
cmdDesc :: forall a. Parser a -> Array (Tuple (Maybe String) (Chunk Doc))
cmdDesc = mapParser desc
  where
    desc :: forall x. OptHelpInfo -> Option x -> Tuple (Maybe String) (Chunk Doc)
    desc _ opt =
      case (un Option opt).optMain of
        CmdReader gn cmds p -> Tuple gn $
          tabulate do
            cmd <- Array.reverse cmds
            d <- maybe [] pure <<< map (un ParserInfo >>> _.infoProgDesc) $ p cmd
            pure $ Tuple (string cmd) (align (extractChunk d))
        _ -> mempty

-- | Generate a brief help text for a parser.
briefDesc :: forall a. ParserPrefs -> Parser a -> Chunk Doc
briefDesc = briefDesc' true

-- | Generate a brief help text for a parser, only including mandatory
--   options and arguments.
missingDesc :: forall a. ParserPrefs -> Parser a -> Chunk Doc
missingDesc = briefDesc' false

-- | Generate a brief help text for a parser, allowing the specification
--   of if optional arguments are show.
briefDesc' :: forall a. Boolean -> ParserPrefs -> Parser a -> Chunk Doc
briefDesc' showOptional pprefs = fold_tree <<< treeMapParser (optDesc pprefs style)
  where
    style = OptDescStyle
      { descSep: string "|"
      , descHidden: false
      , descOptional: showOptional
      , descSurround: true }

fold_tree :: OptTree (Chunk Doc) -> Chunk Doc
fold_tree (Leaf x) = x
fold_tree (MultNode xs) = foldr ((<</>>) <<< fold_tree) mempty xs
fold_tree (AltNode xs) = alt_node
                       <<< Array.filter (not <<< isEmpty)
                       <<< map fold_tree $ xs
  where
    alt_node :: Array (Chunk Doc) -> Chunk Doc
    alt_node [n] = n
    alt_node ns = map parens
                <<< foldr (chunked (\x y -> x </> char '|' </> y)) mempty
                $ ns

-- | Generate a full help text for a parser.
fullDesc :: forall a. ParserPrefs -> Parser a -> Chunk Doc
fullDesc pprefs = tabulate <<< Array.catMaybes <<< mapParser doc
  where
    doc :: forall x. OptHelpInfo -> Option x -> Maybe (Tuple Doc Doc)
    doc info opt = do
      guard $ not $ isEmpty n
      guard $ not $ isEmpty h
      pure $ Tuple (extractChunk n) (align $ extractChunk $ h <<+>> hdef)
      where
        n = optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk $ map show_def $ optShowDefault opt
        show_def s = parens (string "default:" <+> string s)
    style = OptDescStyle
      { descSep: string ","
      , descHidden: true
      , descOptional: true
      , descSurround: false }

errorHelp :: Chunk Doc -> ParserHelp
errorHelp chunk = over ParserHelp _{ helpError = chunk } mempty

headerHelp :: Chunk Doc -> ParserHelp
headerHelp chunk = over ParserHelp _{ helpHeader = chunk } mempty

suggestionsHelp :: Chunk Doc -> ParserHelp
suggestionsHelp chunk = over ParserHelp _{ helpSuggestions = chunk } mempty

usageHelp :: Chunk Doc -> ParserHelp
usageHelp chunk = over ParserHelp _{ helpUsage = chunk } mempty

bodyHelp :: Chunk Doc -> ParserHelp
bodyHelp chunk = over ParserHelp _{ helpBody = chunk } mempty

footerHelp :: Chunk Doc -> ParserHelp
footerHelp chunk = over ParserHelp _{ helpFooter = chunk } mempty

-- | Generate the help text for a program.
parserHelp :: forall a. ParserPrefs -> Parser a -> ParserHelp
parserHelp pprefs p = bodyHelp <<< vsepChunks $
  [ with_title "Available options:" (fullDesc pprefs p) ] <> (group_title <$> cs)
  where
    def = "Available commands:"

    cs :: Array (NonEmptyArray (Tuple (Maybe String) (Chunk Doc)))
    cs = Array.groupBy ((==) `on` fst) $ cmdDesc p

    group_title arr =
      let {head, tail} = (NEA.uncons arr)
      in with_title (fromMaybe def $ fst head) $ vcatChunks ([snd head] <> (snd <$> tail))


    with_title :: String -> Chunk Doc -> Chunk Doc
    with_title title = map (string title .$. _)

-- | Generate option summary.
parserUsage :: forall a. ParserPrefs -> Parser a -> String -> Doc
parserUsage pprefs p progn = hsep
  [ string "Usage:"
  , string progn
  , align (extractChunk (briefDesc pprefs p)) ]

{-# ANN footerHelp "HLint: ignore Eta reduce" #-}
