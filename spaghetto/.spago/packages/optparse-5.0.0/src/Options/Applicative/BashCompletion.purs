-- | You don't need to import this module to enable bash completion.
-- |
-- | See [the wiki](http://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion)
-- | for more information on bash completion.
module Options.Applicative.BashCompletion
  ( bashCompletionParser
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Options.Applicative.Builder (flag', int, internal, long, option, strOption, value)
import Options.Applicative.Common (mapParser, runParserInfo, showOption)
import Options.Applicative.Help.Chunk (Chunk(..))
import Options.Applicative.Help.Pretty (Doc, displayS, renderPretty)
import Options.Applicative.Internal (runCompletion)
import Options.Applicative.Internal.Utils (lines, startsWith, unLines)
import Options.Applicative.Types (ArgPolicy(..), CReader(..), Completer(..), CompletionResult(..), OptHelpInfo(..), OptName, OptReader(..), Option(..), Parser, ParserInfo(..), ParserPrefs, SomeParser(..), many, optHelp)

-- | Provide basic or rich command completions
data Richness
  = Standard
  -- ^ Add no help descriptions to the completions
  | Enriched Int Int
  -- ^ Include tab separated description for options
  --   and commands when available.
  --   Takes option description length and command
  --   description length.

derive instance richnessEq :: Eq Richness
derive instance richnessOrd :: Ord Richness
derive instance richnessGeneric :: Generic Richness _
instance richnessShow :: Show Richness where show = genericShow

bashCompletionParser :: forall a. ParserInfo a -> ParserPrefs -> Parser CompletionResult
bashCompletionParser pinfo pprefs = complParser
  where
    failure opts = CompletionResult
      { execCompletion: \progn -> unLines <$> opts progn }

    complParser =
      ( failure <$>
        (  bashCompletionQuery pinfo pprefs
        -- To get rich completions, one just needs the first
        -- command. To customise the lengths, use either of
        -- the `desc-length` options.
        -- zsh commands can go on a single line, so they might
        -- want to be longer.
        <$> ( flag' Enriched (long "bash-completion-enriched" `append` internal)
                <*> option int (long "bash-completion-option-desc-length" `append` internal `append` value 40)
                <*> option int (long "bash-completion-command-desc-length" `append` internal `append` value 40)
          <|> pure Standard
          )
        <*> (map Array.fromFoldable <<< many <<< strOption) (long "bash-completion-word"
                                  `append` internal)
        <*> option int (long "bash-completion-index" `append` internal) )
      ) <|> failure <$>
          (bashCompletionScript <$>
            strOption (long "bash-completion-script" `append` internal))
        <|> failure <$>
          (fishCompletionScript <$>
            strOption (long "fish-completion-script" `append` internal))
        <|> failure <$>
          (zshCompletionScript <$>
            strOption (long "zsh-completion-script" `append` internal))


bashCompletionQuery :: forall a. ParserInfo a -> ParserPrefs -> Richness -> Array String -> Int -> String -> Effect (Array String)
bashCompletionQuery pinfo pprefs richness ws i _ = case runCompletion compl pprefs of
  Just (Left (Tuple (SomeParser e) a))
    -> runExists (\p -> list_options a p) e
  Just (Right c)
    -> run_completer c
  Nothing
    -> pure []
  where
    { init:ws', rest:ws'' } = arraySplitAt i ws

    compl = runParserInfo pinfo (List.fromFoldable $ Array.drop 1 ws')

    list_options :: forall x. ArgPolicy -> Parser x -> Effect (Array String)
    list_options a
      = map fold
      <<< sequence
      <<< mapParser (opt_completions a)

    --
    -- Prior to 0.14 there was a subtle bug which would
    -- mean that completions from positional arguments
    -- further into the parse would be shown.
    --
    -- We therefore now check to see that
    -- hinfoUnreachableArgs is off before running the
    -- completion for position arguments.
    --
    -- For options and flags, ensure that the user
    -- hasn't disabled them with `--`.
    opt_completions :: ∀ x. ArgPolicy -> OptHelpInfo -> Option x -> Effect (Array String)
    opt_completions argPolicy hinfo opt = case (un Option opt).optMain of
      OptReader ns _ _
         | argPolicy /= AllPositionals
        -> pure <<< add_opt_help opt $ show_names ns
         | otherwise
        -> pure []
      FlagReader ns _
         | argPolicy /= AllPositionals
        -> pure <<< add_opt_help opt $ show_names ns
         | otherwise
        -> pure []
      ArgReader rdr
         | (un OptHelpInfo hinfo).hinfoUnreachableArgs
        -> pure []
         | otherwise
        -> run_completer (un CReader rdr).crCompleter
      CmdReader _ ns p
         | (un OptHelpInfo hinfo).hinfoUnreachableArgs
        -> pure []
         | otherwise
        -> pure <<< add_cmd_help p $ filter_names ns

    -- When doing enriched completions, add any help specified
    -- to the completion variables (tab separated).
    add_opt_help :: forall x f. Functor f => Option x -> f String -> f String
    add_opt_help opt = case richness of
      Standard
        -> identity
      Enriched len _
        -> map (\o -> let h = un Chunk $ optHelp opt
                       in  maybe o (\h' -> o <> "\t" <> render_line len h') h)

    -- When doing enriched completions, add the command description
    -- to the completion variables (tab separated).
    add_cmd_help :: forall f x. Functor f => (String -> Maybe (ParserInfo x)) -> f String -> f String
    add_cmd_help p = case richness of
      Standard
        -> identity
      Enriched _ len
        -> map (\cmd -> let h = p cmd >>= un Chunk <<< _.infoProgDesc <<< un ParserInfo
                         in  maybe cmd (\h' -> cmd <> "\t" <> render_line len h') h)

    show_names :: Array OptName -> Array String
    show_names = filter_names <<< map showOption

    -- We only want to show a single line in the completion results description.
    -- If there was a line break, it would come across as a different completion
    -- possibility.
    render_line :: Int -> Doc -> String
    render_line len doc = case map NEA.uncons $ NEA.fromArray $ lines (displayS (renderPretty 1.0 len doc)) of
      Nothing -> ""
      Just {head:x, tail: []} -> x
      Just {head:x, tail: _} -> x <> "..."

    filter_names :: (Array String) -> Array String
    filter_names = Array.filter is_completion

    run_completer :: Completer -> Effect (Array String)
    run_completer c = un Completer c (fromMaybe "" (Array.head ws''))

    is_completion :: String -> Boolean
    is_completion =
      case Array.head ws'' of
        Just w -> startsWith $ String.Pattern w
        Nothing -> const true


arraySplitAt
  :: forall a
   . Int
  -> Array a
  -> { init :: Array a, rest :: Array a }
arraySplitAt idx arr =
  case idx of
    0 ->
      { init: [], rest: arr }
    i ->
      { init: Array.slice 0 i arr, rest: Array.slice i (Array.length arr) arr }

bashCompletionScript :: String -> String -> Effect (Array String)
bashCompletionScript prog progn = pure
  [ "_" <> progn <> "()"
  , "{"
  , "    local CMDLINE"
  , "    local IFS=$'\\n'"
  , "    CMDLINE=(--bash-completion-index $COMP_CWORD)"
  , ""
  , "    for arg in ${COMP_WORDS[@]}; do"
  , "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)"
  , "    done"
  , ""
  , "    COMPREPLY=( $(" <> prog <> " \"${CMDLINE[@]}\") )"
  , "}"
  , ""
  , "complete -o filenames -F _" <> progn <> " " <> progn ]

-- |
-- | **Note**: Fish Shell
-- |
-- | Derived from Drezil's post in #169.
-- |
-- | ```
-- | commandline
-- | -c or --cut-at-cursor only print selection up until the current cursor position
-- | -o or --tokenize tokenize the selection and print one string-type token per line
-- | ```
-- |
-- | We tokenize so that the call to count (and hence --bash-completion-index)
-- | gets the right number use cut-at-curstor to not bother sending anything
-- | after the cursor position, which allows for completion of the middle of
-- | words.
-- |
-- | Tab characters separate items from descriptions.
fishCompletionScript :: String -> String -> Effect (Array String)
fishCompletionScript prog progn = pure
  [ " function _" <> progn
  , "    set -l cl (commandline --tokenize --current-process)"
  , "    # Hack around fish issue #3934"
  , "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)"
  , "    set -l cn (count $cn)"
  , "    set -l tmpline --bash-completion-enriched --bash-completion-index $cn"
  , "    for arg in $cl"
  , "      set tmpline $tmpline --bash-completion-word $arg"
  , "    end"
  , "    for opt in (" <> prog <> " $tmpline)"
  , "      if test -d $opt"
  , "        echo -E \"$opt/\""
  , "      else"
  , "        echo -E \"$opt\""
  , "      end"
  , "    end"
  , "end"
  , ""
  , "complete --no-files --command " <> progn <> " --arguments '(_"  <> progn <>  ")'"
  ]

zshCompletionScript :: String -> String -> Effect (Array String)
zshCompletionScript prog progn = pure
  [ "#compdef " <> progn
  , ""
  , "local request"
  , "local completions"
  , "local word"
  , "local index=$((CURRENT - 1))"
  , ""
  , "request=(--bash-completion-enriched --bash-completion-index $index)"
  , "for arg in ${words[@]}; do"
  , "  request=(${request[@]} --bash-completion-word $arg)"
  , "done"
  , ""
  , "IFS=$'\\n' completions=($( " <> prog <> " \"${request[@]}\" ))"
  , ""
  , "for word in $completions; do"
  , "  local -a parts"
  , ""
  , "  # Split the line at a tab if there is one."
  , "  IFS=$'\\t' parts=($( echo $word ))"
  , ""
  , "  if [[ -n $parts[2] ]]; then"
  , "     if [[ $word[1] == \"-\" ]]; then"
  , "       local desc=(\"$parts[1] ($parts[2])\")"
  , "       compadd -d desc -- $parts[1]"
  , "     else"
  , "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))"
  , "       compadd -l -d desc -- $parts[1]"
  , "     fi"
  , "  else"
  , "    compadd -f -- $word"
  , "  fi"
  , "done"
  ]
