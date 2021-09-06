{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Spago.Config.AST
  ( ConfigModification(..)
  , ResolvedUnresolvedExpr(..)
  , modifyRawConfigExpression
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Data.Sequence         as Seq
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Parser          as Parser
import qualified Spago.Dhall           as Dhall
import qualified Data.List.NonEmpty    as NonEmpty


type Expr = Dhall.Expr Parser.Src Dhall.Import
type ResolvedExpr = Dhall.Expr Parser.Src Void

-- |
-- Indicates the change Spago wants to make to the Dhall expression
-- used to produce a 'Spago.Config.Config' value.
data ConfigModification
  = AddPackages ![PackageName]
  -- ^ Adds packages to the @dependencies@ field
  | AddSources ![Text]
  -- ^ Adds sources to the @sources@ field
  | SetName Text
  -- ^ Sets the @name@ field to the provided name

-- |
-- Indicates the change to make once inside the Dhall expression,
-- regardless of what value(s) it produces.
data AstUpdate
  = InsertListText (Seq Expr)
  | SetText Expr

-- |
-- Newtype over a Tuple that stores the same expression
-- in the version where its imports are resolved (i.e. @Expr Src Void@)
-- and the version where its imports are not resolved  (i.e. @Expr Src Import@).
newtype ResolvedUnresolvedExpr =
  ResolvedUnresolvedExpr { resolvedUnresolvedExpr :: (ResolvedExpr, Expr) }

-- |
--
-- Modifies a Dhall expression that can be parsed to produce a `Spago.Config.Config` value.
--
-- Such an expression is anything that, when normalized, produces a
-- \"record expression\" whose
-- * @dependencies@ key contains a \"list expression\" of text that
--   corresponds to package names.
-- * @name@ key corresponds to a text expression containing the name of the project
-- * @sources@ key contains a \"list expression\" of text that
--   corresponds to source globs.
--
-- Since the user may be requesting changes that result in a no-op
-- (e.g. add packages that have already been added),
-- we first determine if a change needs to be made,
-- and then attempt to make that change.
--
-- To accomplish this goal, we normalize the expression first. There are two reasons why.
-- First, it makes the implementation simpler. Rather than figuring out what the expression
-- currently contains (e.g. whether its @dependencies@ field has the packages we want to add)
-- while we are trying to make a change (e.g. adding new packages to the @dependencies@ field),
-- we break this into two steps:
-- 1. determining what change we actually need to make
-- (e.g. if we are trying to add packages, @foo@ and @bar@, to the @dependencies@ field
-- but @foo@ is already added, then we need to add only @bar@), and
-- 2. where to make the change in the AST.
--
-- Breaking this down into two problems makes the solving the second problem easier.
--
-- Second, by confirming below that the normalized expression found in the @spago.dhall@ file
-- IS a @RecordLit@ with the field we need to modify (e.g. it has a @dependencies@ field),
-- we can make some assumptions about the @Expr@ passed into `modifyRawDhallExpression`.
-- For example, we don't need to know what @Embed@ data constructor cases are because we can infer
-- based on where we are in the expression whether they are a Record expression that has
-- our desired field (e.g. the @dependencies@ field) or a List expression.
--
-- In other words, `modifyRawDhallExpression` can actually succeed for the cases we support.
modifyRawConfigExpression :: HasLogFunc env => ConfigModification -> ResolvedUnresolvedExpr -> RIO env Expr
modifyRawConfigExpression astMod ResolvedUnresolvedExpr { resolvedUnresolvedExpr = (normalizedExpr, originalExpr) } = case astMod of
  AddPackages newPackages -> do
    maybeAllInstalledPkgs <- findListTextValues dependenciesText PackageName
    case maybeAllInstalledPkgs of
      Nothing -> do
        pure originalExpr
      Just allInstalledPkgs -> do
        let pkgsToInstall = nubSeq $ Seq.filter (`notElem` allInstalledPkgs) $ Seq.fromList newPackages
        if null pkgsToInstall
        then do
          pure originalExpr
        else do
          modifyRawDhallExpression dependenciesText (InsertListText (Dhall.toTextLit . packageName <$> pkgsToInstall)) originalExpr
  AddSources newSources -> do
    maybeAllSources <- findListTextValues sourcesText id
    case maybeAllSources of
      Nothing -> do
        pure originalExpr
      Just allSources -> do
        let sourcesToInstall = nubSeq $ Seq.filter (`notElem` allSources) $ Seq.fromList newSources
        if null sourcesToInstall
        then do
          pure originalExpr
        else do
          modifyRawDhallExpression sourcesText (InsertListText (Dhall.toTextLit <$> sourcesToInstall)) originalExpr

  SetName newName -> do
    maybeName <- findTextValue nameText
    case maybeName of
      Nothing -> do
        pure originalExpr
      Just originalName
        | originalName == newName -> do
            pure originalExpr
        | otherwise -> do
            modifyRawDhallExpression nameText (SetText (Dhall.toTextLit newName)) originalExpr
  where
    findListTextValues :: HasLogFunc env => Text -> (Text -> a) -> RIO env (Maybe (Seq a))
    findListTextValues key f = case normalizedExpr of
      Dhall.RecordLit kvs -> case Dhall.Map.lookup key kvs of
        Just Dhall.RecordField { recordFieldValue } -> case recordFieldValue of
          Dhall.ListLit _ dependencies -> do
            Just . fmap f <$> traverse (throws . Dhall.fromTextLit) dependencies
          other -> do
            logWarn $ display $ "not listlit: " <> failedToAddDepsExpectedRecordKey other
            pure Nothing
        _ -> do
          logWarn $ display $ "no record lit: " <> failedToAddDepsExpectedRecordKey (Dhall.RecordLit kvs)
          pure Nothing
      other -> do
        logWarn $ display $ "other: " <> failedToAddDepsExpectedRecordKey other
        pure Nothing
      where
        failedToAddDepsExpectedRecordKey e =
          "Failed to add dependencies. You should have a record with the `" <> key <> "` key for this to work.\n" <>
          "Expression was: " <> pretty e

    findTextValue :: HasLogFunc env => Text -> RIO env (Maybe Text)
    findTextValue key = case normalizedExpr of
      Dhall.RecordLit kvs -> case Dhall.Map.lookup key kvs of
        Just Dhall.RecordField { recordFieldValue } -> case recordFieldValue of
          Dhall.TextLit (Dhall.Chunks [] txt) -> pure $ Just txt
          other -> do
            logWarn $ display $ "not listlit: " <> failedToAddDepsExpectedRecordKey other
            pure Nothing
        _ -> do
          logWarn $ display $ "no record lit: " <> failedToAddDepsExpectedRecordKey (Dhall.RecordLit kvs)
          pure Nothing
      other -> do
        logWarn $ display $ "other: " <> failedToAddDepsExpectedRecordKey other
        pure Nothing
      where
        failedToAddDepsExpectedRecordKey e =
          "Failed to add dependencies. You should have a record with the `" <> key <> "` key for this to work.\n" <>
          "Expression was: " <> pretty e

-- | \"dependencies\" - Reduce chance of spelling mistakes/typos
dependenciesText :: Text
dependenciesText = "dependencies"

-- | \"sources\" - Reduce chance of spelling mistakes/typos
sourcesText :: Text
sourcesText = "sources"

-- | \"name\" - Reduce chance of spelling mistakes/typos
nameText :: Text
nameText = "name"

-- | Indicates where we are in the expression
data ExprLevel
  = AtRootExpression Text
  -- ^ The outermost expression
  --   that indicates which field within the record
  --   expression we want to update.
  | SearchingForField !(NonEmpty Text)
  -- ^ An expression within the root expression
  --   where we still haven't found the record-like expression
  --   that has the next @key@ in the @keyStack@.
  --
  --   The @NonEmpty Text@ arg is a stack of keys.
  --   - @Field@ pushes new keys on top of the stack
  --   - @RecordLit@ pops off the key at the top of the stack
  --   and looks up the field corresponding do it
  --   - @With@ sometimes also pops off one or more of the keys in
  --   the @keyStack@.
  | WithinField
  -- ^ This expression is the expression we wish to update
  --   (e.g. the value associated with the @dependencies@ field when
  --   we are adding new packages).
  deriving (Eq, Show)

data UpdateResult
  = Updated !Expr
  -- ^ The expression was succesfully updated with the requested change.
  | VariableName !(Text, Int)
  -- ^ The expression to update is the binding value with this name
  --   that corresponds to the specified de Brujin index.
  --   See "Dhall.Core#t:Var".
  | EncounteredEmbed
  -- ^ As long as we have previously normalized the original expression
  --   and verified that it will produce the \"shape\" we are expecting,
  --   then when we encounter an @Embed@ constructor, we can make
  --   one assumption about it: the type of the expression
  --   must match what we are looking for in the current @ExprLevel@.
  --
  --   For example, if we are @WithinField@ and we encounter an @Embed@ case, then we know
  --   the import will produce an expression that matches the type of the one we
  --   are trying to update. For example, if it will produce an expression that has
  --   type, @List Text@, then we can wrap it in a @ListAppend embedExpr newListLitExpr@.
  --
  --   If we are @SearchingForField@ and we encounter an @Embed@ case, then we know
  --   the import will produce a record expression. In this case, we can refer to
  --   its underlying values via the @keyStack@ provided via the @SearchingForField@.

-- |
-- Basically 'Prelude.fmap' but only for the @Updated@ case.
mapUpdated :: (Expr -> Expr) -> UpdateResult -> UpdateResult
mapUpdated f (Updated e) = Updated (f e)
mapUpdated _ other = other

-- |
-- Modifies any supported Dhall expression with the requested changes.
--
-- To make this implementation cover most of the usual cases while still making this simple,
-- the following cases will NOT be supported:
-- - Lam binding expr - @λ(binding : Text) -> { dependencies = [ x ] }@
--     Although a @Lam@ ca not be a root-level expression, it could still appear in various places.
--     We could try to update the function\'s body, but without greater context, it\'s possible
--     that updating the body could affect other things, too. If someone is using something as
--     complicated as lambdas, we will force them to update the file manually.
-- - App func arg - @(λ(x : A) -> { dependencies = [ x ] }) \"bar\"@
--     This can produce a record expression and unlike @Lam@ can be a root-level expression.
--     However, this is a complex feature and can be difficult to update correctly like @Lam@.
--     Thus, we won't be covering it and instead will force the user to update the file manually.
-- - BoolIf condition thenPath elsePath - @if x then y else z@
--     If the update is in either the @thenPath@ or the @elsePath@, which one do we update?
--     Without more context, we can not know and might update the value incorrectly.
--     Thus, we force the user to manually add the dependencies if this is used.
-- - Project expr keys - @{ dependencies = [\"bar\"], other = \"foo\" }.{ dependencies }@
--     This can produce a record expresion. Since this is unlikely to be used frequently,
--     and requires a bit more work due to the type for @keys@, we will not support it below.
--     However, this could be added in a future PR.
--
-- The below cases will be supported. Each is described below with a small description of how to update them:
-- - Embed _ - @./spago.dhall@
--     This imports and refers to another Dhall expression elsewhere. Without normalizing it, we do not know what it is
--     but we can make assumptions about it. See the @EncounteredEmbed@ constructor for `UpdateResult`.
-- - ListLit - @[\"a\", \"literal\", \"list\", \"of\", \"values\"]@
--     When the `AstUpdate` is a @InsertListText additions@, updating this expression merely means adding the @additions@ to its list.
-- - ListAppend - @[\"list1\"] # [\"list2\"]@
--     Similar to @ListLit@ except it appends two list expressions together.
--     If it doesn't contain a @ListLit@ (e.g. @expr1 # expr2@), we can wrap the entire
--     expression in a @ListAppend originalExpr listExprWithNewPkgs@.
-- - RecordLit - @{ key = value }@
--     This is ultimately what we are looking for, so we can update the respective field.
--     However, due to supporting @Field@, which selects values within a record,
--     we need to support looking up other keys besides the main key (e.g @dependencies@).
-- - Field recordExpr selection - @{ key = { dependencies = [\"bar\"] } }.key@
--     This can produce a record expression. Depending on the record expression,
--     we might need to update the values within the record expression
--     that are ultimately exposed via the key (e.g. @{ config = { ..., dependencies = []} }.config@)
-- - Prefer recordExpr overrides - @{ dependencies = [\"foo\"] } \/\/ { dependencies = [\"bar\"] }@
--     This produces a record expression. If we update the @recordExpr@
--     arg and its @dependencies@ is overridden by @overrides@, then the update is pointless.
--     If the @overrides@ value overrides something irrelevant to the record\'s @dependencies@ field,
--     then we need to update the @recordExpr@. So, we need to try to update the @overrides@ arg first
--     and only if that fails do we attempt to update the @recordExpr@.
-- - With recordExpr field update - @{ dependencies = [\"foo\"] } with dependencies = [\"bar\"]@
--     This produces a record expression. Similar to @Prefer@,
--     we should attempt to update the @update@ value first before attempting to update @recordExpr@.
-- - Let binding expr - @let binding = value in expr@
--     The expression we need to update will often be in the @expr@ (i.e. after the @in@ keyword;
--     > let src = [ "src" ] in { ..., dependencies = [ "old" ], sources = src }`)
--     However, we might need to update the expression associated with the bound variable name.
--     (e.g. @let config = { dependencies = [ \"foo\" ] } in config@). We will not know
--     until we have looked at what the @dependencies@ key refers to before finding out that
--     the binding for the variable name @config@ is what we need to update.
--
-- Since this is modifying the raw AST and might produce an invalid configuration file,
-- the returned expression should be verified to produce a valid configuration format.
modifyRawDhallExpression :: HasLogFunc env => Text -> AstUpdate -> Expr -> RIO env Expr
modifyRawDhallExpression initialKey astMod originalExpr = do
  result <- updateExpr (AtRootExpression initialKey) originalExpr
  case result of
    Just (Updated newExpr) -> do
      pure newExpr
    _ -> do
      pure originalExpr
  where
    -- |
    -- Adds the additions to a @ListLit@'s @Seq@ argument
    updateListTextByAppending :: Seq Expr -> Seq Expr -> Seq Expr
    updateListTextByAppending additions listLitSeqArg = Seq.sort (additions <> listLitSeqArg)

    -- |
    -- Removes some boilerplate: changes @expr@ to @expr # [\"new\"]@
    updateListTextByWrappingListAppend :: Seq Expr -> Expr -> Expr
    updateListTextByWrappingListAppend additions expr =
      Dhall.ListAppend expr $ Dhall.ListLit Nothing $ updateListTextByAppending additions Seq.empty

    updateListTextByMergingListLits :: Seq Expr -> Expr -> Maybe Expr
    updateListTextByMergingListLits additions expr = case expr of
      Dhall.ListLit ann ls -> Just $ Dhall.ListLit ann $ updateListTextByAppending additions ls
      Dhall.ListAppend left right -> do
        (\newLeft -> Dhall.ListAppend newLeft right) <$> updateListTextByMergingListLits additions left
        <|> (\newRight -> Dhall.ListAppend left newRight) <$> updateListTextByMergingListLits additions right
      _ -> Nothing

    -- |
    -- > ./spago.dhall
    -- (or some other expression where the required update is within the embed (e.g. @./spago.dhall // { sources = [\"foo\"] }@)
    -- to
    -- > let varName = ./spago.dhall
    -- > in  varName
    -- >       with dependencies = varName.dependencies # ["new"]
    updateListTextByWrappingLetBinding :: NonEmpty Text -> Seq Expr -> Text -> Expr -> Expr
    updateListTextByWrappingLetBinding keyStack additions varName expr = do
      let
        var = Dhall.Var (Dhall.V varName 0)

        -- `let __embed = expr`
        binding = Dhall.makeBinding varName expr

        -- `__embed.key1.key2.key3`
        varSelect = foldl' (\acc nextKey -> Dhall.Field acc (Dhall.makeFieldSelection nextKey)) var keyStack

        -- `__embed.dependencies # ["new"]`
        lsAppend =
          Dhall.ListAppend
            varSelect
            (Dhall.ListLit Nothing $ updateListTextByAppending additions Seq.empty)
      Dhall.Let binding $ Dhall.With var keyStack lsAppend

    -- | Updates an expression by recursively updating any subexpressions
    -- until the update succeeds (@Just Updated@) or fails (@Nothing@). If called
    -- on a \"root-level\" expression, @Just EncounteredEmbed@ and @Just VariableName@ also
    -- count as failure. For subexpressions, these returned values may be used to determine how to
    -- update a subexpression found within the \"root-level\" expression.
    updateExpr :: HasLogFunc env => ExprLevel -> Expr -> RIO env (Maybe UpdateResult)
    updateExpr level expr = case expr of
      -- ./spago.dhall
      Dhall.Embed _ -> do
        case level of
          AtRootExpression key -> do
            case astMod of
              --    `./spago.dhall`
              -- to
              --    `let __embed = ./spago.dhall in __embed with key = __embed.key # additions`
              InsertListText additions -> do
                pure $ Just $ Updated $ updateListTextByWrappingLetBinding (key :| []) additions "__embed" expr

              --    `./spago.dhall`
              -- to
              --    `./spago.dhall with key = "new"`
              SetText t -> do
                pure $ Just $ Updated $ Dhall.With expr (key :| []) t

          SearchingForField _ ->
            pure $ Just EncounteredEmbed

          WithinField ->
            pure $ Just EncounteredEmbed

      -- let varname = ... in ... varName
      Dhall.Var (Dhall.V varName deBrujinIndex) -> do
        case level of
          AtRootExpression _ -> do
            -- This should never happen because we've already verified that the normalized expression
            -- produces a RecordLit with a dependencies field that stores a list of text values.
            pure Nothing

          WithinField -> do
            case astMod of
              --  `let pkg = [ "package" ] in { ..., dependencies = pkg }
              -- to
              --  `let pkg = [ "package" ] in { ..., dependencies = pkg # [ "newPackage" ] }
              -- For this expression, we wrap it in a `ListAppend`
              InsertListText additions -> do
                pure $ Just $ Updated $ updateListTextByWrappingListAppend additions expr

              --  `let x = "old" in { ..., name = x, otherKey = x }
              -- to
              --  `let x = "old" in { ..., name = "new", otherKey = x }
              SetText t -> do
                pure $ Just $ Updated t

          SearchingForField _ -> do
            -- We got to the final expression and find that the real expression is stored
            -- in a binding. For example, the second `config` in
            --    `let config = { ..., dependencies = ... } in config`
            pure $ Just $ VariableName (varName, deBrujinIndex)

      Dhall.TextLit _ -> do
        case level of
          AtRootExpression _ -> pure Nothing
          SearchingForField _ -> pure Nothing
          WithinField -> case astMod of
            --  `{ ..., name = "old" }
            -- to
            --  `{ ..., name = "new" }
            SetText t -> pure $ Just $ Updated t
            InsertListText _ -> pure Nothing

      -- ["foo", "bar"]
      Dhall.ListLit ann ls -> do
        case level of
          AtRootExpression _ -> do
            -- This should never happen because we've already verified that the normalized expression
            -- produces a RecordLit with a dependencies field that stores a list of text values.
            pure Nothing

          SearchingForField _ -> do
            -- This could arise if one was storing a list at one point and then trying to access
            -- an element within the list at another point. Since it's uncommon, we won't
            -- support it here.
            pure Nothing

          WithinField -> do
            case astMod of
              --  `{ ..., dependencies = ["old"] }
              -- to
              --  `{ ..., dependencies = ["old", "new"] }
              InsertListText additions -> do
                pure $ Just $ Updated $ Dhall.ListLit ann $ updateListTextByAppending additions ls

              SetText _ -> do
                pure Nothing

      -- left # right
      Dhall.ListAppend left right -> do
        case level of
          AtRootExpression _ -> do
            -- This should never happen because we've already verified that the normalized expression
            -- produces a RecordLit with a dependencies field that stores a list of text values.
            pure Nothing

          SearchingForField _ -> do
            -- This could arise if one was storing a list at one point and then trying to access
            -- an element within the list at another point. Since it's uncommon, we won't
            -- support it here.
            pure Nothing

          WithinField -> do
            case astMod of
              --  `["foo"] # expr` -> `["old", "new"] # expr`
              --  `expr # ["old"]` -> `expr # ["old", "new"]`
              --  `expr1 # expr2` -> `expr1 # expr2 # ["new"]`
              InsertListText additions -> do
                Just . Updated <$> do
                  let
                    mergeResult =
                      (\newLeft -> Dhall.ListAppend newLeft right) <$> updateListTextByMergingListLits additions left
                      <|> (\newRight -> Dhall.ListAppend left newRight) <$> updateListTextByMergingListLits additions right
                  case mergeResult of
                    Just lsAppend -> do
                      pure lsAppend
                    Nothing -> do
                      -- Since we couldn't add the update to an existing ListLit
                      -- we'll just add it the the end
                      pure $ updateListTextByWrappingListAppend additions expr

              SetText _ -> do
                pure Nothing

      -- { key = value, ... }
      Dhall.RecordLit kvs -> do
        case level of
          WithinField ->
            -- This should never happen because we've already verified that the normalized expression
            -- produces a RecordLit with a dependencies field that stores a list of text values.
            pure Nothing

          SearchingForField (key :| keys) -> do
            case Dhall.Map.lookup key kvs of
              Nothing -> do
                pure Nothing
              Just Dhall.RecordField { recordFieldValue } -> do
                let
                  updateRecordLit =
                    Dhall.RecordLit
                      . flip (Dhall.Map.insert key) kvs
                      . Dhall.makeRecordField

                  newLevel = case keys of
                    [] -> WithinField
                    (nextKey:tail) -> SearchingForField (nextKey :| tail)

                fmap (mapUpdated updateRecordLit) <$> updateExpr newLevel recordFieldValue

          AtRootExpression key -> do
            case Dhall.Map.lookup key kvs of
              Nothing -> do
                pure Nothing
              Just Dhall.RecordField { recordFieldValue } -> do
                let
                  updateRecordLit =
                    Dhall.RecordLit
                      . flip (Dhall.Map.insert key) kvs
                      . Dhall.makeRecordField

                fmap (mapUpdated updateRecordLit) <$> updateExpr WithinField recordFieldValue

      -- recordExpr.selection
      Dhall.Field recordExpr selection@Dhall.FieldSelection { fieldSelectionLabel } -> do
        case level of
          WithinField -> do
            case astMod of
              --  `{ dependencies = otherConfig.someKey }`
              -- to
              --  `{ dependencies = otherConfig.someKey # [ "new" ] }`
              InsertListText additions -> do
                pure $ Just $ Updated $ updateListTextByWrappingListAppend additions expr

              --  `let x = { someKey = "foo" } in { ..., name = otherConfig.someKey }`
              -- to
              --  `let x = { someKey = "new" } in { ..., name = otherConfig.someKey }`
              SetText _ -> do
                fmap (mapUpdated (\newRecord -> Dhall.Field newRecord selection)) <$> updateExpr level recordExpr

          AtRootExpression key -> do
            --  `{ config = { ..., dependencies = [ "package" ] } }.config`
            -- to
            --  `{ config = { ..., dependencies = [ "package", "new" ] } }.config`
            let
              newLevel = SearchingForField (fieldSelectionLabel :| [key])
            maybeResult <- fmap (mapUpdated (\newRecord -> Dhall.Field newRecord selection)) <$> updateExpr newLevel recordExpr
            case maybeResult of
              Just EncounteredEmbed -> do
                case astMod of
                  InsertListText additions -> do
                    pure $ Just $ Updated $ Dhall.Field (updateListTextByWrappingLetBinding (key :| []) additions "__embed" expr) selection

                  SetText t -> do
                    pure $ Just $ Updated $ Dhall.Field (Dhall.With expr (key :| []) t) selection

              Just (Updated _) -> do
                pure maybeResult

              Just (VariableName _) -> do
                pure maybeResult

              Nothing -> do
                pure maybeResult

          SearchingForField keys -> do
            let
              newLevel = SearchingForField (fieldSelectionLabel `NonEmpty.cons` keys)
            fmap (mapUpdated (\newRecord -> Dhall.Field newRecord selection)) <$> updateExpr newLevel recordExpr

      -- left // right
      Dhall.Prefer charSet preferAnn left right -> do
        case level of
          WithinField -> do
            case astMod of
              InsertListText _ -> do
                -- Can't insert a list of text values into a record expression.
                -- Prefer only works on record expressions, so this is an invalid expression.
                pure Nothing

              SetText _ -> do
                -- Can't insert a list of text values into a record expression.
                -- Prefer only works on record expressions, so this is an invalid expression.
                pure Nothing

          AtRootExpression key -> do
            -- Two possibilities:
            --
            --  Override:
            --    `{ ..., dependencies = ["old"] } // { dependencies = ["package"] }`
            --   to
            --    `{ ..., dependencies = ["old"] } // { dependencies = ["package", "new"] }`
            --
            --  Irrelvant:
            --    `{ ..., dependencies = ["old"] } // { sources = ["src"] }`
            --   to
            --    `{ ..., dependencies = ["old", "new"] } // { sources = ["src"] }`
            let
              newLevel = SearchingForField (key :| [])
            maybeRight <- updateExpr newLevel right
            case maybeRight of
              Just EncounteredEmbed -> do
                case astMod of
                  InsertListText additions -> do
                    pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn left
                      $ updateListTextByWrappingLetBinding (key :| []) additions "__embed" right

                  --    ` { ..., name = "old1" } // ./spago.dhall`
                  -- to
                  --    `({ ..., name = "old1" } // ./spago.dhall) with key = "new"`
                  SetText t -> do
                    pure $ Just $ Updated $ Dhall.With expr (key :| []) t

              Just (VariableName _) -> do
                -- `The `Just VariableName` can't happen here because this is `AtRootExpression`
                pure maybeRight

              Just (Updated newRight) -> do
                pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn left newRight

              Nothing -> do
                maybeLeft <- updateExpr newLevel left
                case maybeLeft of
                  Just EncounteredEmbed -> do
                    case astMod of
                      InsertListText additions -> do
                        case right of
                          --    `./spago.dhall // { irrelevantKey = "irrelevantValue" }`
                          -- to
                          --    `let __embed = ./spago.dhall in embed // { irrelevantKey = "irrelevantValue", dependencies = embed.dependencies # ["new" ] }`
                          Dhall.RecordLit kvs -> do
                            let
                              varName = "__embed"
                              var = Dhall.Var (Dhall.V varName 0)

                              -- `let __embed = ./spago.dhall`
                              binding = Dhall.makeBinding varName left

                              -- `__embed.dependencies # ["new" ]`
                              lsAppend =
                                updateListTextByWrappingListAppend additions
                                  $ Dhall.Field var (Dhall.makeFieldSelection key)

                              -- `{ irrelevantKey = "irrelevantValue", key = __embed.dependencies # ["new"] }`
                              newRight =
                                Dhall.RecordLit
                                $ flip (Dhall.Map.insert key) kvs
                                $ Dhall.makeRecordField lsAppend
                            pure $ Just $ Updated $ Dhall.Let binding (Dhall.Prefer charSet preferAnn var newRight)

                          --    `./spago.dhall // { x = { irrelevantKey = "irrelevantValue" } }.x`
                          -- to
                          --    `( let __embed = ./spago.dhall
                          --       in  __embed with dependencies = __embed.dependencies # ["name"]
                          --     ) // { x = { irrelevantKey = "irrelevantValue" } }.x`
                          _ -> do
                            pure $ Just $ Updated
                              $ Dhall.Prefer charSet preferAnn (updateListTextByWrappingLetBinding (key :| []) additions "__embed" left) right

                      SetText t -> do
                        case right of
                          --    `./spago.dhall // { irrelevantKey = "irrelevantValue" }`
                          -- to
                          --    `./spago.dhall // { irrelevantKey = "irrelevantValue", name = "new" }`
                          Dhall.RecordLit kvs -> do
                            let
                              newRight =
                                Dhall.RecordLit
                                $ flip (Dhall.Map.insert key) kvs
                                $ Dhall.makeRecordField t
                            pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn left newRight

                          --    `./spago.dhall // { x = { irrelevantKey = "irrelevantValue" } }.x`
                          -- to
                          --    `./spago.dhall with name = "name" // { x = { irrelevantKey = "irrelevantValue" } }.x`
                          _ -> do
                            pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn (Dhall.With left (key :| []) t) right

                  Just (VariableName _) -> do
                    -- `The `Just VariableName` can't happen here because this is `AtRootExpression`
                    pure maybeLeft

                  Just (Updated newLeft) -> do
                    pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn newLeft right

                  Nothing -> do
                    pure maybeLeft

          SearchingForField _ -> do
            -- See Dhall.Prefer's `AtRootExpression` case
            -- but for this situation, we're trying to find a record, so we don't match against a "dependencies" field
            maybeRight <- updateExpr level right
            case maybeRight of
              Just EncounteredEmbed -> do
                pure maybeRight

              Just (VariableName _) -> do
                pure maybeRight

              Just (Updated newRight) -> do
                pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn left newRight

              Nothing -> do
                maybeLeft <- updateExpr level left
                case maybeLeft of
                  Just EncounteredEmbed -> do
                    pure maybeLeft

                  Just (VariableName _) -> do
                    pure maybeLeft

                  Just (Updated newLeft) -> do
                    pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn newLeft right

                  Nothing -> do
                    pure maybeLeft

      -- recordExpr with field1.field2.field3 = update
      Dhall.With recordExpr field update ->
        case level of
          WithinField -> do
            case astMod of
              -- The field we're updating is a `List Text` value, not a record.
              InsertListText _ -> do
                pure Nothing

              -- The field we're updating is a `List Text` value, not a record.
              SetText _ -> do
                pure Nothing

          AtRootExpression key | field == (key :| []) -> do
            --    `{ ..., dependencies = ["old"] } with dependencies = ["package"]`
            --  to
            --    `{ ..., dependencies = ["old"] } with dependencies = ["package", "new"]`
            maybeResult <- updateExpr WithinField update
            case maybeResult of
              Just EncounteredEmbed -> do
                case astMod of
                  --    `{ ..., dependencies = ["old"] } with dependencies = ./deps.dhall`
                  --  to
                  --    `{ ..., dependencies = ["old"] } with dependencies = ./deps.dhall # ["new"]`
                  InsertListText additions -> do
                    pure $ Just $ Updated $ Dhall.With recordExpr field $ updateListTextByWrappingListAppend additions update

                  --    `{ ..., name = ["old"] } with name = ./deps.dhall`
                  --  to
                  --    `{ ..., name = ["old"] } with name = "new"`
                  SetText t -> do
                    pure $ Just $ Updated $ Dhall.With recordExpr field t

              Just (VariableName _) -> do
                -- This can't happen because the Dhall expression is invalid. There can't be a variable name
                -- if there isn't a let binding.
                --    `{ ..., dependencies = ["old"] } with dependencies = x`
                pure maybeResult

              Just (Updated newUpdate) -> do
                pure $ Just $ Updated $ Dhall.With recordExpr field newUpdate

              Nothing -> do
                -- This can't happen because the Dhall expression is invalid. If this is a Root level With, then
                -- recordExpr must be an expression that produces our Config schema, which means it will have
                -- a dependencies field. So, if we failed to update that dependencies field, then the expression
                -- itself is invalid.
                pure maybeResult

          AtRootExpression key -> do
            let
              newLevel = SearchingForField (key :| [])
            maybeResult <- updateExpr newLevel recordExpr
            case maybeResult of
              Just EncounteredEmbed -> do
                case astMod of
                  --     ```
                  --     ./spago.dhall
                  --       with someField = update
                  --     ```
                  --  to
                  --     ```
                  --     let __embed = ./spago.dhall
                  --     in embed
                  --          with field = update
                  --          with dependencies = embed.dependencies # ["new"]
                  --     ```
                  InsertListText additions -> do
                    let
                      varName = "__embed"
                      var = Dhall.Var (Dhall.V varName 0)

                      -- `let __embed = recordExpr`
                      binding = Dhall.makeBinding varName recordExpr

                      -- `var.dependencies # ["new"]`
                      astUpdate =
                        Dhall.ListAppend (Dhall.Field var (Dhall.makeFieldSelection key))
                          $ Dhall.ListLit Nothing $ updateListTextByAppending additions Seq.empty

                    pure $ Just $ Updated
                      $ Dhall.Let binding
                      -- Note: we can't use `Prefer` here to merge two `With` updates into a single record update
                      -- (e.g. `foo with key1 = x with key2 = x` -> foo // { key1 = x, key2 = y }` )
                      -- because we might have a nested selector
                      -- (e.g. `{ outer = { inner = [] } } with outer.inner = ["foo"]` ).
                      -- So, we must instead wrap it in another `With`
                      -- (e.g. `{ outer = { inner = [] }, dependencies = [] } with outer.inner = ["foo"] with dependencies = ["foo"]` ).
                      $ Dhall.With (Dhall.With var field update) (key :| []) astUpdate

                  --     ```
                  --     ./spago.dhall
                  --        with someField = update
                  --     ```
                  --  to
                  --     ```
                  --     ./spago.dhall
                  --        with field = update
                  --        with name = "new"
                  --     ```
                  SetText t -> do
                    pure $ Just $ Updated $ Dhall.With expr (key :| []) t

              Just (VariableName _) -> do
                -- `The `Just VariableName` can't happen here because this is `AtRootExpression`
                pure maybeResult

              Just (Updated newUpdate) -> do
                pure $ Just $ Updated $ Dhall.With recordExpr field newUpdate

              Nothing -> do
                pure maybeResult

          SearchingForField keyStack -> do
            {-
              ```
              ({ outer = { config = { dependencies = ... } } }
                with outer = { config = { dependencies = ... } }
              ).outer.config
            -}
            let
              levelForUpdate :: NonEmpty Text -> NonEmpty Text -> Maybe ExprLevel
              levelForUpdate (fieldKey :| fieldKeys) (nextKey :| nextKeys)
                | fieldKey == nextKey = case (fieldKeys, nextKeys) of
                    ([], []) ->
                      Just WithinField
                    ([], nextKey':nextKeys') ->
                      Just $ SearchingForField (nextKey' :| nextKeys')
                    (_:_, []) -> Nothing
                    (fieldKey':fieldKeys', nextKey': nextKeys') ->
                      levelForUpdate (fieldKey' :| fieldKeys') (nextKey' :| nextKeys')
                | otherwise = Nothing

            case levelForUpdate field keyStack of
              Just levelForUpdateSearch -> do
                maybeUpdate <- updateExpr levelForUpdateSearch update
                case maybeUpdate of
                  Just EncounteredEmbed -> do
                    pure maybeUpdate

                  Just (VariableName _) -> do
                    pure maybeUpdate

                  Just (Updated newUpdate) -> do
                    pure $ Just $ Updated $ Dhall.With recordExpr field newUpdate

                  Nothing ->
                    updateRecordExpr

              Nothing -> do
                updateRecordExpr

            where
              updateRecordExpr = do
                maybeRecordExpr <- updateExpr level recordExpr
                case maybeRecordExpr of
                  Just EncounteredEmbed -> do
                    pure maybeRecordExpr

                  Just (VariableName _) -> do
                    pure maybeRecordExpr

                  Just (Updated newRecordExpr) -> do
                    pure $ Just $ Updated $ Dhall.With newRecordExpr field update

                  Nothing -> do
                    pure maybeRecordExpr

      Dhall.Let binding@Dhall.Binding { variable, value } inExpr -> do
        case level of
          WithinField -> do
            case astMod of
              --    `let lsBinding = ["old"] ... in { dependencies =  let x = lsBinding in x }`
              -- to
              --    `let lsBinding = ["old"] ... in { dependencies = (let x = lsBinding in x) # ["new"] }`
              InsertListText additions -> do
                pure $ Just $ Updated $ updateListTextByWrappingListAppend additions expr

              --    `let lsBinding = "old" ... in { name = let x = lsBinding in x }`
              -- to
              --    `let lsBinding = "old" ... in { name = "new" }`
              SetText t -> do
                pure $ Just $ Updated t

          AtRootExpression key -> do
            let
              newLevel = SearchingForField (key :| [])
            maybeResult <- updateExpr newLevel inExpr
            case maybeResult of
              Just EncounteredEmbed -> do
                case astMod of
                  --  `let useless = "foo" in ./spago.dhall`
                  -- to
                  --  ```
                  --  let useless = "foo"
                  --  let __embed = ./spago.dhall
                  --  in  __embed with dependencies = __embed.dependencies # ["new"]
                  --  ```
                  InsertListText additions -> do
                    pure $ Just $ Updated $ Dhall.Let binding $ updateListTextByWrappingLetBinding (key :| []) additions "__embed" inExpr

                  --  `let useless = "foo" in ./spago.dhall`
                  -- to
                  --  ```
                  --  let useless = "foo"
                  --  in ./spago.dhall with name = "new"
                  --  ```
                  SetText t -> do
                    pure $ Just $ Updated $ Dhall.Let binding $ Dhall.With inExpr (key :| []) t

              Just (Updated newInExpr) -> do
                pure $ Just $ Updated $ Dhall.Let binding newInExpr

              Just (VariableName (name, deBrujinIndex)) | name == variable && deBrujinIndex == 0 -> do
                maybeValue <- updateExpr newLevel value
                case maybeValue of
                  Just EncounteredEmbed -> do
                    case astMod of
                      --    `let x = ./spago.dhall in x`
                      -- to
                      --    `let x = (let __embed = ./spago.dhall in __embed with key = __embed.key # additions) in x`
                      InsertListText additions -> do
                        pure $ Just $ Updated $ Dhall.Let (Dhall.makeBinding variable
                          $ updateListTextByWrappingLetBinding (key :| []) additions "__embed" value) inExpr

                      --    `let x = ./spago.dhall in x`
                      -- to
                      --    `let x = ./spago.dhall with name = "new" in x`
                      SetText t -> do
                        pure $ Just $ Updated
                          $ Dhall.Let
                              (Dhall.makeBinding variable (Dhall.With value (key :| []) t))
                              inExpr

                  Just (Updated newValue) -> do
                    pure $ Just $ Updated $ Dhall.Let (Dhall.makeBinding variable newValue) inExpr

                  Just (VariableName _) -> do
                    -- invalid Dhall expression because this is AtRootExpression
                    pure maybeValue

                  Nothing -> do
                    pure maybeValue

              Just (VariableName _) -> do
                -- Invalid Dhall expression because this is AtRootExpression
                pure maybeResult

              Nothing -> do
                pure maybeResult

          SearchingForField keyStack -> do
            maybeResult <- updateExpr level inExpr
            case maybeResult of
              embedded@(Just EncounteredEmbed) -> do
                pure embedded

              Just (Updated newInExpr) -> do
                pure $ Just $ Updated $ Dhall.Let binding newInExpr

              Just (VariableName (name, deBrujinIndex)) | name == variable, deBrujinIndex > 0 -> do
                pure $ Just $ VariableName (name, deBrujinIndex - 1)

              Just (VariableName (name, deBrujinIndex)) | name == variable && deBrujinIndex == 0 -> do
                maybeValue <- updateExpr level value
                case maybeValue of
                  Just EncounteredEmbed -> do
                    case astMod of
                      --    `let x = { key1 = ./spago.dhall } in x.key1`
                      -- to
                      --    `let x = { key1 = ./spago.dhall } with key1.dependencies = key.dependencies # ["new"] in x.key1`
                      InsertListText additions -> do
                        pure $ Just $ Updated $ Dhall.Let (Dhall.makeBinding variable
                          $ updateListTextByWrappingLetBinding keyStack additions "__embed" value) inExpr

                      --    `let x = { key1 = ./spago.dhall } in x.key1`
                      -- to
                      --    `let x = { key1 = ./spago.dhall } with key.name = "new" in x.key1`
                      SetText t -> do
                        pure $ Just $ Updated $ Dhall.Let (Dhall.makeBinding variable
                          $ Dhall.With value keyStack t) inExpr

                  Just (Updated newValue) -> do
                    pure $ Just $ Updated $ Dhall.Let (Dhall.makeBinding variable newValue) inExpr

                  Just (VariableName _) -> do
                    -- `let x = "foo" let y = x in y`
                    -- This binding refers to another binding
                    pure maybeValue

                  Nothing -> do
                    pure maybeValue

              Just (VariableName _) -> do
                -- Variable name doesn't match this let binding's name
                pure maybeResult

              Nothing -> do
                pure maybeResult

      _ -> do
        pure Nothing
