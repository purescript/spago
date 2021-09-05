{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Spago.Config.AST
  ( AstModification(..)
  , modifyRawAST
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Parser          as Parser
import qualified Spago.Dhall           as Dhall
import qualified Data.List.NonEmpty    as NonEmpty


type Expr = Dhall.Expr Parser.Src Dhall.Import
type ResolvedExpr = Dhall.Expr Parser.Src Void

-- |
-- Indicates the change Spago wants to make to the expression
data AstModification
  = AddPackages ![PackageName]
  | AddSources ![Text]
  | SetName Text

-- |
-- Indicates the change to make once inside the expression
data AstUpdate
  = InsertListText (Seq Expr)
  | SetText Expr

-- |
-- Since the user may be requesting to add packages that don't exist in the package set,
-- we only continue if all of them exist.
--
-- We then need to determine which of the user-requested packages
-- haven't been added yet and only add those (e.g. if user wants to add `foo` and `bar`,
-- but `bar` is already added, we only add `foo`).
--
-- To accomplish this goal, we normalize the expression first. There are two reasons why.
-- First, it makes the implementation simpler. Rather than figuring out which have been added
-- while we're trying to add the new packages, we break this into two steps: 1) determining
-- what those not-yet-added package are and 2) where to add them in the AST.
-- Breaking this down into two problems makes the solving the second problem easier.
--
-- Second, by confirming below that the normalized expression found in the `spago.dhall` file
-- IS a `RecordLit` with a `dependencies` field, we can make some assumptions about
-- the `Expr` passed into `modifyRawAST'`. For example, we don't need to know what `Embed` cases
-- are because we can infer based on where we are in the expression whether they are a
-- Record expression with a "dependencies" key or a List expression.
--
-- In other words, `modifyRawAST'` can actually succeed for the cases we support.
modifyRawAST :: HasLogFunc env => AstModification -> ResolvedExpr -> Expr -> RIO env Expr
modifyRawAST astMod normalizedExpr originalExpr = case astMod of
  AddPackages newPackages -> do
    mbAllInstalledPkgs <- findListTextValues dependenciesText PackageName
    case mbAllInstalledPkgs of
      Nothing -> do
        pure originalExpr
      Just allInstalledPkgs -> do
        let pkgsToInstall = nubSeq $ Seq.filter (`notElem` allInstalledPkgs) $ Seq.fromList newPackages
        if null pkgsToInstall
        then do
          pure originalExpr
        else do
          modifyRawAST' dependenciesText (InsertListText (Dhall.toTextLit . packageName <$> pkgsToInstall)) originalExpr
  AddSources newSources -> do
    mbAllInstalledPkgs <- findListTextValues sourcesText id
    case mbAllInstalledPkgs of
      Nothing -> do
        pure originalExpr
      Just allInstalledPkgs -> do
        let sourcesToInstall = nubSeq $ Seq.filter (`notElem` allInstalledPkgs) $ Seq.fromList newSources
        if null sourcesToInstall
        then do
          pure originalExpr
        else do
          modifyRawAST' sourcesText (InsertListText (Dhall.toTextLit <$> sourcesToInstall)) originalExpr

  SetName newName -> do
    mbName <- findTextValue nameText
    case mbName of
      Nothing -> do
        pure originalExpr
      Just originalName
        | originalName == newName -> do
            pure originalExpr
        | otherwise -> do
            modifyRawAST' nameText (SetText (Dhall.TextLit (Dhall.Chunks [] newName))) originalExpr
  where
    -- | Code from https://stackoverflow.com/questions/45757839
    nubSeq :: Ord a => Seq a -> Seq a
    nubSeq xs = (fmap fst . Seq.filter (uncurry notElem)) (Seq.zip xs seens)
      where
        seens = Seq.scanl (flip Set.insert) Set.empty xs

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

-- | "dependencies" - Reduce chance of spelling mistakes/typos
dependenciesText :: Text
dependenciesText = "dependencies"

-- | "sources" - Reduce chance of spelling mistakes/typos
sourcesText :: Text
sourcesText = "sources"

-- | "name" - Reduce chance of spelling mistakes/typos
nameText :: Text
nameText = "name"

-- | Indicates where we are in the expresion
data ExprLevel
  = AtRootExpression Text
  -- ^ The outermost expression
  --   that indicates which field within the record
  --   expression we want to update.
  | SearchingForField !(NonEmpty Text)
  -- ^ An expression within the root expression
  --   where we still haven't found the record with the
  --   `dependencies` field key.
  --   The list of Text values is a stack of keys. We use
  --   the key on the head of the list when we encounter a
  --   `RecordLit` to loookup a field within that record.
  --   `Field` pushes new keys on top of the stack
  --   `RecordLit` looks up those fields and drops the keys on the stack
  --   Transitioning from `AtRootExpression` to `SearchingForField`
  --   should therefore always have `"dependencies"` at the bottom of the stack.
  | WithinField
  -- ^ This expression is the value associated with the `dependencies` field.
  --   It must be a `List`-like structure, so immediately append the new packages
  --   to the expression via a `ListAppend` or merge `ListLit`s together
  --   if it's a `ListAppend`.
  deriving (Eq, Show)

data UpdateResult
  = Updated !Expr
  -- ^ The expression was succesfully updated with the new packages
  | VariableName !(Text, Int)
  -- ^ The expression to update is the binding value with this name
  --   that corresponds to the specified de Brujin index
  | EncounteredEmbed
  -- ^ We encountered an `Embed` constructor. We can make only
  --   two assumptions about it:
  --    1. If we are `WithinField`, then this must be an
  --       expression that produces a `List`-like structure.
  --    2. We are NOT `WithinField`, then this must be an
  --       expression that produces a `Record`-like Config-schema structure.
  --       Thus, a "dependencies" key should exist. If it doesn't, the
  --       Dhall expression is invalid (as verified by our prior normalized expression)

-- |
-- Basically `fmap` but only for the `Updated` case.
mapUpdated :: (Expr -> Expr) -> UpdateResult -> UpdateResult
mapUpdated f (Updated e) = Updated (f e)
mapUpdated _ other = other

-- |
-- A Configuration's Dhall expression is anything that, when normalized, produces a
-- "record expression" whose `dependencies` key contains a "list expression" of text that
-- corresponds to package names.
--
-- To make this implementation cover most of the usual cases while still making this simple,
-- the following cases will NOT be supported:
-- - Lam binding expr - `λ(binding : Text) -> { dependencies = [ x ] }`
--     Although a `Lam` can't be a root-level expression, it could still appear in various places.
--     We could try to update the function's body, but without greater context, it's possible
--     that updating the body could affect other things, too. If someone is using something as
--     complicated as lambdas, we'll force them to update the file manually.
-- - App func arg - `(λ(x : A) -> { dependencies = [ x ] }) "bar"`
--     This can produce a record expression and unlike `Lam` can be a root-level expression.
--     However, this is a complex feature and can be difficult to update correctly like `Lam`.
--     Thus, we won't be covering it and instead will force the user to update the file manually.
-- - BoolIf condition thenPath elsePath - `if x then y else z`
--     If the update is in either the `thenPath` or the `elsePath`, which one is it?
--     Withought more context, we can't know and might update the value incorrectly.
--     Thus, we force the user to manually add the dependencies if this is used.
-- - Project expr keys - `{ dependencies = ["bar"], other = "foo" }.{ dependencies }`
--     This can produce a record expresion. Since this is unlikely to be used frequently,
--     and requires a bit more work due to the type for `keys`, we won't support it below.
--
-- The below cases will be supported. Each is described below with a small description of how to update them:
-- - Embed _ - `./spago.dhall`
--     This refers to another Dhall expression elssewhere. Without normalizing it, we don't know what it is
--     but we can make assumptions about it. See the `EncounteredEmbed` constructor for `UpdateResult`.
-- - ListLit - ["a", "literal", "list", "of", "values"]
--     This is what will often be the expression associated with the "dependencies" key.
--     Updating it merely means adding the new packages to its list.
-- - ListAppend - `["list1"] # ["list2"]`
--     Similar to `ListLit` except it appends two list expressions together.
--     If it doesn't contain a `ListLit` (e.g. `expr1 # expr2`), we can wrap the entire
--     expression in a `ListAppend originalExpr listExprWithNewPkgs`.
-- - RecordLit - `{ key = value }`
--     This is ultimately what we're looking for, so we can update the `dependencies`' field's list.
--     However, due to supporting `Field`, which selects values within a record,
--     we need to support looking up other keys besides the "dependencies" key.
-- - Field recordExpr selection - `{ key = { dependencies = ["bar"] } }.key`
--     This can produce a record expression. Depending on the record expression,
--     we might need to update the values within the record expression
--     that are ultimately exposed via the key (e.g. `{ config = { ..., dependencies = []} }.config`)
-- - Prefer recordExpr overrides - `{ dependencies = ["foo"] } // { dependencies = ["bar"] }`
--     This produces a record expression. If we update the `recordExpr`
--     arg and its `dependencies` is overridden by `overrides`, then the update is pointless.
--     If the `overrides` value overrides something irrelevant to the record's `dependencies` field,
--     then we need to update the `recordExpr`. So, we need to try to update the `overrides` arg first
--     and only if that fails do we attempt to update the `recordExpr`.
-- - With recordExpr field update - `{ dependencies = ["foo"] } with dependencies = ["bar"]`
--     This produces a record expression. Similar to `Prefer`,
--     we should attempt to update the `update` value first before attempting to update `recordExpr`.
-- - Let binding expr - `let binding = value in expr`
--     The expression we need to update will often be in the `expr` (i.e. after the 'in' keyword;
--     ``let src = [ "src" ] in { ..., dependencies = [ "old" ], sources = src }`).
--     However, we might need to update the expression associated with the bound variable name.
--     (e.g. `let config = { dependencies = [ "foo" ] } in config`). We won't know
--     until we have looked at what the "dependencies" key refers to before finding out that
--     the binding for the variable name `config` is what we need to update.
--
-- Since this is modifying the raw AST and might produce an invalid configuration file,
-- the returned expression should be verified to produce a valid configuration format.
modifyRawAST' :: HasLogFunc env => Text -> AstUpdate -> Expr -> RIO env Expr
modifyRawAST' initialKey astMod originalExpr = do
  result <- updateExpr (AtRootExpression initialKey) originalExpr
  case result of
    Just (Updated newExpr) -> do
      pure newExpr
    _ -> do
      pure originalExpr
  where
    -- |
    -- Adds the packages to the `ListLit`'s `Seq` argument
    updateListTextByAppending :: Seq Expr -> Seq Expr -> Seq Expr
    updateListTextByAppending additions dependencies = Seq.sort (additions <> dependencies)

    -- |
    -- Removes some boilerplate: changes `expr` to `expr # ["new"]`
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
    --    `./spago.dhall` (or some other expression where the required update is within the embed (e.g. `./spago.dhall // { sources = ["foo"] }`)
    -- to
    --    `let varname = ./spago.dhall in varName with dependencies = varName.dependencies # ["new"]`
    updateListTextByWrappingLetBinding :: NonEmpty Text -> Seq Expr -> Text -> Expr -> Expr
    updateListTextByWrappingLetBinding keyStack additions varName expr = do
      let
        var = Dhall.Var (Dhall.V varName 0)

        -- `let __embed = expr`
        binding = Dhall.makeBinding varName expr

        -- `__embed.key1.key2.key3
        varSelect = foldl' (\acc nextKey -> Dhall.Field acc (Dhall.makeFieldSelection nextKey)) var keyStack

        -- `__embed.dependencies # ["new"]`
        lsAppend =
          Dhall.ListAppend
            varSelect
            (Dhall.ListLit Nothing $ updateListTextByAppending additions Seq.empty)
      Dhall.Let binding $ Dhall.With var keyStack lsAppend

    -- | Updates a "root-level" expression
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
            mbResult <- fmap (mapUpdated (\newRecord -> Dhall.Field newRecord selection)) <$> updateExpr newLevel recordExpr
            case mbResult of
              Just EncounteredEmbed -> do
                case astMod of
                  InsertListText additions -> do
                    pure $ Just $ Updated $ Dhall.Field (updateListTextByWrappingLetBinding (key :| []) additions "__embed" expr) selection

                  SetText t -> do
                    pure $ Just $ Updated $ Dhall.Field (Dhall.With expr (key :| []) t) selection

              _ -> do
                -- Nothing, Just Updated, or Just VariableName
                pure mbResult

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
            mbRight <- updateExpr newLevel right
            case mbRight of
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

              varName@(Just (VariableName _)) -> do
                -- `The `Just VariableName` can't happen here because this is `AtRootExpression`
                pure varName

              Just (Updated newRight) -> do
                pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn left newRight

              Nothing -> do
                mbLeft <- updateExpr newLevel left
                case mbLeft of
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

                  varName@(Just (VariableName _)) -> do
                    -- `The `Just VariableName` can't happen here because this is `AtRootExpression`
                    pure varName

                  Just (Updated newLeft) -> do
                    pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn newLeft right

                  Nothing -> pure Nothing

          SearchingForField _ -> do
            -- See Dhall.Prefer's `AtRootExpression` case
            -- but for this situation, we're trying to find a record, so we don't match against a "dependencies" field
            mpRight <- updateExpr level right
            case mpRight of
              embedded@(Just EncounteredEmbed) -> do
                pure embedded

              varName@(Just (VariableName _)) -> do
                pure varName

              Just (Updated newRight) -> do
                pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn left newRight

              Nothing -> do
                mbLeft <- updateExpr level left
                case mbLeft of
                  embedded@(Just EncounteredEmbed) -> do
                    pure embedded

                  varName@(Just (VariableName _)) -> do
                    pure varName

                  Just (Updated newLeft) -> do
                    pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn newLeft right

                  Nothing -> do
                    pure Nothing

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
            mbResult <- updateExpr WithinField update
            case mbResult of
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

              varName@(Just (VariableName _)) -> do
                -- This can't happen because the Dhall expression is invalid. There can't be a variable name
                -- if there isn't a let binding.
                --    `{ ..., dependencies = ["old"] } with dependencies = x`
                pure varName

              Just (Updated newUpdate) -> do
                pure $ Just $ Updated $ Dhall.With recordExpr field newUpdate

              Nothing -> do
                -- This can't happen because the Dhall expression is invalid. If this is a Root level With, then
                -- recordExpr must be an expression that produces our Config schema, which means it will have
                -- a dependencies field. So, if we failed to update that dependencies field, then the expression
                -- itself is invalid.
                pure Nothing

          AtRootExpression key -> do
            let
              newLevel = SearchingForField (key :| [])
            mbResult <- updateExpr newLevel recordExpr
            case mbResult of
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

              varName@(Just (VariableName _)) -> do
                -- `The `Just VariableName` can't happen here because this is `AtRootExpression`
                pure varName

              Just (Updated newUpdate) -> do
                pure $ Just $ Updated $ Dhall.With recordExpr field newUpdate

              nothing@Nothing -> do
                pure nothing

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
                mbUpdate <- updateExpr levelForUpdateSearch update
                case mbUpdate of
                  embedded@(Just EncounteredEmbed) -> do
                    pure embedded

                  varName@(Just (VariableName _)) -> do
                    pure varName

                  Just (Updated newUpdate) -> do
                    pure $ Just $ Updated $ Dhall.With recordExpr field newUpdate

                  Nothing ->
                    updateRecordExpr

              Nothing -> do
                updateRecordExpr

            where
              updateRecordExpr = do
                mbRecordExpr <- updateExpr level recordExpr
                case mbRecordExpr of
                  embedded@(Just EncounteredEmbed) -> do
                    pure embedded

                  varName@(Just (VariableName _)) -> do
                    pure varName

                  Just (Updated newRecordExpr) -> do
                    pure $ Just $ Updated $ Dhall.With newRecordExpr field update

                  nothing@Nothing -> do
                    pure nothing

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
            result <- updateExpr newLevel inExpr
            case result of
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
                mbValue <- updateExpr newLevel value
                case mbValue of
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

                  varName@(Just (VariableName _)) -> do
                    -- invalid Dhall expression because this is AtRootExpression
                    pure varName

                  nothing@Nothing -> do
                    pure nothing

              varName@(Just (VariableName _)) -> do
                -- Invalid Dhall expression because this is AtRootExpression
                pure varName

              nothing@Nothing -> do
                pure nothing

          SearchingForField keyStack -> do
            result <- updateExpr level inExpr
            case result of
              embedded@(Just EncounteredEmbed) -> do
                pure embedded

              Just (Updated newInExpr) -> do
                pure $ Just $ Updated $ Dhall.Let binding newInExpr

              Just (VariableName (name, deBrujinIndex)) | name == variable, deBrujinIndex > 0 -> do
                pure $ Just $ VariableName (name, deBrujinIndex - 1)

              Just (VariableName (name, deBrujinIndex)) | name == variable && deBrujinIndex == 0 -> do
                mbValue <- updateExpr level value
                case mbValue of
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

                  varName@(Just (VariableName _)) -> do
                    -- `let x = "foo" let y = x in y`
                    -- This binding refers to another binding
                    pure varName

                  nothing@Nothing -> do
                    pure nothing

              varName@(Just (VariableName _)) -> do
                -- Variable name doesn't match this let binding's name
                pure varName

              nothing@Nothing -> do
                pure nothing

      _ -> do
        pure Nothing