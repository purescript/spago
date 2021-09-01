{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Spago.Config.AST
  ( addRawDeps
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Data.Map              as Map
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Parser          as Parser
import qualified Spago.Dhall           as Dhall


type Expr = Dhall.Expr Parser.Src Dhall.Import
type ResolvedExpr = Dhall.Expr Parser.Src Void

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
-- the `Expr` passed into `addRawDeps'` and avoid handling some cases. In other words,
-- `addRawDeps'` can actually succeed for the cases we support.
addRawDeps :: HasLogFunc env => [PackageName] -> ResolvedExpr -> Expr -> RIO env Expr
addRawDeps newPackages normalizedExpr originalExpr = do
  mbAllInstalledPkgs <- findInstalledPackages
  case mbAllInstalledPkgs of
    Nothing -> do
      pure originalExpr
    Just allInstalledPkgs -> do
      let pkgsToInstall = nubSeq $ Seq.filter (`notElem` allInstalledPkgs) $ Seq.fromList newPackages
      if null pkgsToInstall
      then do
        pure originalExpr
      else do
        addRawDeps' (Dhall.toTextLit . packageName <$> pkgsToInstall) originalExpr
  where
    findInstalledPackages :: HasLogFunc env => RIO env (Maybe (Seq PackageName))
    findInstalledPackages = case normalizedExpr of
      Dhall.RecordLit kvs -> case Dhall.Map.lookup "dependencies" kvs of
        Just Dhall.RecordField { recordFieldValue } -> case recordFieldValue of
          Dhall.ListLit _ dependencies -> do
            Just . fmap PackageName <$> traverse (throws . Dhall.fromTextLit) dependencies
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
          "Failed to add dependencies. You should have a record with the `dependencies` key for this to work.\n" <>
          "Expression was: " <> pretty e

-- |
-- A Configuration's Dhall expression is anything that, when normalized, produces a
-- "record expression" whose `dependencies` key contains a "list expression" of text that
-- corresponds to package names.
--
-- Below is an explanation of terms used in the code below. In short, is our current position
-- within the Dhall expression "before" or "after" the "dependencies" key and are we inside
-- or outside a "let binding" expression?
-- - Root Level = the outermost/topmost expression(s) where we haven't
--     discovered the "record expression" containing the "dependencies" key
--     and haven't discovered a "let binding" expression.
-- - Leaf Level = we found the "record expression" containing the "dependencies" key
--     and are now within it. There are no "let binding" expressions that may force us
--     to travere back "out" of the expression to do the update.
-- - Recursive Level = we found a "let binding" expression before discovering
--     the "record expression" with the "dependencies" key. Once in this level, if we
--     later enter the Leaf Level, we may have to back "out" of the Leaf Level
--     to find the correct expression to update. Once found, we may enter
--     the correct expression as though it was a Root Level and may need to back "out"
--     again in case its expression refers to another even earlier binding.
--
-- To make this implementation cover most of the usual cases while still making this simple,
-- the following cases will NOT be supported at both the root and leaf levels:
-- - Lam binding expr - `λ(binding : Text) -> { dependencies = [ x ] }`
--     Although a `Lam` can't be a root-level expression, it could still appear in various places.
--     We could try to update the function's body, but without greater context, it's possible
--     that updating the body could affect other things, too. If someone is using something as
--     complicated as lambdas, we'll force them to update the file manually.
-- - App func arg - `(λ(x : A) -> { dependencies = [ x ] }) "bar"`
--     This can produce a record expression and unlike `Lam` can be a root-level expression.
--     However, this is a complex feature and can be difficult to update correctly like `Lam`.
--     Thus, we won't be covering it and instead will force the user to update the file manually.
-- - Project expr keys - `{ dependencies = ["bar"], other = "foo" }.{ dependencies }`
--     This can produce a record expresion. Since this is unlikely to be used frequently,
--     and requires a bit more work due to the type for `keys`, we won't support it below.
--
-- The below cases will be supported. Each is described below with a small description of how to update them:
-- - RecordLit - `{ dependencies = ["bar"] }`
--     This is ultimately what we're looking for, so we can update the `dependencies`' field's list
-- - Prefer recordExpr overrides - `{ dependencies = ["foo"] } // { dependencies = ["bar"] }`
--     This produces a record expression. If we update the `recordExpr`
--     arg and its `dependencies` is overridden by `overrides`, then the update is pointless.
--     If the `overrides` value overrides something irrelevant to the record's `dependencies` field,
--     then we need to update the `recordExpr`. So, we need to try to update the `overrides` arg first
--     and only if that fails do we attempt to update the `recordExpr`.
-- - With recordExpr field update - `{ dependencies = ["foo"] } with dependencies = ["bar"]`
--     This produces a record expression. Similar to `Prefer`,
--     we should attempt to update the `update` value first before attempting to update `recordExpr`.
-- - Field recordExpr selection - `{ key = { dependencies = ["bar"] } }.key`
--     This can produce a record expression. If the expression can be updated, then we try to update it.
-- - Let binding expr - `let binding = value in expr`
--     This expression is what makes implementing this update difficult. The record
--     will most likely be in `expr` (e.g. `let pkg = "foo" in { dependencies = [ pkg ] }`),
--     so updating it should be straight forward. However, it can also exist in the binding
--     (e.g. `let config = { dependencies = [ "foo" ] } in config`). We won't know
--     until we have reached the terminating record expression before finding out that
--     the binding for the variable name `config` is what we need to update.
--
-- When attempting to update an expression not in a "let binding" expression,
-- we typically return `Maybe Expr`:
-- - Nothing = the attempted update failed
-- - Just = the attempted update succeeded
--
-- When attempting to update an expression within a "let binding" expression,
-- we return `Maybe (Either Text expr)`:
-- - Nothing = the attempted update failed
-- - Just Right = the attempted update succeeded
-- - Just Left = the attempted update should succeed, but we need to update the binding, not the expression.
addRawDeps' :: HasLogFunc env => Seq Expr -> Expr -> RIO env Expr
addRawDeps' pkgsToInstall originalExpr = do
  result <- updateRootLevelExpr originalExpr
  case result of
    Just newExpr -> do
      pure newExpr
    Nothing -> do
      logWarn "Failed to add dependencies."
      pure originalExpr
  where
    -- |
    -- Adds the packages to the `ListLit`'s `Seq` argument
    updateDependencies :: Seq Expr -> Seq Expr
    updateDependencies dependencies = Seq.sort (pkgsToInstall <> dependencies)

    isEmbed :: Expr -> Bool
    isEmbed = \case
      Dhall.Embed _ -> True
      _ -> False

    -- | Updates a "root-level" expression
    updateRootLevelExpr :: HasLogFunc env => Expr -> RIO env (Maybe Expr)
    updateRootLevelExpr expr = case expr of
      -- { key = value, ... }
      Dhall.RecordLit kvs -> do
        case Dhall.Map.lookup "dependencies" kvs of
          Nothing -> do
            pure Nothing
          Just Dhall.RecordField { recordFieldValue } -> do
            let
              updateRecordLit =
                Dhall.RecordLit
                  . flip (Dhall.Map.insert "dependencies") kvs
                  . Dhall.makeRecordField

            fmap updateRecordLit <$> updateLeafExpr Map.empty recordFieldValue

      -- recordExpr.selection
      Dhall.Field fieldExpr selection@Dhall.FieldSelection { fieldSelectionLabel } -> do
        case fieldExpr of
          Dhall.RecordLit kvs -> 
            case Dhall.Map.lookup fieldSelectionLabel kvs of
              Just Dhall.RecordField { recordFieldValue } -> do
                let
                  rewrapInRecordLit =
                    Dhall.RecordLit
                      . flip (Dhall.Map.insert fieldSelectionLabel) kvs
                      . Dhall.makeRecordField
                fmap ((\x -> Dhall.Field x selection) . rewrapInRecordLit) <$> updateRootLevelExpr recordFieldValue
              Nothing -> do
                pure Nothing
          _ -> do
            pure Nothing

      -- left // right
      Dhall.Prefer charSet preferAnn left right -> do
        mbRight <- updateRootLevelExpr right
        case mbRight of
          Just newRight -> pure $ Just $ Dhall.Prefer charSet preferAnn left newRight
          Nothing
            | isEmbed left -> do
                --    ./spago.dhall // recordExpr
                -- becomes
                --    let __embed = .spago.dhall
                --    in __embed // recordExpr // { dependencies = __embed.dependencies # <pkgsToInstall> }
                let
                  varName = "__embed"
                  var = Dhall.Var (Dhall.V varName 0)
                pure $ Just $ Dhall.Let (Dhall.makeBinding varName left)
                  (Dhall.Prefer charSet preferAnn  var
                    $ Dhall.Prefer charSet Dhall.PreferFromSource right
                    $ Dhall.RecordLit
                    $ Dhall.Map.singleton "dependencies"
                    $ Dhall.makeRecordField
                    $ Dhall.ListAppend
                        (Dhall.Field var (Dhall.makeFieldSelection "dependencies"))
                        (Dhall.ListLit Nothing $ updateDependencies Seq.empty)
                    )
            | otherwise -> do
                fmap (\newLeft -> Dhall.Prefer charSet preferAnn newLeft right) <$> updateRootLevelExpr left

      -- recordExpr with field1.field2.field3 = update
      Dhall.With recordExpr field update
        | field == "dependencies" :| [] -> do
            fmap (\newUpdate -> Dhall.With recordExpr field newUpdate) <$> updateLeafExpr Map.empty update
        | isEmbed recordExpr -> do
            --    ./spago.dhall with sources = [ "bar" ]
                -- becomes
                --    let __embed = .spago.dhall
                --    in __embed with sources = [ "bar"] with dependencies = __embed.dependencies # <pkgsToInstall>
                let
                  varName = "__embed"
                  var = Dhall.Var (Dhall.V varName 0)
                pure $ Just $ Dhall.Let (Dhall.makeBinding varName recordExpr)
                  (Dhall.With 
                    (Dhall.With var field update)
                    ("dependencies" :| [])
                    $ Dhall.RecordLit
                    $ Dhall.Map.singleton "dependencies"
                    $ Dhall.makeRecordField
                    $ Dhall.ListAppend
                        (Dhall.Field var (Dhall.makeFieldSelection "dependencies"))
                        (Dhall.ListLit Nothing $ updateDependencies Seq.empty)
                    )
        | otherwise -> do
            fmap (\newRecordExpr -> Dhall.With newRecordExpr field update) <$> updateRootLevelExpr recordExpr

      Dhall.Let binding@Dhall.Binding { variable, value } inExpr -> do
        result <- updateRecursiveExpr (Map.singleton variable value) inExpr
        case result of
          Nothing -> pure Nothing
          Just update -> case update of
            Right newInExpr -> pure $ Just $ Dhall.Let binding newInExpr
            Left varName
              | varName == variable -> do
                  fmap (\newValue -> Dhall.Let (Dhall.makeBinding variable newValue) inExpr) <$> updateRootLevelExpr value
              | otherwise -> do
                  pure Nothing

      other -> do
        logWarn $ display $ Text.intercalate "\n"
          [ "Failed to add dependencies."
          , ""
          , "Expected a top-level expression that produces a record with a `dependencies` key."
          , ""
          , "Valid top-level expressions are:"
          , " - RecordLit - `{ key = value, ... }`"
          , " - Let - `let binding = value in expr`"
          , " - Prefer - `expr1 // expr2`"
          , " - With - `expr1 with expr2`"
          , " - Field - `expr.value`"
          , ""
          , "Invalid top-level expressions include but are not limited to: "
          , " - Lam - λ(x : A) -> x (doesn't produce a Record-like expression)"
          , " - App - `(λ(x : A) -> x) 4` (it's safer to let the user handle these unique cases)"
          , " - Project - `{ key1 = value1, key2 = value2}.{ key }` (less frequently used)"
          , ""
          , "Top-level expression was: " <> pretty other
          ]
        pure Nothing

    -- |
    -- Same as `updateRootExpr` but
    -- - has a `Dhall.Var` case
    -- - includes a `bindingsMap` argument
    -- - uses `updateLeafLetBinding` rather than `updateLeafExpr`
    --     so that we return `Maybe (Either Text Expr)` rather than `Maybe Expr`
    updateRecursiveExpr :: Map Text Expr -> Expr -> RIO env (Maybe (Either Text Expr))
    updateRecursiveExpr bindingsMap expr = case expr of
      Dhall.Var (Dhall.V varName _) -> do
        pure $ Just $ Left varName

      -- { key = value, ... }
      Dhall.RecordLit kvs -> do
        case Dhall.Map.lookup "dependencies" kvs of
          Nothing -> do
            pure Nothing
          Just Dhall.RecordField { recordFieldValue } -> do
            let
              updateRecordLit =
                Dhall.RecordLit
                  . flip (Dhall.Map.insert "dependencies") kvs
                  . Dhall.makeRecordField

            fmap (fmap updateRecordLit) <$> updateLeafLetBinding bindingsMap recordFieldValue

      Dhall.Field fieldExpr selection@Dhall.FieldSelection { fieldSelectionLabel } -> do
        case fieldExpr of
          Dhall.RecordLit kvs ->
            case Dhall.Map.lookup fieldSelectionLabel kvs of
              Just Dhall.RecordField { recordFieldValue } -> do
                let
                  rewrapInRecordLit =
                    Dhall.RecordLit
                      . flip (Dhall.Map.insert fieldSelectionLabel) kvs
                      . Dhall.makeRecordField
                fmap (fmap ((\x -> Dhall.Field x selection) . rewrapInRecordLit)) <$> updateRecursiveExpr bindingsMap recordFieldValue
              Nothing -> do
                pure Nothing
          _ -> do
            pure Nothing

      Dhall.Prefer charSet preferAnn left right -> do
        mbRight <- updateRecursiveExpr bindingsMap right
        case mbRight of
          Just (Right newRight) -> do
            pure $ Just $ Right $ Dhall.Prefer charSet preferAnn left newRight
          Just (Left _) -> do
            pure Nothing
          Nothing -> do
            fmap (fmap (\newLeft -> Dhall.Prefer charSet preferAnn newLeft right)) <$> updateRecursiveExpr bindingsMap left

      Dhall.With recordExpr field update
        | field == "dependencies" :| [] -> do
            -- fmap (\newUpdate -> Dhall.With recordExpr field newUpdate) <$> updateLeafExpr Map.empty update
            pure Nothing
        | otherwise -> do
            fmap (fmap (\newRecordExpr -> Dhall.With newRecordExpr field update)) <$> updateRecursiveExpr bindingsMap recordExpr

      Dhall.Let binding@Dhall.Binding { variable, value } inExpr -> do
        result <- updateRecursiveExpr (Map.insert variable value bindingsMap) inExpr
        case result of
          Nothing -> pure Nothing
          Just update -> case update of
            Right newInExpr -> pure $ Just $ Right $ Dhall.Let binding newInExpr
            l@(Left varName)
              | varName == variable -> do
                  fmap (\newValue -> Right $ Dhall.Let (Dhall.makeBinding variable newValue) inExpr) <$> updateLeafExpr bindingsMap value
              | otherwise -> do
                  pure $ Just l

      _ -> pure Nothing

    -- |
    -- Handles the update that installs the packages to the correct location
    -- within the Dhall AST
    --
    -- Is pretty much the same as `updateRootLevelExpr` but
    -- doesn't have a `RecordLit` case.
    updateLeafExpr :: Map Text Expr -> Expr -> RIO env (Maybe Expr)
    updateLeafExpr bindingsMap expr = case expr of
      Dhall.ListLit ty dependencies -> do
        pure $ Just $ Dhall.ListLit ty $ updateDependencies dependencies

      Dhall.ListAppend l r -> do
        let
          result = (\left -> Dhall.ListAppend left r) <$> updateListAppend l
            <|> (\right -> Dhall.ListAppend l right) <$> updateListAppend r

        pure $ case result of
          Just listAppend ->
            Just listAppend
          Nothing ->
            Just
            $ Dhall.ListAppend l
            $ Dhall.ListAppend r
            $ Dhall.ListLit Nothing $ updateDependencies Seq.empty

      Dhall.Field fieldExpr selection@Dhall.FieldSelection { fieldSelectionLabel } -> do
        case fieldExpr of
          Dhall.RecordLit kvs ->
            case Dhall.Map.lookup fieldSelectionLabel kvs of
              Just Dhall.RecordField { recordFieldValue } -> do
                let
                  rewrapResult =
                    Dhall.RecordLit
                      . flip (Dhall.Map.insert fieldSelectionLabel) kvs
                      . Dhall.makeRecordField
                fmap (\newFieldExpr -> Dhall.Field (rewrapResult newFieldExpr) selection) <$> updateLeafExpr bindingsMap recordFieldValue

              _ -> pure Nothing

          _ -> pure Nothing

      Dhall.Let binding@Dhall.Binding { variable, value } inExpr -> do
        result <- updateLeafLetBinding (Map.insert variable value bindingsMap) inExpr
        case result of
          Nothing -> pure Nothing
          Just update -> case update of
            Right newInExpr -> do
              pure $ Just $ Dhall.Let binding newInExpr
            Left varName
              | varName == variable -> do
                  updateLeafExpr bindingsMap value
              | otherwise -> do
                  pure Nothing

      _ -> do
        pure Nothing

    -- |
    -- Updates the first `LisLit` found, traversing nested `ListAppend`s.
    updateListAppend :: Expr -> Maybe Expr
    updateListAppend = \case
      Dhall.ListLit _ dependencies -> do
        Just . Dhall.ListLit Nothing $ updateDependencies dependencies
      Dhall.ListAppend left right -> do
        (\l -> Dhall.ListAppend l right) <$> updateListAppend left
          <|> (Dhall.ListAppend left) <$> updateListAppend right
      _ -> Nothing

    -- |
    -- `Nothing` = update could not be done at all
    -- `Just Right` = update could be done in `inExpr`
    -- `Just Left` = update should be done in binding's `value`.
    updateLeafLetBinding :: Map Text Expr -> Expr -> RIO env (Maybe (Either Text Expr))
    updateLeafLetBinding bindingsMap expr = case expr of
      Dhall.Var (Dhall.V varName _) -> do
        pure $ Just $ Left varName

      Dhall.ListLit ty dependencies -> do
        pure $ Just $ Right $ Dhall.ListLit ty $ updateDependencies dependencies

      Dhall.ListAppend l r -> do
        let
          res = (\left -> Dhall.ListAppend left r) <$> updateListAppend l
            <|> (\right -> Dhall.ListAppend l right) <$> updateListAppend r
          listAppend = case res of
            Just ls -> ls
            Nothing -> do
              Dhall.ListAppend l
                $ Dhall.ListAppend r
                $ Dhall.ListLit Nothing
                $ updateDependencies Seq.empty
        pure $ Just $ Right listAppend

      Dhall.Field fieldExpr selection@Dhall.FieldSelection { fieldSelectionLabel } -> do
        case fieldExpr of
          Dhall.RecordLit kvs ->
            case Dhall.Map.lookup fieldSelectionLabel kvs of
              Just Dhall.RecordField { recordFieldValue } -> do
                let
                  rewrapResult =
                    Dhall.RecordLit
                      . flip (Dhall.Map.insert fieldSelectionLabel) kvs
                      . Dhall.makeRecordField
                fmap (fmap (\newFieldExpr -> Dhall.Field (rewrapResult newFieldExpr) selection)) <$> updateLeafLetBinding bindingsMap recordFieldValue

              _ -> pure Nothing

          _ -> pure Nothing

      Dhall.Let binding@Dhall.Binding { variable, value } inExpr -> do
        result <- updateLeafLetBinding (Map.insert variable value bindingsMap) inExpr
        case result of
          Nothing -> pure Nothing
          Just update -> case update of
            Right newInExpr -> do
              pure $ Just $ Right $ Dhall.Let binding newInExpr
            l@(Left varName)
              | varName == variable -> do
                  fmap (\newValue -> Right $ Dhall.Let (Dhall.makeBinding variable newValue) inExpr) <$> updateLeafExpr bindingsMap value
              | otherwise -> do
                  pure $ Just l

      _ -> do
        pure Nothing

-- | Code from https://stackoverflow.com/questions/45757839
nubSeq :: Ord a => Seq a -> Seq a
nubSeq xs = (fmap fst . Seq.filter (uncurry notElem)) (Seq.zip xs seens)
  where
    seens = Seq.scanl (flip Set.insert) Set.empty xs
