{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Spago.Config.AST
  ( addRawDeps
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Data.List             as List
import qualified Data.List.NonEmpty    as NonEmpty
import qualified Data.Map              as Map
import qualified Data.SemVer           as SemVer
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Data.Versions         as Version
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.TypeCheck
import qualified Dhall.Parser          as Parser
import qualified Web.Bower.PackageMeta as Bower

import qualified Spago.Dhall           as Dhall
import qualified Spago.Messages        as Messages


type Expr = Dhall.Expr Parser.Src Dhall.Import

addRawDeps :: HasLogFunc env => Config -> [PackageName] -> Expr -> RIO env Expr
addRawDeps Config { packageSet = PackageSet{..} } newPackages originalExpr =
  case NonEmpty.nonEmpty notInPackageSet of
    Just pkgsNotInPackageSet -> do
      logWarn $ display $ Messages.failedToAddDeps $ NonEmpty.map packageName pkgsNotInPackageSet
      pure originalExpr
    Nothing -> do
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
    notInPackageSet = filter (\p -> Map.notMember p packagesDB) newPackages

    findInstalledPackages :: HasLogFunc env => RIO env (Maybe (Seq PackageName))
    findInstalledPackages = case Dhall.normalize originalExpr of
      Dhall.RecordLit kvs -> case Dhall.Map.lookup "dependencies" kvs of
        Just Dhall.RecordField { recordFieldValue } -> case recordFieldValue of
          Dhall.ListLit _ dependencies -> do
            Just . fmap PackageName <$> traverse (throws . Dhall.fromTextLit) dependencies
          other -> do
            logWarn $ display $ failedToAddDepsExpectedRecordKey other
            pure Nothing
        _ -> do
          logWarn "Failed to add dependencies. You should have a record with the `dependencies` key for this to work."
          pure Nothing
      other -> do
        logWarn $ display $ failedToAddDepsExpectedRecordKey other
        pure Nothing
      where
        failedToAddDepsExpectedRecordKey e =
          "Failed to add dependencies. You should have a record with the `dependencies` key for this to work.\n" <>
          "Expression was: " <> pretty e

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

    -- | Updates a "root-level" expression
    updateRootLevelExpr :: Expr -> RIO env (Maybe Expr)
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

            fmap updateRecordLit <$> updateLeafExpr recordFieldValue

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
          Nothing -> do
            fmap (\newLeft -> Dhall.Prefer charSet preferAnn newLeft right) <$> updateRootLevelExpr left

      -- recordExpr with field1.field2.field3 = update
      Dhall.With recordExpr field update
        | field == "dependencies" :| [] -> do
            fmap (\newUpdate -> Dhall.With recordExpr field newUpdate) <$> updateLeafExpr update
        | otherwise -> do
            fmap (\newRecordExpr -> Dhall.With newRecordExpr field update) <$> updateRootLevelExpr recordExpr

      _ -> do
        pure Nothing

    -- |
    -- Handles the update that installs the packages to the correct location
    -- within the Dhall AST
    --
    -- Is pretty much the same as `updateRootLevelExpr` but
    -- doesn't have a `RecordLit` case.
    updateLeafExpr :: Expr -> RIO env (Maybe Expr)
    updateLeafExpr expr = case expr of
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
        fmap (\newFieldExpr -> Dhall.Field newFieldExpr selection) <$> updateLeafField fieldExpr fieldSelectionLabel

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

    updateLeafField :: Expr -> Text -> RIO env (Maybe Expr)
    updateLeafField field key = case field of
      Dhall.RecordLit kvs -> 
        case Dhall.Map.lookup key kvs of
          Just Dhall.RecordField { recordFieldValue } -> do
            let
              rewrapResult =
                Dhall.RecordLit
                  . flip (Dhall.Map.insert key) kvs
                  . Dhall.makeRecordField
            fmap rewrapResult <$> updateLeafExpr recordFieldValue

          _ -> pure Nothing

      _ -> pure Nothing

-- | Code from https://stackoverflow.com/questions/45757839
nubSeq :: Ord a => Seq a -> Seq a
nubSeq xs = (fmap fst . Seq.filter (uncurry notElem)) (Seq.zip xs seens)
  where
    seens = Seq.scanl (flip Set.insert) Set.empty xs

