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
import qualified Data.Text             as Text
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Parser          as Parser
import qualified Spago.Dhall           as Dhall


type Expr = Dhall.Expr Parser.Src Dhall.Import
type ResolvedExpr = Dhall.Expr Parser.Src Void

-- |
-- Indicates the change Spago wants to make to the Dhall expression
-- used to produce a 'Spago.Config.Config' value.
data ConfigModification
  = AddPackages ![PackageName]
  -- ^ Adds packages to the @dependencies@ field
  | UpdateName Text
  -- ^ Sets the @name@ field to the provided value
  | MigrateFromV1
  -- ^ Migrates from v1 config to the latest config.
  --
  -- v1 config only has the following type:
  -- @
  -- { name : Text
  -- , packages : packageMap
  -- , dependencies : List Text
  -- }
  -- @

-- |
-- Indicates the change to make once inside the Dhall expression,
-- regardless of what value(s) it produces.
data AstUpdate
  = InsertListText (Seq Expr)
  -- ^ Appends the given sequence of @TextLit@ values to the update location
  | SetText Text
  -- ^ Overwrites the update location with the given @TextLit@ value

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
-- For now, such an expression is anything that, when normalized, produces a
-- \"record expression\" whose
-- * @dependencies@ key contains a \"list expression\" of text that
--   corresponds to package names.
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
-- but @foo@ is already added, then we only need to add @bar@), and
-- 2. where to make the change in the AST.
--
-- Breaking this down into two problems makes the solving the second problem easier.
--
-- Second, by confirming below that the normalized expression found in the @spago.dhall@ file
-- IS a @RecordLit@ with the field we need to modify (e.g. it has a @dependencies@ field),
-- we can make some assumptions about the @Expr@ passed into `modifyRawDhallExpression`.
--
-- For example, we don't need to know what @Embed@ data constructor cases are because we can infer
-- based on where we are in the expression whether they are a Record expression that has
-- our desired field (e.g. the @dependencies@ field) or a List expression.
--
-- In other words, `modifyRawDhallExpression` can actually succeed for the cases we support.
modifyRawConfigExpression :: HasLogFunc env => ConfigModification -> ResolvedUnresolvedExpr -> RIO env Expr
modifyRawConfigExpression configMod ResolvedUnresolvedExpr { resolvedUnresolvedExpr = (normalizedExpr, originalExpr) } = case configMod of
  AddPackages newPackages -> do
    maybeAllInstalledPkgs <- findField dependenciesText $ findListText PackageName
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

  UpdateName newName -> do
    maybeName <- findField nameText (findText id)
    case maybeName of
      Just oldName | oldName /= newName -> do
          modifyRawDhallExpression sourcesText (SetText newName) originalExpr
      _ -> do
        pure originalExpr

  MigrateFromV1 -> do
    maybeSources <- findField sourcesText (const $ pure $ Just ())
    case maybeSources of
      Just{} -> do
        pure originalExpr
      Nothing -> do
        case originalExpr of
          Dhall.RecordLit kvs ->
            pure
              $ Dhall.RecordLit
              $ flip (Dhall.Map.insert sourcesText) kvs
              $ Dhall.makeRecordField listExpr

          _ ->
            pure
              $ Dhall.With originalExpr (sourcesText :| [])
              $ listExpr
        where
          listExpr =
            Dhall.ListLit Nothing
              $ Seq.fromList
              $ Dhall.toTextLit <$>
              [ "src/**/*.purs"
              , "test/**/*.purs"
              ]

  where
    findField :: forall a env. HasLogFunc env => Text -> (ResolvedExpr -> RIO env (Maybe a)) -> RIO env (Maybe a)
    findField key f = case normalizedExpr of
      Dhall.RecordLit kvs -> case Dhall.Map.lookup key kvs of
        Just Dhall.RecordField { recordFieldValue } -> do
          logDebug $ display $ "In normalized expression, found a field for key, '" <> key <> "'."
          f recordFieldValue
        _ -> do
          logDebug $ display $ "In normalized expression, did not find a field for key, '" <> key <> "'."
          pure Nothing
      _ -> do
        logDebug "In normalized expression, did not find a `RecordLit`."
        pure Nothing

    findListText :: forall a env. HasLogFunc env => (Text -> a) -> ResolvedExpr -> RIO env (Maybe (Seq a))
    findListText f = \case
      Dhall.ListLit _ dependencies -> do
        Just . fmap f <$> traverse (throws . Dhall.fromTextLit) dependencies
      _ -> do
        logDebug "In normalized expression, did not find a `ListLit`."
        pure Nothing

    findText :: forall a env. HasLogFunc env => (Text -> a) -> ResolvedExpr -> RIO env (Maybe a)
    findText f = \case
      Dhall.TextLit (Dhall.Chunks [] t) -> do
        pure $ Just $ f t
      _ -> do
        logDebug "In normalized expression, did not find a `ListLit`."
        pure Nothing

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
--
-- When the stack is empty, we are at the place in the expression
-- where we should do the update.
-- Otherwise, we are trying to find a record expression
-- that has the next key on the stack
--
-- Here's how the @levelKeyStack@ gets modified
--   - @Field@ pushes new keys on top of the stack
--   - @RecordLit@ pops off the key at the top of the stack
--   and looks up the field corresponding to it
--   - @With@ sometimes pops off one or more of the keys in
--   the @levelKeyStack@ when it can update that part of it.
newtype ExprLevel = ExprLevel { levelKeyStack :: [Text] }
  deriving (Eq, Show)

data UpdateResult
  = Updated !Expr
  -- ^ The expression was succesfully updated with the requested change.
  | VariableName !(Text, Int) ![Text]
  -- ^ The expression to update is the binding value with this name
  --   that corresponds to the specified de Brujin index.
  --   See "Dhall.Core#t:Var".
  --
  --  The @[Text]@ arg stores the @levelKeyStack@ at the time
  --  the variable that needs to be updated was found. This value
  --  should be used instead of whatever the keyStack is for a given level
  --  once the correct binding is found.

-- |
-- Basically 'Prelude.fmap' but only for the @Updated@ case.
mapUpdated :: (Expr -> Expr) -> UpdateResult -> UpdateResult
mapUpdated f (Updated e) = Updated (f e)
mapUpdated _ other = other

printUpdateResult :: UpdateResult -> Text
printUpdateResult = \case
  Updated _ ->
    "Updated <expr>"
  VariableName (name, idx) newKeyStack ->
    "VariableName " <> name <> " " <> Text.pack (show idx) <> " [" <> Text.intercalate ", " newKeyStack <> "]"

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
--     While we could update this using the same logic for @Embed@,
--     it is likely better to let the user manually update their file instead. If one is using
--     an advanced feature like this, there are likely reasons why that we can not anticipate here.
--     Thus, we won't be covering it and instead will force the user to update the file manually.
-- - BoolIf condition thenPath elsePath - @if x then y else z@
--     If the update is in either the @thenPath@ or the @elsePath@, which one do we update?
--     Without more context, we can not know and might update the value incorrectly.
--     Thus, we force the user to manually add the dependencies if this is used.
-- - Project expr (Right typeExpr) - @let P = { a : Text } in { a = \"1\", b = 2 }.(P)@
--     This only produces a record expresion. We don't support the @Right@ version of @Project@
--     because it requires traversing type-level constructors correctly. We will, however,
--     support its @Left@ version.
--
-- The below cases will be supported. Each is described below with a small description of how to update them:
-- - Embed _ - @./spago.dhall@
--     This imports and refers to another Dhall expression elsewhere.
--     As long as we have previously normalized the original expression
--     and verified that it will produce the \"shape\" we are expecting,
--     then when we encounter an @Embed@ constructor, we can make
--     one assumption about it: the type of the expression
--     must match what we are looking for in the current @ExprLevel@.
--
--     For example, if the @levelKeyStack@ is empty and we encounter an @Embed@ case
--     and we are trying to add packages to the @dependencies@ field, then we know
--     the import will produce an @List Text@-like expression. In such a case,
--     we can wrap it in a @ListAppend embedExpr newListLitExpr@.
--
--     If we the @levelKeyStack@ is not empty and we encounter an @Embed@ case, then we know
--     the import will produce a record expression. In such a case, we can wrap
--     it in a let binding and do the update with a @With@ expression that
--     appends the update to the part of the record we wish to update.
-- - ListLit - @[\"a\", \"literal\", \"list\", \"of\", \"values\"]@
--     When the `AstUpdate` is a @InsertListText additions@, updating this expression 
--     merely means adding the @additions@ to its list.
-- - ListAppend - @[\"list1\"] # [\"list2\"]@
--     Similar to @ListLit@ except it appends two list expressions together.
--     If it doesn't contain a @ListLit@ (e.g. @expr1 # expr2@), we can wrap the entire
--     expression in a @ListAppend originalExpr listExprWithNewPkgs@.
-- - RecordLit - @{ key = value }@
--     This is ultimately what we are looking for, so we can update the respective field.
--     However, due to supporting @Field@, which selects values within a record,
--     we need to support looking up keys besides the main key (e.g @dependencies@).
-- - Field recordExpr selection - @{ key = { dependencies = [\"bar\"] } }.key@
--     This can produce any expression since it is extracting the field within a record expression.
--     Let's say we were originally going to update a record expression's @depenencies@ field.
--     In such a case, we're looking for a record expression that has such a field. However,
--     when we come across a @Field@, it informs us that we must now find a record expression
--     that has a key matching the field being selected, and go "down" that key before
--     we can continue our original search (i.e. the record expression that has a @dependencies@ field),
-- - Project expr (Left keys) - @{ dependencies = [\"bar\"], other = \"foo\" }.{ dependencies }@
--     This produces a record expresion. We only need to update this expression if
--     one of its keys is the next key on the @levelKeyStack@ we want to update.
-- - Prefer recordExpr overrideRecordExpr - @{ dependencies = [\"foo\"] } \/\/ { dependencies = [\"bar\"] }@
--     This produces a record expression. If we update the @recordExpr@
--     arg and the field we are updating (e.g. @dependencies@) is overridden by @overrideRecordExpr@,
--     then the update is pointless.
--     If the @overrideRecordExpr@ value overrides something that we are not trying to update
--     (e.g. the @sources@ field when updating the @dependencies@ field),
--     then we need to update the @recordExpr@. So, we need to try to update the @overrideRecordExpr@ arg first
--     and only if that fails do we attempt to update the @recordExpr@.
-- - With recordExpr field update - @{ dependencies = [\"foo\"] } with dependencies = [\"bar\"]@
--     This produces a record expression. Similar to @Prefer@,
--     we should attempt to update the @update@ value first before attempting to update @recordExpr@.
--     Since a @With@ can use nested fields (e.g. @recordExpr with outer.inner.dependencies = update@),
--     we will see whether all of the values at the top of the @levelKeyStack@ match the nested fields
--     the @With@ is using to update part of the record expression. If it does, then we
--     try to update the @update@ expression. If that fails or if the match fails,
--     then we update the record expression as the update is updating something
--     irrelevant to our request.
-- - Let binding inExpr - @let binding = value in expr@
--     The expression we need to update will often be in the @inExpr@ (i.e. after the @in@ keyword;
--     > let src = [ "src" ] in { ..., dependencies = [ "old" ], sources = src }`)
--     However, we might need to update the expression associated with the bound variable name.
--     (e.g. @let config = { dependencies = [ \"foo\" ] } in config@). We will not know
--     where we need to update the value to which the binding refers until after we have
--     learned how it is used.
--
--     However, updating the value to which a binding refers can introduce a side-effect if
--     the binding is used in two or more places.
--     > let x = "foo" in { usage1 = x, usage2 = x }
--     If we update the value for @x@ with the intent of updating @usage1@, we will also update
--     @usage2@, which might be invalid. On the other hand, sometimes this side-effect
--     is actually intentional if one binding is building off of another. Consider:
--     > let x = "foo" let y = x # ["bar"] in { useX = x, useY = y }
--
--     Thus, for this implementation, we assume that we should update the binding's value
--     if the @levelKeyStack@ is not empty. Otherwise, we do the update in the @inExpr@.
--
-- Since this is modifying the raw AST and might produce an invalid configuration file,
-- the returned expression should be verified to produce a valid configuration format.
modifyRawDhallExpression :: HasLogFunc env => Text -> AstUpdate -> Expr -> RIO env Expr
modifyRawDhallExpression initialKey astMod originalExpr = do
  result <- updateExpr (ExprLevel { levelKeyStack = [initialKey] }) originalExpr
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

    -- |
    -- Tries to find a @ListLit@ in a potentially-nested @ListAppend@ expression
    -- and tries to append the additions to that @ListLit@
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

    debugCase level caseMsg =
    -- Change @logDebug@ to @logWarn@ to see results in tests
      logDebug $ "Level: " <> displayShow level <> " - " <> caseMsg

    debugResult level caseMsg maybeResult = do
      debugCase level (caseMsg <> " - got: " <> displayShow (fmap printUpdateResult maybeResult))
      pure maybeResult

    -- |
    -- Updates an expression by recursively updating any subexpressions
    -- until the update succeeds (@Just Updated@) or fails (@Nothing@). If called
    -- on a \"root-level\" expression, a @Just VariableName@ also
    -- counts as failure. For subexpressions, a returned @Just VariableName@ value may be used to determine how to
    -- update a subexpression found within the \"root-level\" expression.
    updateExpr :: HasLogFunc env => ExprLevel -> Expr -> RIO env (Maybe UpdateResult)
    updateExpr level@ExprLevel{..} expr = case expr of
      -- ./spago.dhall
      Dhall.Embed _ -> do
        debugCase level "Embed"
        case astMod of
          InsertListText additions -> do
            case levelKeyStack of
              --    `{ ..., dependencies = ./spago-deps.dhall }`
              -- to
              --    `{ ..., dependencies = ./spago-deps.dhall # additions }`
              [] -> do
                pure $ Just $ Updated $ updateListTextByWrappingListAppend additions expr

              --    `./spago.dhall`
              -- to
              --    `let __embed = ./spago.dhall in __embed with key = __embed.key # additions`
              (key:keys) -> do
                pure $ Just $ Updated $ updateListTextByWrappingLetBinding (key :| keys) additions "__embed" expr

          SetText t -> do
            pure $ Just $ Updated $ Dhall.toTextLit t

      -- let varname = ... in ... varName
      Dhall.Var (Dhall.V varName deBrujinIndex) -> do
        debugCase level $ "Var(varName =" <> displayShow varName <> ", deBrujin Index = " <> displayShow deBrujinIndex <> ")"
        case levelKeyStack of
          -- Since the @levelKeyStack@ is empty, we should do the update here rather than
          -- traversing back up those let bindings because we don't know if those
          -- let bindings are used in two or more places.
          --
          -- This means the update won't be as nice sylistically in some circumstances,
          -- but it does guarantee that modification is made safely. Otherwise,
          -- the let binding might be used elsewhere in a way we weren't anticipating.
          --
          -- For example:
          --    `let x = ["foo"] let y = x let z = y in z`
          -- to
          --    `let x = ["foo"] let y = x let z = y in z # ["new"]`
          [] -> do
            case astMod of
              InsertListText additions -> do
                pure $ Just $ Updated $ updateListTextByWrappingListAppend additions expr

              SetText t -> do
                pure $ Just $ Updated $ Dhall.toTextLit t

          -- Since, we still need to go "down" fields in various record expressions,
          -- we can't do the update here. For example, the second `config` in
          --    `let config = { ..., dependencies = ... } in config`
          _ -> do
            pure $ Just $ VariableName (varName, deBrujinIndex) levelKeyStack

      Dhall.TextLit (Dhall.Chunks chunks lastTxt) -> do
        debugCase level $ "TextLit( " <> displayShow (fold $ ((\(t,_) -> t <> "${expr}") <$> chunks) <> [lastTxt]) <> ")"
        case levelKeyStack of
          [] ->
            case astMod of
              SetText t -> do
                pure $ Just $ Updated $ Dhall.toTextLit t

              InsertListText{} -> do
                pure Nothing

          (_:_) -> do
            pure Nothing

      Dhall.TextAppend{} -> do
        debugCase level "TextAppend"
        case levelKeyStack of
          [] ->
            case astMod of
              SetText t -> do
                pure $ Just $ Updated $ Dhall.toTextLit t

              InsertListText{} -> do
                pure Nothing

          (_:_) -> do
            pure Nothing

      -- ["foo", "bar"]
      Dhall.ListLit ann ls -> do
        debugCase level $ "ListLit( ls =" <> displayShow ls <> ")"
        case levelKeyStack of
          (_:_) -> do
            -- This could happen if one was accessing part of a list via `List/head` or something.
            -- However, since we don't support functions yet, we don't need to do anything here.
            pure Nothing

          [] -> do
            case astMod of
              --  `{ ..., dependencies = ["old"] }
              -- to
              --  `{ ..., dependencies = ["old", "new"] }
              InsertListText additions -> do
                pure $ Just $ Updated $ Dhall.ListLit ann $ updateListTextByAppending additions ls

              -- Changing a `ListLit` value to a `TextLit` is invalid
              SetText{} -> do
                pure Nothing

      -- left # right
      Dhall.ListAppend left right -> do
        debugCase level $ "ListAppend( left = " <> displayShow left <> ", right = " <> displayShow right <> ")"
        case levelKeyStack of
          (_:_) -> do
            -- This could happen if one was accessing part of a list via `List/head` or something.
            -- However, since we don't support functions yet, we don't need to do anything here.
            pure Nothing

          [] -> do
            case astMod of
              --  `["foo"] # expr` -> `["old", "new"] # expr`
              --  `expr # ["old"]` -> `expr # ["old", "new"]`
              --  `expr1 # ["old"] # expr2` -> `expr1 # ["old", "new"] # expr2`
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
                      -- Since we couldn't add the update to an existing ListLit,
                      -- we'll just add it the the end
                      pure $ updateListTextByWrappingListAppend additions expr

              SetText{} -> do
                pure Nothing

      -- { key = value, ... }
      Dhall.RecordLit kvs -> do
        let
          caseMsg = "RecordLit( keys = " <> displayShow (Dhall.Map.keys kvs)
        debugCase level caseMsg
        case levelKeyStack of
          -- Invalid Dhall expression
          [] -> do
            pure Nothing

          (key:keys) ->
            case Dhall.Map.lookup key kvs of
              Nothing -> do
                pure Nothing
              Just Dhall.RecordField { recordFieldValue } -> do
                let
                  updateRecordLit =
                    Dhall.RecordLit
                      . flip (Dhall.Map.insert key) kvs
                      . Dhall.makeRecordField

                  newLevel = ExprLevel { levelKeyStack = keys }

                maybeResult <- fmap (mapUpdated updateRecordLit) <$> updateExpr newLevel recordFieldValue
                debugResult level caseMsg maybeResult

      -- recordExpr.selection
      Dhall.Field recordExpr selection@Dhall.FieldSelection { fieldSelectionLabel } -> do
        let caseMsg = "Field( fieldSelectionLabel = " <> displayShow fieldSelectionLabel <> ")"
        debugCase level caseMsg
        --  `{ config = { ..., dependencies = [ "package" ] } }.config`
        -- to
        --  `{ config = { ..., dependencies = [ "package", "new" ] } }.config`
        let
          newLevel = ExprLevel { levelKeyStack = fieldSelectionLabel : levelKeyStack }
        maybeResult <- updateExpr newLevel recordExpr
        debugResult level caseMsg maybeResult
        case maybeResult of
          Just (Updated newRecordExpr) -> do
            pure $ Just $ Updated $ Dhall.Field newRecordExpr selection

          Just VariableName{} -> do
            case levelKeyStack of
              -- Don't traverse back up the let binding because it will introduce a side-effect.
              -- For example, if we were updating the `dependencies` field...
              -- Given: `let x = ["foo"]        in { name = x, dependencies =  { useX = x }.useX,              ... }`
              -- Wrong: `let x = ["foo", "new"] in { name = x, dependencies =  { useX = x }.useX,              ... }`
              -- Right: `let x = ["foo"]        in { name = x, dependencies = ({ useX = x }.useX) # [ "new" ], ... }`
              [] -> do
                case astMod of
                  InsertListText additions -> do
                    pure $ Just $ Updated $ updateListTextByWrappingListAppend additions expr

                  SetText t -> do
                    pure $ Just $ Updated $ Dhall.toTextLit t

              -- We still need to lookup a key, so it's safe to update a previous let binding.
              _ -> do
                pure maybeResult

          Nothing -> do
            pure maybeResult

      -- { foo = "bar", baz = "2" }.{ foo } == { foo = "bar" }
      Dhall.Project recordExpr projectKeys@(Left keysExposed) -> do
        let caseMsg = "Project( Left keys = (" <> displayShow keysExposed <> ")"
        debugCase level caseMsg
        case levelKeyStack of
          -- We could only do an update here if we were replacing an entire record expression
          -- with a new one. We don't currently support that and likely never will.
          -- The below example shows how the `rec` value is replaced with a new record expresion.
          --    `{ a = "foo", rec = { value = "bar" } }`
          -- to
          --    `{ a = "foo", rec = { something = "else" } }`
          [] -> do
            pure Nothing

          -- We can only update the underlying record expression if
          -- one of the keys exposed by the projection is the next key we're trying to find.
          --    `({ config = { dependencies = ["old"], ... } }.{ config }).config`
          -- to
          --    `({ config = { dependencies = ["old", "new"], ... } }.{ config }).config`
          (key:_) | key `elem` keysExposed -> do
            maybeRecordExpr <- updateExpr level recordExpr
            void $ debugResult level caseMsg maybeRecordExpr
            case maybeRecordExpr of
              Just (Updated newRecordExpr) -> do
                pure $ Just $ Updated $ Dhall.Project newRecordExpr projectKeys

              Just VariableName{} -> do
                pure maybeRecordExpr

              Nothing -> do
                pure maybeRecordExpr

          -- None of the keys exposed by the projection are ones we're interestd in.
          -- For example, if we are trying to update the `dependencies` field,
          -- we would update the record expression on the left of the `Prefer` / `//`
          -- rather than the right record expression.
          --    `{ dependencies = ["old"] }        // { hiddenKey = "bar", exposedKey = ["foo" ] }.{ exposedKey }
          -- to
          --    `{ dependencies = ["old", "new"] } // { hiddenKey = "bar", exposedKey = ["foo" ] }.{ exposedKey }
          (_:_) -> do
            pure Nothing

      -- left // right
      Dhall.Prefer charSet preferAnn left right -> do
        let caseMsg = "Prefer"
        debugCase level caseMsg

        maybeRight <- updateExpr level right
        void $ debugResult level (caseMsg <> " - right") maybeRight
        case maybeRight of
          Just VariableName{} -> do
            pure maybeRight

          Just (Updated newRight) -> do
            pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn left newRight

          Nothing -> do
            maybeLeft <- updateExpr level left
            void $ debugResult level (caseMsg <> " - left") maybeLeft
            case maybeLeft of
              Just VariableName{} -> do
                pure maybeLeft

              Just (Updated newLeft) -> do
                pure $ Just $ Updated $ Dhall.Prefer charSet preferAnn newLeft right

              Nothing -> do
                pure maybeLeft

      -- recordExpr with field1.field2.field3 = update
      Dhall.With recordExpr field update -> do
        debugCase level caseMsg
        --    ```
        --    { outer =
        --       { config =
        --          { ..., dependencies = ["old"] }
        --       } with config.dependencies = ["package"]`
        --    }.outer.config
        --    ```
        --  to
        --    { outer =
        --       { config =
        --          { ..., dependencies = ["old"] }
        --       } with config.dependencies = ["package", "new"]`
        --    }.outer.config
        --    ```
        let
          levelForUpdate :: [Text] -> [Text] -> Maybe ExprLevel
          levelForUpdate (fieldKey:fieldKeys') (nextKey:nextKeys')
            | fieldKey == nextKey = levelForUpdate fieldKeys' nextKeys'
          levelForUpdate [] nextKeys' = Just $ ExprLevel { levelKeyStack = nextKeys' }
          levelForUpdate _ _ = Nothing

        case levelForUpdate (toList field) levelKeyStack of
          Just levelForUpdateSearch -> do
            maybeUpdate <- updateExpr levelForUpdateSearch update
            void $ debugResult level (caseMsg <> " - update") maybeUpdate
            case maybeUpdate of
              Just VariableName{} -> do
                pure maybeUpdate

              Just (Updated newUpdate) -> do
                pure $ Just $ Updated $ Dhall.With recordExpr field newUpdate

              Nothing ->
                updateRecordExpr

          Nothing -> do
            updateRecordExpr

        where
          caseMsg = "With( field = " <> displayShow field <> ")"

          updateRecordExpr = do
            maybeRecordExpr <- updateExpr level recordExpr
            void $ debugResult level (caseMsg <> " - recordExpr") maybeRecordExpr
            case maybeRecordExpr of
              Just VariableName{} -> do
                pure maybeRecordExpr

              Just (Updated newRecordExpr) -> do
                pure $ Just $ Updated $ Dhall.With newRecordExpr field update

              Nothing -> do
                pure maybeRecordExpr

      Dhall.Let binding@Dhall.Binding { variable, value } inExpr -> do
        debugCase level caseMsg
        maybeResult <- updateExpr level inExpr
        void $ debugResult level (caseMsg <> " - inExpr") maybeResult
        case maybeResult of
          Just (Updated newInExpr) -> do
            pure $ Just $ Updated $ Dhall.Let binding newInExpr

          -- We've learned that we need to update a let binding's value, but
          -- the variable below is referring to a binding with the same name,
          -- but which appeared earlier in the expression.
          -- So, we need to update that binding, not this one.
          --
          --    `let x = 1 let x = 2 in x   == 2`
          --    `let x = 1 let x = 2 in x@0 == 2`
          --    `let x = 1 let x = 2 in x@1 == 1`
          Just (VariableName (name, deBrujinIndex) newKeyStack) | name == variable, deBrujinIndex > 0 -> do
            if shouldStopUpdwardsTraversal then do
              debugUpwardsTraversalStop
              case astMod of
                --    `let lsBinding = ["old"] ... in { dependencies =  let x = lsBinding in x }`
                -- to
                --    `let lsBinding = ["old"] ... in { dependencies = (let x = lsBinding in x) # ["new"] }`
                InsertListText additions -> do
                  pure $ Just $ Updated $ updateListTextByWrappingListAppend additions expr

                SetText t -> do
                  pure $ Just $ Updated $ Dhall.toTextLit t
            else do
              debugUpwardsTraversalAllow
              pure $ Just $ VariableName (name, deBrujinIndex - 1) newKeyStack

          -- This expression is the binding whose value we need to update.
          Just (VariableName (name, deBrujinIndex) newKeyStack) | name == variable && deBrujinIndex == 0 -> do
            let valueLevel = ExprLevel { levelKeyStack = newKeyStack }
            maybeValue <- updateExpr valueLevel value
            void $ debugResult level (caseMsg <> " - value") maybeValue
            case maybeValue of
              Just (Updated newValue) -> do
                pure $ Just $ Updated $ Dhall.Let (Dhall.makeBinding variable newValue) inExpr

              Just VariableName{} -> do
                if shouldStopUpdwardsTraversal then do
                  debugUpwardsTraversalStop
                  case astMod of
                    --    `let lsBinding = ["old"] ... in { dependencies =  let x = lsBinding in x }`
                    -- to
                    --    `let lsBinding = ["old"] ... in { dependencies = (let x = lsBinding in x) # ["new"] }`
                    InsertListText additions -> do
                      pure $ Just $ Updated $ Dhall.Let (Dhall.makeBinding variable
                        $ updateListTextByWrappingListAppend additions value) inExpr

                    SetText t -> do
                      pure $ Just $ Updated $ Dhall.toTextLit t

                else do
                  debugUpwardsTraversalAllow
                  -- `let x = "foo" let y = x in y`
                  -- This binding refers to another binding
                  pure maybeValue

              Nothing -> do
                pure maybeValue

          Just VariableName{} -> do
            -- Variable name doesn't match this let binding's name
            pure maybeResult

          Nothing -> do
            pure maybeResult
        where
          caseMsg = "Let( variable = " <> displayShow variable <> ")"
          debugUpwardsTraversalStop = debugCase level (caseMsg <> " - stopping upwards traversal and updating here")
          debugUpwardsTraversalAllow = debugCase level (caseMsg <> " - allowing upwards traversal")

          -- Normally, we would continue traversing up the expression and update the
          -- referenced variable. However, since the @levelKeyStack@ is empty, we are currently
          -- at the location within the expression that we want to update.
          -- If we leave the current expression and go back "up" to its parent expression,
          -- we might update a let binding's value in such a way that it produces a side-effect.
          -- If the let binding is used in two places, then we only want to update it for one
          -- usage but not the other. To ensure that happens, we'll stop here and make the update.
          --
          -- It would be safe to update the let binding if we confirmed that it's only being used
          -- in a single place, but such a check would require traversing the entire expression.
          shouldStopUpdwardsTraversal = null levelKeyStack

      _ -> do
        debugCase level "Unsupported case"
        pure Nothing
