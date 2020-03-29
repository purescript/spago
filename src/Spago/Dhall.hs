{-# LANGUAGE GADTs #-}
module Spago.Dhall
  ( module Spago.Dhall
  , module Dhall
  ) where

import           Spago.Prelude

import qualified Control.Monad.Trans.State.Strict      as State
import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import           Dhall
import           Dhall.Core                            as Dhall hiding (pretty)
import qualified Dhall.Format
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser                          as Parser
import qualified Dhall.Pretty
import           Dhall.TypeCheck                       (typeOf)
import           Dhall.Util                            as Dhall
import qualified Lens.Family
import qualified System.FilePath                       as FilePath

type DhallExpr a = Dhall.Expr Parser.Src a


-- | Format a Dhall file in ASCII
--   We first check if it's already formatted, if not we reformat it.
format :: MonadIO m => Text -> m ()
format pathText = liftIO $
  try (f Dhall.Check) >>= \case
    Left (_e :: SomeException) ->
      f Dhall.Write
    Right _ -> pure ()
  where
    f = Dhall.Format.format . Dhall.Format.Format Dhall.Pretty.ASCII Dhall.NoCensor path
    path = Dhall.InputFile $ Text.unpack pathText


-- | Prettyprint a Dhall expression adding a comment on top
prettyWithHeader :: Pretty.Pretty a => Dhall.Header -> DhallExpr a -> Dhall.Text
prettyWithHeader (Header header) expr = do
  let doc = Pretty.pretty header <> Pretty.pretty expr
  PrettyText.renderStrict $ Pretty.layoutSmart Pretty.defaultLayoutOptions doc

data TemplateComments = WithComments | NoComments

processComments :: TemplateComments -> Text -> Text
processComments WithComments = id
processComments NoComments   = stripComments

stripComments :: Text -> Text
stripComments dhallSrc =
  -- This is a hack taking advantage of current Dhall's parser behavior which does not preserve comments
  -- This impl might need to be revisited after https://github.com/dhall-lang/dhall-haskell/issues/145 is fixed
  case Parser.exprFromText mempty dhallSrc of
    Left _     -> dhallSrc
    Right expr -> pretty expr


-- | Return a list of all imports starting from a particular file
readImports :: Text -> IO [Dhall.Import]
readImports pathText = do
  fileContents <- readTextFile $ pathFromText pathText
  expr <- throws $ Parser.exprFromText mempty fileContents
  status <- load expr
  let graph = Lens.Family.view Dhall.Import.graph status
  pure $ childImport <$> graph
  where
    load expr'
      = State.execStateT
          (Dhall.Import.loadWith expr')
          (Dhall.Import.emptyStatus (FilePath.takeDirectory $ Text.unpack pathText))

    childImport
      = Dhall.Import.chainedImport . Dhall.Import.child



readRawExpr :: Text -> IO (Maybe (Dhall.Header, DhallExpr Dhall.Import))
readRawExpr pathText = do
  exists <- testfile pathText
  if exists
    then (do
      packageSetText <- readTextFile $ pathFromText pathText
      fmap Just $ throws $ Parser.exprAndHeaderFromText mempty packageSetText)
    else pure Nothing


writeRawExpr :: Text -> (Dhall.Header, DhallExpr Dhall.Import) -> IO ()
writeRawExpr pathText (header, expr) = do
  -- After modifying the expression, we have to check if it still typechecks
  -- if it doesn't we don't write to file.
  resolvedExpr <- Dhall.Import.load expr
  _ <- throws (Dhall.TypeCheck.typeOf resolvedExpr)
  writeTextFile pathText $ prettyWithHeader header expr
  format pathText


-- | Returns a Dhall Text literal from a lone string
toTextLit :: Pretty a => Text -> DhallExpr a
toTextLit str = Dhall.TextLit (Dhall.Chunks [] str)


-- | Casts a Dhall Text literal to a string, or fails
fromTextLit
  :: (Pretty a, Typeable a)
  => DhallExpr a
  -> Either (ReadError a) Text
fromTextLit(Dhall.TextLit (Dhall.Chunks [] str)) = Right str
fromTextLit expr                                 = Left $ ExprIsNotTextLit expr


-- | Require a key from a Dhall.Map, and run an action on it if found.
--   If not found, return the name of the key.
requireKey
  :: (Typeable b, Pretty b, MonadIO m, MonadThrow m)
  => Dhall.Map.Map Text (DhallExpr b)
  -> Text
  -> (DhallExpr b -> m a)
  -> m a
requireKey ks name f = case Dhall.Map.lookup name ks of
  Just v  -> f v
  Nothing -> throwM (RequiredKeyMissing name ks)


-- | Same as `requireKey`, but we give it a Dhall.Decoder to automagically decode from
requireTypedKey
  :: (MonadIO m, MonadThrow m)
  => Dhall.Map.Map Text (DhallExpr Void)
  -> Text
  -> Dhall.Decoder a
  -> m a
requireTypedKey ks name typ = requireKey ks name $ \expr -> case Dhall.extract typ expr of
  Success v -> pure v
  Failure _ -> throwM $ RequiredKeyMissing name ks

-- | Try to find a key from a Dhall.Map, and automagically decode the value with the given Dhall.Type
--   If not found, return `Nothing`, if type is incorrect throw error
maybeTypedKey
  :: (MonadIO m, MonadThrow m)
  => Dhall.Map.Map Text (DhallExpr Void)
  -> Text
  -> Dhall.Decoder a
  -> m (Maybe a)
maybeTypedKey ks name typ = typify `mapM` Dhall.Map.lookup name ks
  where
    typify expr = case Dhall.extract typ expr of
      Success v -> pure v
      Failure a -> throwM a


-- | Convert a Dhall expression to a given Dhall type
--
--   We first annotate the expression with the Dhall type we want to get,
--   then try to typecheck it. We then need to run `Dhall.extract` on the
--   result of the normalization (we need to normalize so that extract can work)
--   and return a `Right` only if both typecheck and normalization succeeded.
coerceToType
  :: Dhall.Decoder a -> DhallExpr Void -> Either (ReadError Void) a
coerceToType typ expr = do
  let annot = Dhall.Annot expr $ Dhall.expected typ
  let checkedType = typeOf annot
  case (Dhall.extract typ $ Dhall.normalize annot, checkedType) of
    (Success x, Right _) -> Right x
    _                    -> Left $ WrongType typ expr


-- | Spago configuration cannot be read
data ReadError a where
 -- | a package has the wrong type
 WrongType             :: Typeable a => Dhall.Decoder b -> DhallExpr a -> ReadError a
 -- | the toplevel value is not a record
 ConfigIsNotRecord     :: Typeable a => DhallExpr a -> ReadError a
 -- | the "packages" key is not a record
 PackagesIsNotRecord   :: Typeable a => DhallExpr a -> ReadError a
 -- | the "dependencies" key is not a list
 DependenciesIsNotList :: Typeable a => DhallExpr a -> ReadError a
 -- | the expression is not a Text Literal
 ExprIsNotTextLit      :: Typeable a => DhallExpr a -> ReadError a
 -- | the packages.dhall could not be parsed
 CannotParsePackageSet :: Typeable a => DhallExpr a -> ReadError a
 -- | the Import is not pointing to the right repo
 ImportCannotBeUpdated :: Typeable a => Dhall.Import -> ReadError a
 -- | a key is missing from a Dhall map
 RequiredKeyMissing    :: Typeable a => Text -> Dhall.Map.Map Text (DhallExpr a) -> ReadError a

instance (Pretty a, Typeable a) => Exception (ReadError a)

instance (Pretty a) => Show (ReadError a) where
  show err = Text.unpack $ Text.intercalate "\n" $
    [ _ERROR <> ": Error while reading spago.dhall:"
    , "" ]
    <> msg err

    where
      msg :: ReadError a -> [Dhall.Text]
      msg (WrongType typ val) =
        [ "Explanation: you tried to coerce an expression to the wrong type."
        , ""
        , "The type was the following:"
        , ""
        , "↳ " <> pretty (Dhall.expected typ)
        , ""
        , "And the expression was the following:"
        , ""
        , "↳ " <> pretty val
        ]
      msg (PackagesIsNotRecord tl) =
        [ "Explanation: The \"packages\" key must contain a record of packages."
        , ""
        , "The value was instead:"
        , ""
        , "↳ " <> pretty tl
        ]
      msg (DependenciesIsNotList e) =
        [ "Explanation: The \"dependencies\" key must contain a list of package names."
        , ""
        , "The value was instead:"
        , ""
        , "↳ " <> pretty e
        ]
      msg (ConfigIsNotRecord tl) =
        [ "Explanation: The config should be a record."
        , ""
        , "Its type is instead:"
        , ""
        , "↳ " <> pretty tl
        ]
      msg (RequiredKeyMissing key ks) =
        [ "Explanation: a record is missing a required key."
        , ""
        , "The key missing is:"
        , ""
        , "↳ " <> key
        , ""
        , "The keys in the record are:"
        , ""
        , "↳ " <> (Text.intercalate ", " $ Dhall.Map.keys ks)
        ]
      msg (ExprIsNotTextLit e) =
        [ "Explanation: the configuration contained a value that we expected to be"
        , "a string, but wasn't."
        , ""
        , "The value was instead:"
        , ""
        , "↳ " <> pretty e
        ]
      msg (CannotParsePackageSet e) =
        [ "Explanation: it was not possible to parse the `packages.dhall` file."
        , ""
        , "This is its Dhall expression:"
        , ""
        , "↳ " <> pretty e
        ]
      msg (ImportCannotBeUpdated imp) =
        [ "Explanation: one of the imports in your `packages.dhall` file was not"
        , "pointing to the purescript/package-sets repo, thus it couldn't be upgraded."
        , ""
        , "The import was:"
        , ""
        , "↳ " <> pretty (Dhall.Embed imp)
        ]

      _ERROR :: Dhall.Text
      _ERROR = "\ESC[1;31mError\ESC[0m"
