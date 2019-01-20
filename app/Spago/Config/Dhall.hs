{-# LANGUAGE GADTs #-}
module Spago.Config.Dhall
  ( module Spago.Config.Dhall
  , module Dhall
  ) where

import           Control.Exception                     (Exception)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (Pretty)
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import           Data.Typeable                         (Typeable)
import           Dhall
import           Dhall.Core                            as Dhall hiding (Type, pretty)
import qualified Dhall.Map
import qualified Dhall.Parser                          as Parser
import           Turtle                                (MonadIO)
import Data.Foldable (fold)


type DhallExpr a = Dhall.Expr Parser.Src a

-- | Prettyprint a Dhall expression
pretty :: Pretty.Pretty a => DhallExpr a -> Dhall.Text
pretty = PrettyText.renderStrict
  . Pretty.layoutPretty Pretty.defaultLayoutOptions
  . Pretty.pretty

-- | Returns a Dhall Text literal from a lone string
toTextLit :: Pretty a => Text -> DhallExpr a
toTextLit str = Dhall.TextLit (Dhall.Chunks [] str)

-- | Casts a Dhall Text literal to a string, or fails
fromTextLit
  :: (Pretty a, Typeable a)
  => DhallExpr a
  -> Either (ReadError a) Text
fromTextLit (Dhall.TextLit (Dhall.Chunks [] str)) = Right str
fromTextLit expr                                  = Left $ ExprIsNotTextLit expr

-- | Require a key from a Dhall.Map, and run an action on it if found.
--   If not found, return the name of the key.
requireKey
  :: (Typeable b)
  => Dhall.Map.Map Text (DhallExpr b)
  -> Text
  -> (DhallExpr b -> Either (ReadError b) a)
  -> Either (ReadError b) a
requireKey ks name f = case (Dhall.Map.lookup name ks) of
  Just v  -> f v
  Nothing -> Left $ RequiredKeyMissing name ks


-- | Spago configuration cannot be read
data ReadError a where
 WrongPackageType      :: Typeable a => DhallExpr a -> ReadError a
   -- ^ a package has the wrong type
 ConfigIsNotRecord     :: Typeable a => DhallExpr a -> ReadError a
   -- ^ the toplevel value is not a record
 PackagesIsNotRecord   :: Typeable a => DhallExpr a -> ReadError a
   -- ^ the "packages" key is not a record
 DependenciesIsNotList :: Typeable a => DhallExpr a -> ReadError a
   -- ^ the "dependencies" key is not a list
 ExprIsNotTextLit      :: Typeable a => DhallExpr a -> ReadError a
   -- ^ the expression is not a Text Literal
 RequiredKeyMissing    :: Typeable a => Text -> Dhall.Map.Map Text (DhallExpr a) -> ReadError a
   -- ^ a key is missing from a Dhall map

instance (Pretty a, Typeable a) => Exception (ReadError a)

instance (Pretty a) => Show (ReadError a) where
  show err = Text.unpack $ Text.intercalate "\n" $
    [ _ERROR <> ": Error while reading spago.dhall:"
    , "" ]
    <> msg err

    where
      msg :: ReadError a -> [Dhall.Text]
      msg (WrongPackageType pkg) =
        [ "Explanation: The outermost record must only contain packages."
        , ""
        , "The following field was not a package:"
        , ""
        , "↳ " <> pretty pkg
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

      _ERROR :: Dhall.Text
      _ERROR = "\ESC[1;31mError\ESC[0m"
