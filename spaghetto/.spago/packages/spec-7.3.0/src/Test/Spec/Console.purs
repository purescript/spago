module Test.Spec.Console
  ( tellLn
  , tellLns
  , write
  , logWriter
  ) where

import Prelude

import Control.Monad.Writer (class MonadWriter, WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import write :: String -> Effect Unit

logWriter :: forall m. MonadEffect m => WriterT String m Unit -> m Unit
logWriter = execWriterT >=> write >>> liftEffect

tellLn
  :: forall m
   . MonadWriter String m
  => String
  -> m Unit
tellLn l = tell $ l <> "\n"

tellLns
  :: forall m
   . MonadWriter String m
  => Array String
  -> m Unit
tellLns l = for_ l $ (_<> "\n") >>> tell
