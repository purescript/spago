-- A majority of the below code was ported from this JavaScript library
-- https://github.com/grncdr/merge-stream
-- Copyright `merge-stream` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.MergeStream
  ( Interface
  , mergeStreams
  , add
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.Stream (Readable, Duplex, end)
import Node.Library.Execa.Utils (newPassThroughStream)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

newtype Interface = Interface
  { sources :: Ref (Array (Readable ()))
  , output :: Duplex
  }

mergeStreams :: (Interface -> Effect Unit) -> Effect Duplex
mergeStreams useInterface = do
  iface@(Interface { output }) <- buildInterface
  useInterface iface
  pure output

buildInterface :: Effect Interface
buildInterface = do
  sources <- Ref.new []
  output <- newPassThroughStream
  pure $ Interface { sources, output }

add :: forall w. Readable w -> Interface -> Effect Boolean
add source iface@(Interface r) = do
  ifM
    (runEffectFn1 readable r.output)
    ( do
        let readStream = toReadableOnlyStream source
        Ref.modify_ (flip Array.snoc readStream) r.sources

        runEffectFn2 onceEnd readStream (remove readStream iface)
        runEffectFn2 onceError readStream r.output
        runEffectFn3 pipeImpl readStream r.output { end: false }
        pure true
    )
    (pure false)

remove :: forall w. Readable w -> Interface -> Effect Unit
remove source (Interface r) = do
  arr <- Ref.read r.sources
  let arr' = Array.filter (not <<< unsafeRefEq (toReadableOnlyStream source)) arr
  stillReadable <- runEffectFn1 readable r.output
  when (Array.length arr' == 0 && stillReadable) do
    end r.output mempty

toReadableOnlyStream :: forall w. Readable w -> Readable ()
toReadableOnlyStream = unsafeCoerce

foreign import onceEnd :: EffectFn2 (Readable ()) (Effect Unit) Unit
foreign import onceError :: EffectFn2 (Readable ()) Duplex Unit
foreign import pipeImpl :: EffectFn3 (Readable ()) Duplex { end :: Boolean } Unit
foreign import readable :: EffectFn1 (Duplex) Boolean
