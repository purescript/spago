module Web.Event.Internal.Types where

-- | Basic type for all DOM events.
foreign import data Event :: Type

-- | A DOM item that can emit events.
foreign import data EventTarget :: Type
