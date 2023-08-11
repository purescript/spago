module Test.Spec
  ( Spec
  , SpecT(..)
  , module Reexport
  , SpecTree
  , mapSpecTree
  , collect

  , ComputationType(..)
  , hoistSpec

  , class Example
  , evaluateExample

  , parallel
  , sequential

  , class FocusWarning
  , focus
  , describeOnly
  , itOnly

  , describe
  , it
  , pending
  , pending'

  , aroundWith
  , around
  , around_

  , before
  , before_
  , beforeWith
  , beforeAll
  , beforeAll_

  , after
  , after_
  , afterAll
  , afterAll_
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Monad.Cont (class MonadCont, class MonadTrans)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (WriterT, execWriterT, mapWriterT, tell)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Function (applyFlipped)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, un)
import Effect.AVar (AVar)
import Effect.AVar as AVarEff
import Effect.Aff (Aff, error, throwError, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Prim.TypeError (class Warn, Text)
import Test.Spec.Tree (ActionWith, Item(..), Tree(..)) as Reexport
import Test.Spec.Tree (ActionWith, Item(..), Tree(..), bimapTree, discardUnfocused, modifyAroundAction)


type Spec a = SpecT Aff Unit Identity a

newtype SpecT g i m a = SpecT (WriterT (Array (SpecTree g i)) m a)

derive instance newtypeSpecT :: Newtype (SpecT g i m a) _
derive newtype instance functorSpecT :: Functor m => Functor (SpecT g i m)
derive newtype instance applySpecT :: Apply m => Apply (SpecT g i m)
derive newtype instance applicativeSpecT :: Applicative m => Applicative (SpecT g i m)
derive newtype instance altSpecT :: Alt m => Alt (SpecT g i m)
derive newtype instance plusSpecT :: Plus m => Plus (SpecT g i m)
derive newtype instance alternativeSpecT :: (Alternative m) => Alternative (SpecT g i m)
derive newtype instance bindSpecT :: Bind m => Bind (SpecT g i m)
derive newtype instance monadSpecT :: Monad m => Monad (SpecT g i m)
derive newtype instance monadRecSpecT :: MonadRec m => MonadRec (SpecT g i m)
derive newtype instance monadPlusSpecT :: MonadPlus m => MonadPlus (SpecT g i m)
derive newtype instance monadTransSpecT :: MonadTrans (SpecT g i)
derive newtype instance monadEffectWriter :: MonadEffect m => MonadEffect (SpecT g i m)
derive newtype instance monadContSpecT :: MonadCont m => MonadCont (SpecT g i m)
derive newtype instance monadThrowSpecT :: MonadThrow e m => MonadThrow e (SpecT g i m)
derive newtype instance monadErrorSpecT :: MonadError e m => MonadError e (SpecT g i m)
derive newtype instance monadAskSpecT :: MonadAsk r m => MonadAsk r (SpecT g i m)
derive newtype instance monadReaderSpecT :: MonadReader r m => MonadReader r (SpecT g i m)
derive newtype instance monadStateSpecT :: MonadState s m => MonadState s (SpecT g i m)

type SpecTree m a = Tree (ActionWith m a) (Item m a)

mapSpecTree
  :: forall m m' g g' i a i'
   . Functor m'
   => (m ~> m')
   -> (SpecTree g i -> SpecTree g' i')
   -> SpecT g i m a
   -> SpecT g' i' m' a
mapSpecTree g f = over SpecT $ mapWriterT $ g >>> map (map $ map f)

data ComputationType = CleanUpWithContext (Array String) | TestWithName (NonEmptyArray String)

hoistSpec :: forall m' m i a b. Monad m' => (m ~> m') -> (ComputationType -> a ~> b) -> SpecT a i m ~> SpecT b i m'
hoistSpec onM f = mapSpecTree onM $ bimapTree onCleanUp onTest
  where
    onCleanUp :: Array String -> (ActionWith a i) -> ActionWith b i
    onCleanUp name around' = \i -> f (CleanUpWithContext name) (around' i)
    onTest :: NonEmptyArray String -> Item a i -> Item b i
    onTest name = over Item \item ->
      let
        e :: ((i -> b Unit) -> b Unit) -> b Unit
        e g = g (f (TestWithName name) <<< item.example <<< applyFlipped)
      in item { example = e }


-- | Collects all tests, if something is focused, all unfocused tests will be discarded
collect :: forall m g i a. Functor m => SpecT g i m a -> m (Array (SpecTree g i))
collect = un SpecT >>> execWriterT >>> map discardUnfocused

class Example t arg m | t -> arg, t -> m where
  evaluateExample :: t -> (ActionWith m arg -> m Unit) -> m Unit

instance exampleFunc :: Example (arg -> m Unit) arg m where
  evaluateExample :: (arg -> m Unit) -> (ActionWith m arg -> m Unit) -> m Unit
  evaluateExample t around' = around' t

else instance exampleMUnit :: Example (m Unit) Unit m where
  evaluateExample :: (m Unit) -> (ActionWith m Unit -> m Unit) -> m Unit
  evaluateExample t around' = around' $ \_ -> t


-- | Nullary class used to raise a custom warning for the focusing functions.
class FocusWarning

instance warn :: Warn (Text "Test.Spec.focus usage") => FocusWarning

-- ---------------------
-- --       DSL       --
-- ---------------------

-- | `focus` focuses all spec items of the given spec.
-- |
-- | Applying `focus` to a spec with focused spec items has no effect.
focus :: forall m g i a. FocusWarning => Monad m => SpecT g i m a -> SpecT g i m a
focus = over SpecT $ mapWriterT $ map $ map \xs ->
  if any (any $ un Item >>> _.isFocused) xs
    then xs
    else map (bimap identity (\(Item r) -> Item r {isFocused = true})) xs


-- | Combine a group of specs into a described hierarchy.
describe
  :: forall m g i a
   . Monad m
  => String
  -> SpecT g i m a
  -> SpecT g i m a
describe name = over SpecT $ mapWriterT $ map $ map \group -> [Node (Left name) group]


-- | Combine a group of specs into a described hierarchy and mark it as the
-- | only group to actually be evaluated. (useful for quickly narrowing down
-- | on a set)
describeOnly
  :: forall m g i a
   . FocusWarning
  => Monad m
  => String
  -> SpecT g i m a
  -> SpecT g i m a
describeOnly = map focus <<< describe

-- | marks all spec items of the given spec to be safe for parallel evaluation.
parallel
  :: forall m g i a
   . Monad m
  => SpecT g i m a
  -> SpecT g i m a
parallel = mapSpecTree identity $ bimap identity (setParallelizable true)

-- | marks all spec items of the given spec to be evaluated sequentially.
sequential
  :: forall m g i a
   . Monad m
  => SpecT g i m a
  -> SpecT g i m a
sequential = mapSpecTree identity $ bimap identity (setParallelizable false)

setParallelizable :: forall g a. Boolean -> Item g a -> Item g a
setParallelizable value = over Item \i -> i{isParallelizable = i.isParallelizable <|> Just value}

-- | Create a pending spec.
pending
  :: forall m g i
   . Monad m
  => String
  -> SpecT g i m Unit
pending name = SpecT $ tell [Leaf name Nothing]

-- | Create a pending spec with a body that is ignored by
-- | the runner. It can be useful for documenting what the
-- | spec should test when non-pending.
pending'
  :: forall m g i
   . Monad m
  => String
  -> g Unit
  -> SpecT g i m Unit
pending' name _ = pending name

-- | Create a spec with a description.
it
  :: forall m t arg g
   . Monad m
  => Example t arg g
  => String
  -> t
  -> SpecT g arg m Unit
it name test = SpecT $ tell
  [ Leaf name $ Just $ Item
      { isParallelizable: Nothing
      , isFocused: false
      , example: evaluateExample test
      }
  ]

-- | Create a spec with a description and mark it as the only one to
-- | be run. (useful for quickly narrowing down on a single test)
itOnly
  :: forall m t arg g
   . FocusWarning
  => Monad m
  => Example t arg g
  => String
  -> t
  -> SpecT g arg m Unit
itOnly = map focus <<< it


-- ---------------------
-- --      HOOKS      --
-- ---------------------

-- | Run a custom action before and/or after every spec item.
aroundWith
  :: forall m g i i' a
   . Monad m
  => (ActionWith g i -> ActionWith g i')
  -> SpecT g i m a
  -> SpecT g i' m a
aroundWith action = mapSpecTree identity $ bimap action (modifyAroundAction action)

-- | Run a custom action before and/or after every spec item.
around_ :: forall m g i a. Monad m => (g Unit -> g Unit) -> SpecT g i m a -> SpecT g i m a
around_ action = aroundWith $ \e a -> action (e a)

-- | Run a custom action after every spec item.
after :: forall m g e f i a. Monad m => MonadBracket e f g => ActionWith g i -> SpecT g i m a -> SpecT g i m a
after action = aroundWith $ \e x -> e x `finally` action x
  where
  finally :: forall x. g x -> g Unit -> g x
  finally act fin = bracket (pure unit) (\_ _ -> fin) (const act)

-- | Run a custom action after every spec item.
after_ :: forall m g e f i a. Monad m => MonadBracket e f g => g Unit -> SpecT g i m a -> SpecT g i m a
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: forall m g i a. Monad m => (ActionWith g i -> g Unit) -> SpecT g i m a -> SpecT g Unit m a
around action = aroundWith $ \e _ -> action e

-- | Run a custom action before every spec item.
before :: forall m g i a. Monad m => Monad g => g i -> SpecT g i m a -> SpecT g Unit m a
before action = around (action >>= _)

-- | Run a custom action before every spec item.
before_ :: forall m g i a. Monad m => Monad g => g Unit -> SpecT g i m a -> SpecT g i m a
before_ action = around_ (action *> _)

-- | Run a custom action before every spec item.
beforeWith :: forall m g i i' a. Monad m => Monad g => (i' -> g i) -> SpecT g i m a -> SpecT g i' m a
beforeWith action = aroundWith $ \e x -> action x >>= e

-- | Run a custom action before the first spec item.
beforeAll :: forall m g i a. MonadEffect m => MonadAff g => MonadError Error g => g i -> SpecT g i m a -> SpecT g Unit m a
beforeAll action spec = do
  var <- liftEffect $ AVarEff.new MEmpty
  before (memoize var action) spec

-- | Run a custom action before the first spec item.
beforeAll_ :: forall m g i a. MonadEffect m => MonadAff g => MonadError Error g => g Unit -> SpecT g i m a -> SpecT g i m a
beforeAll_ action spec = do
  var <- liftEffect $ AVarEff.new MEmpty
  before_ (memoize var action) spec

data Memoized a
  = MEmpty
  | MMemoized a
  | MFailed Error

memoize :: forall a m. MonadAff m => MonadError Error m => AVar (Memoized a) -> m a -> m a
memoize var action = do
  liftAff (AVar.take var) >>= case _ of
    MFailed _ -> throwError $ error "exception in beforeAll-hook (see previous failure)"
    MMemoized x -> pure x <* (liftAff $ AVar.put (MMemoized x) var)
    MEmpty -> do
      res <- try action
      liftAff $ AVar.put (either MFailed MMemoized res) var
      either throwError pure res

-- | Run a custom action after the last spec item.
afterAll :: forall m g i a. Monad m => ActionWith g i -> SpecT g i m a -> SpecT g i m a
afterAll action = over SpecT $ mapWriterT $ map $ map \group -> [Node (Right action) group]

-- | Run a custom action after the last spec item.
afterAll_ :: forall m g i a. Monad m => g Unit -> SpecT g i m a -> SpecT g i m a
afterAll_ action = afterAll $ const action
