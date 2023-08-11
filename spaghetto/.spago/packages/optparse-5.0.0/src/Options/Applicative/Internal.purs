module Options.Applicative.Internal
  ( P
  , class MonadP
  , enterContext
  , exitContext
  , getPrefs
  , missingArgP
  , errorP
  , exitP

  , module Reexport

  , hoistMaybe
  , hoistEither
  , runReadM
  , withReadM

  , runP

  , Completion
  , runCompletion
  , contextNames

  , ListT
  , takeListT
  , runListT

  , NondetT
  , cut
  , (<!>)
  , nondetTAltOp
  , disamb

  ) where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Control.Monad.Except (class MonadTrans, runExcept, withExcept)
import Control.Monad.Except.Trans (ExceptT, lift, runExceptT, throwError)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Trans (mapReaderT, runReaderT, ReaderT, ask)
import Control.Monad.State.Trans (StateT, evalStateT, get, modify_, put, runStateT)
import Control.MonadPlus (class MonadPlus, guard)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Exists (mkExists)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Options.Applicative.Types (ArgPolicy, Completer, Context(..), IsCmdStart, ParseError(..), Parser, ParserInfo, ParserPrefs, ReadM(..), SomeParser(..))
import Options.Applicative.Types (ParseError(..)) as Reexport

class (Monad m, Alt m) <= MonadP (m :: Type -> Type) where
  enterContext :: forall a. String -> ParserInfo a -> m Unit
  exitContext :: m Unit
  getPrefs :: m ParserPrefs

  missingArgP :: forall a. ParseError -> Completer -> m a
  errorP :: forall a. ParseError -> m a
  exitP :: forall a b. IsCmdStart -> ArgPolicy -> Parser b -> Maybe a -> m a

newtype P a = P (ExceptT ParseError (StateT (Array Context) (Reader ParserPrefs)) a)

instance pFunctor :: Functor P where
  map f (P m) = P $ map f m

instance pApply :: Apply P where
  apply (P f) (P a) = P $ f <*> a
instance pApplicative :: Applicative P where
  pure a = P $ pure a

instance pAlt :: Alt P where
  alt (P x) (P y) = P $ x `alt` y

instance pBind :: Bind P where
  bind (P x) k = P $ x >>= \a -> case k a of P y -> y

instance pMonad :: Monad P

contextNames :: Array Context -> Array String
contextNames ns =
  let go (Context n _) = n
  in  Array.reverse $ go <$> ns

instance pMonadP :: MonadP P where
  enterContext name pinfo = P $ lift $ modify_ $ Array.(:) $ Context name (mkExists pinfo)
  exitContext = P $ lift $ modify_ $ Array.drop 1
  getPrefs = P <<< lift <<< lift $ ask

  missingArgP e _ = errorP e
  exitP i _ p = P <<< maybe (throwError <<< MissingError i <<< SomeParser $ mkExists p) pure
  errorP = P <<< throwError

hoistMaybe :: forall a m. MonadP m => ParseError -> Maybe a -> m a
hoistMaybe err = maybe (errorP err) pure

hoistEither :: forall a m. MonadP m => Either ParseError a -> m a
hoistEither = either errorP pure

runP :: forall a. P a -> ParserPrefs -> (Tuple (Either ParseError a) (Array Context))
runP (P p) = runReader <<< flip runStateT [] <<< runExceptT $ p

-- uncons :: Array a -> Maybe (Tuple a (Array a))
-- uncons [] = Nothing
-- uncons (x : xs) = Just (Tuple x xs)

runReadM :: forall a m. MonadP m => ReadM a -> String -> m a
runReadM (ReadM r) s = hoistEither <<< runExcept $ runReaderT r s

withReadM :: forall a. (String -> String) -> ReadM a -> ReadM a
withReadM f = ReadM <<< mapReaderT (withExcept f') <<< un ReadM
  where
    f' (ErrorMsg err) = ErrorMsg (f err)
    f' e = e

data ComplResult a
  = ComplParser SomeParser ArgPolicy
  | ComplOption Completer
  | ComplResult a

instance complResultFunctor :: Functor ComplResult where
  map = liftM1

instance complResultApply :: Apply ComplResult where
  apply = ap

instance complResultApplicative :: Applicative ComplResult where
  pure = ComplResult

instance complResultBind :: Bind ComplResult where
  bind m f = case m of
    ComplResult r -> f r
    ComplParser p a -> ComplParser p a
    ComplOption c -> ComplOption c

instance complResultMonad :: Monad ComplResult

newtype Completion a =
  Completion (ExceptT ParseError (ReaderT ParserPrefs ComplResult) a)

instance completionFunctor :: Functor Completion where
  map f (Completion m) = Completion $ map f m

instance completionApply :: Apply Completion where
  apply (Completion f) (Completion a) = Completion $ f <*> a
instance completionApplicative :: Applicative Completion where
  pure a = Completion $ pure a

instance completionAlt :: Alt Completion where
  alt (Completion x) (Completion y) = Completion $ x `alt` y

instance completionBind :: Bind Completion where
  bind (Completion x) k = Completion $ x >>= \a -> case k a of Completion y -> y

instance completionMonad :: Monad Completion

instance completionMonadP :: MonadP Completion where
  enterContext _ _ = pure unit
  exitContext = pure unit
  getPrefs = Completion $ lift ask

  missingArgP _ = Completion <<< lift <<< lift <<< ComplOption
  exitP _ a p _ = Completion <<< lift <<< lift $ ComplParser (SomeParser $ mkExists p) a
  errorP = Completion <<< throwError

runCompletion :: forall r. Completion r -> ParserPrefs -> Maybe (Either (Tuple SomeParser ArgPolicy) Completer)
runCompletion (Completion c) prefs = case runReaderT (runExceptT c) prefs of
  ComplResult _ -> Nothing
  ComplParser p' a' -> Just $ Left (Tuple p' a')
  ComplOption compl -> Just $ Right compl

-- A "ListT done right" implementation

newtype ListT m a = ListT (m (TStep a (ListT m a)))

stepListT :: forall m a. ListT m a -> m (TStep a (ListT m a))
stepListT (ListT m) = m

data TStep a x
  = TNil
  | TCons a x

bimapTStep :: forall a b x y. (a -> b) -> (x -> y) -> TStep a x -> TStep b y
bimapTStep _ _ TNil = TNil
bimapTStep f g (TCons a x) = TCons (f a) (g x)

hoistList :: forall a m. Monad m => Array a -> ListT m a
hoistList = Array.foldr (\x xt -> ListT (pure (TCons x xt))) empty

takeListT :: forall m a. Monad m => Int -> ListT m a -> ListT m a
takeListT 0 = const empty
takeListT n = ListT <<< liftM1 (bimapTStep identity (takeListT (n - 1))) <<< stepListT

runListT ::forall a m. Monad m => ListT m a -> m (List a)
runListT xs = do
  s <- stepListT xs
  case s of
    TNil -> pure List.Nil
    TCons x xt -> liftM1 (List.Cons x) (runListT xt)

instance listTFunctor :: Monad m => Functor (ListT m) where
  map f v = ListT
         $ liftM1 (bimapTStep f (map f))
         $ stepListT v

instance listTApply :: Monad m => Apply (ListT m) where
  apply = ap

instance listTApplicative :: Monad m => Applicative (ListT m) where
  pure = hoistList <<< pure

instance listTBind :: Monad m => Bind (ListT m) where
  bind xs f = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> pure TNil
      TCons x xt -> stepListT $ f x `alt` (xt >>= f)

instance listTMonad :: Monad m => Monad (ListT m)

instance listTAlt :: Monad m => Alt (ListT m) where
  alt xs ys = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> stepListT ys
      TCons x xt -> pure $ TCons x (xt `alt` ys)
instance listTPlus :: Monad m => Plus (ListT m) where
  empty = ListT (pure TNil)
instance listTAlternative :: Monad m => Alternative (ListT m)

instance listTMonadTrans :: MonadTrans ListT where
  lift = ListT <<< liftM1 (_ `TCons` empty)

-- | instance listTMonadZero :: Monad m => MonadZero (ListT m)
instance listTMonadPlus :: Monad m => MonadPlus (ListT m)

-- nondeterminism monad with cut operator

newtype NondetT m a = NondetT (ListT (StateT Boolean m) a)

runNondetT :: forall m a. NondetT m a ->ListT (StateT Boolean m) a
runNondetT (NondetT m) = m

instance nondetTFunctor :: Monad m => Functor (NondetT m) where
  map f = NondetT <<< map f <<< runNondetT

instance nondetTApply :: Monad m => Apply (NondetT m) where
  apply (NondetT m1) (NondetT m2) = NondetT (m1 <*> m2)
instance nondetTApplicative :: Monad m => Applicative (NondetT m) where
  pure = NondetT <<< pure

instance nondetTBind :: Monad m => Bind (NondetT m) where
  bind (NondetT m1) f = NondetT $ m1 >>= runNondetT <<< f
instance nondetTMonad :: Monad m => Monad (NondetT m)

-- | instance nondetTMonadZero :: Monad m => MonadZero (NondetT m)
instance nondetTMonadPlus :: Monad m => MonadPlus (NondetT m)

instance nondetTAlt :: Monad m => Alt (NondetT m) where
  alt (NondetT m1) (NondetT m2) = NondetT (m1 `alt` m2)

instance nondetTPlus :: Monad m => Plus (NondetT m) where
  empty = NondetT empty

instance nondetTAlternative :: Monad m => Alternative (NondetT m)

instance nondetTMonadTrans :: MonadTrans NondetT where
  lift = NondetT <<< lift <<< lift

infixl 99 nondetTAltOp as <!>
nondetTAltOp :: forall a m. Monad m => NondetT m a -> NondetT m a -> NondetT m a
nondetTAltOp m1 m2 = NondetT <<< alt (runNondetT m1) $ do
  s <- lift get
  guard (not s)
  runNondetT m2

cut :: forall m. Monad m => NondetT m Unit
cut = NondetT $ lift (put true)

disamb :: forall m a. Monad m => Boolean -> NondetT m a -> m (Maybe a)
disamb allow_amb xs = do
  xs' <- (_ `evalStateT` false)
       <<< runListT
       <<< takeListT (if allow_amb then 1 else 2)
       <<< runNondetT $ xs
  pure $ case xs' of
    List.Cons x List.Nil -> Just x
    _   -> Nothing
