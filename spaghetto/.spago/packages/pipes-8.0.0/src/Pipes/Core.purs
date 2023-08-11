module Pipes.Core (
  -- * Proxy Monad Transformer
    runEffect
  , runEffectRec

  -- * Categories

  -- ** Respond
  , respond
  , composeResponse'
  , (/>/)
  , composeResponse
  , (//>)

  -- ** Request
  , request
  , composeRequest'
  , (\>\)
  , composeRequest
  , (>\\)

  -- ** Push
  , push
  , composePush
  , (>~>)
  , composePush'
  , (>>~)

  -- ** Pull
  , pull
  , composePull
  , (>+>)
  , composePull'
  , (+>>)

  -- ** Reflect
  , reflect

  -- * Concrete Type Synonyms
  , Effect
  , Producer
  , Pipe
  , Consumer
  , Client
  , Server
  , Effect_
  , Producer_
  , Consumer_
  , Client_
  , Server_

  -- * Flipped operators
  , flippedComposeResponse'
  , (\<\)
  , flippedComposeRequest'
  , (/</)
  , flippedComposePush
  , (<~<)
  , flippedComposePush'
  , (~<<)
  , flippedComposePull
  , (<+<)
  , flippedComposePull'
  , (<<+)
  , flippedComposeResponse
  , (<\\)
  , flippedComposeRequest
  , (//<)

  -- * Re-exports
  , module I
  ) where

import Prelude (class Monad, Unit, pure, (<$>), (<<<), (>>=))
import Pipes.Internal (Proxy(..), X, closed)
import Pipes.Internal (Proxy (), X(), closed) as I
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(Loop, Done))

type Effect      = Proxy X Unit Unit X
type Producer b  = Proxy X Unit Unit b
type Pipe a b    = Proxy Unit a Unit b
type Consumer a  = Proxy Unit a Unit X
type Client a' a = Proxy a' a Unit X
type Server b' b = Proxy X Unit b' b

type Effect_        m r = forall x' x y' y. Proxy x'   x y'   y m r
type Producer_ b    m r = forall x' x.      Proxy x'   x Unit b m r
type Consumer_ a    m r = forall y' y.      Proxy Unit a y'   y m r
type Server_   b' b m r = forall x' x.      Proxy x'   x b'   b m r
type Client_   a' a m r = forall y' y.      Proxy a'   a y'   y m r

runEffect :: forall m r. Monad m => Effect m r -> m r
runEffect = go
  where
    go p = case p of
        Request v _ -> closed v
        Respond v _ -> closed v
        M       m   -> m >>= go
        Pure    r   -> pure r

runEffectRec :: forall m r. (MonadRec m) => Effect m r -> m r
runEffectRec = tailRecM go
  where
  go (Request v _) = Done <$> closed v
  go (Respond v _) = Done <$> closed v
  go (Pure r)      = pure (Done r)
  go (M mr)        = Loop <$> mr

respond :: forall m a a' x x'. Monad m => a -> Proxy x' x a' a m a'
respond a = Respond a Pure

infixl 4 composeResponse         as //>
infixr 4 composeResponse'        as />/
infixl 5 composeRequest          as >\\
infixr 4 composeRequest'         as \>\
infixr 6 composePull'            as +>>
infixl 7 composePull             as >+>
infixl 7 composePush'            as >>~
infixr 8 composePush             as >~>
infixl 4 flippedComposeResponse' as \<\
infixr 4 flippedComposeRequest'  as /</
infixl 8 flippedComposePush      as <~<
infixr 7 flippedComposePull      as <+<
infixr 3 flippedComposeResponse  as <\\
infixl 4 flippedComposeRequest   as //<
infixr 7 flippedComposePush'     as ~<<
infixl 6 flippedComposePull'     as <<+

composeResponse
    :: forall m x x' a' b b' c c'
     . Monad m
    =>       Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    ->       Proxy x' x c' c m a'
composeResponse p0 fb = go p0
  where
    go p = case p of
        Request x' fx  -> Request x' (go <<< fx)
        Respond b  fb' -> fb b >>= go <<< fb'
        M          m   -> M (go <$> m)
        Pure       a   -> Pure a

composeResponse'
    :: forall m x x' a a' b b' c c'
     . Monad m
    => (a -> Proxy x' x b' b m a')
    -> (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x c' c m a')
composeResponse' fa fb a = fa a //> fb

request :: forall a a' y y' m. Monad m => a' -> Proxy a' a y' y m a
request a' = Request a' Pure

-- (>\\)
composeRequest
  :: forall a a' b b' c y y' m
   . Monad m
  => (b' -> Proxy a' a y' y m b)
  ->        Proxy b' b y' y m c
  ->        Proxy a' a y' y m c
composeRequest fb' p0 = go p0
  where
    go p = case p of
        Request b' fb  -> fb' b' >>= go <<< fb
        Respond x  fx' -> Respond x (go <<< fx')
        M          m   -> M (go <$> m)
        Pure       a   -> Pure a

-- (\>\)
composeRequest'
  :: forall a a' b b' c c' y y' m
   . Monad m
  => (b' -> Proxy a' a y' y m b)
  -> (c' -> Proxy b' b y' y m c)
  -> (c' -> Proxy a' a y' y m c)
composeRequest' fb' fc' c' = fb' `composeRequest` fc' c'

pull :: forall a a' m r. Monad m => a' -> Proxy a' a a' a m r
pull = go
  where
    go a' = Request a' (\a -> Respond a go)

composePull
    :: forall a a' b b' c c' _c' m r
     . Monad m
    => ( b' -> Proxy a' a b' b m r)
    -> (_c' -> Proxy b' b c' c m r)
    -> (_c' -> Proxy a' a c' c m r)
composePull fb' fc' c' = fb' +>> fc' c'

composePull'
    :: forall a a' b b' c c' m r
     . Monad m
    => (b' -> Proxy a' a b' b m r)
    ->        Proxy b' b c' c m r
    ->        Proxy a' a c' c m r
composePull' fb' p = case p of
    Request b' fb  -> fb' b' >>~ fb
    Respond c  fc' -> Respond c ((fb' +>> _) <<< fc')
    M          m   -> M ((fb' +>> _) <$> m)
    Pure       r   -> Pure r

push :: forall a a' m r. Monad m => a -> Proxy a' a a' a m r
push = go
  where
    go a = Respond a (\a' -> Request a' go)

composePush
    :: forall _a a a' b b' c c' m r
     . Monad m
    => (_a -> Proxy a' a b' b m r)
    -> ( b -> Proxy b' b c' c m r)
    -> (_a -> Proxy a' a c' c m r)
composePush fa fb a = fa a >>~ fb

composePush'
    :: forall a a' b b' c c' m r
     . Monad m
    =>       Proxy a' a b' b m r
    -> (b -> Proxy b' b c' c m r)
    ->       Proxy a' a c' c m r
composePush' p fb = case p of
    Request a' fa  -> Request a' (\a -> fa a >>~ fb)
    Respond b  fb' -> fb' +>> fb b
    M          m   -> M (m >>= \p' -> pure (p' >>~ fb))
    Pure       r   -> Pure r

reflect
  :: forall a a' b b' m r
   . Monad m
  => Proxy a' a b' b m r -> Proxy b b' a a' m r
reflect = go
  where
    go p = case p of
        Request a' fa  -> Respond a' (go <<< fa)
        Respond b  fb' -> Request b  (go <<< fb')
        M          m   -> M (go <$> m)
        Pure    r      -> Pure r

-- | Equivalent to ('/>/') with the arguments flipped
flippedComposeResponse'
    :: forall m x x' a a' b b' c c'
     . Monad m
    => (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x b' b m a')
    -> (a -> Proxy x' x c' c m a')
flippedComposeResponse' p1 p2 = p2 />/ p1

-- | Equivalent to ('\>\') with the arguments flipped
flippedComposeRequest'
  :: forall a a' b b' c c' y y' m
   . Monad m
    => (c' -> Proxy b' b y' y m c)
    -> (b' -> Proxy a' a y' y m b)
    -> (c' -> Proxy a' a y' y m c)
flippedComposeRequest' p1 p2 = p2 \>\ p1

-- | Equivalent to ('>~>') with the arguments flipped
flippedComposePush
    :: forall a a' b b' c c' m r
     . Monad m
    => (b -> Proxy b' b c' c m r)
    -> (a -> Proxy a' a b' b m r)
    -> (a -> Proxy a' a c' c m r)
flippedComposePush p1 p2 = p2 >~> p1

-- | Equivalent to ('>+>') with the arguments flipped
flippedComposePull
    :: forall a a' b b' c c' m r
     . Monad m
    => (c' -> Proxy b' b c' c m r)
    -> (b' -> Proxy a' a b' b m r)
    -> (c' -> Proxy a' a c' c m r)
flippedComposePull p1 p2 = p2 >+> p1

-- | Equivalent to ('//>') with the arguments flipped
flippedComposeResponse
    :: forall m x x' a' b b' c c'
     . Monad m
    => (b -> Proxy x' x c' c m b')
    ->       Proxy x' x b' b m a'
    ->       Proxy x' x c' c m a'
flippedComposeResponse f p = p //> f

-- | Equivalent to ('>\\') with the arguments flipped
flippedComposeRequest
  :: forall a a' b b' c y y' m
   . Monad m
    =>        Proxy b' b y' y m c
    -> (b' -> Proxy a' a y' y m b)
    ->        Proxy a' a y' y m c
flippedComposeRequest p f = f >\\ p

-- | Equivalent to ('>>~') with the arguments flipped
flippedComposePush'
    :: forall a a' b b' c c' m r
     . Monad m
    => (b  -> Proxy b' b c' c m r)
    ->        Proxy a' a b' b m r
    ->        Proxy a' a c' c m r
flippedComposePush' k p = p >>~ k

-- | Equivalent to ('+>>') with the arguments flipped
flippedComposePull'
    :: forall a a' b b' c c' m r
     . Monad m
    =>         Proxy b' b c' c m r
    -> (b'  -> Proxy a' a b' b m r)
    ->         Proxy a' a c' c m r
flippedComposePull' k p = p +>> k
