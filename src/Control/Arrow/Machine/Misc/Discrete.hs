{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module
    Control.Arrow.Machine.Misc.Discrete
      (
        -- * Discrete type
        -- $type

        T(),
        updates,
        value,

        arr,
        arr2,
        arr3,
        arr4,
        arr5,

        constant,
        unsafeConstant,
        hold,
        accum,
        fromEq,

        edge,
        asUpdater,
        kSwitch,
        dkSwitch,

        -- * Discrete algebra
        -- $alg

        Alg(Alg),
        eval,
        refer
      )
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow hiding (arr)
import Control.Applicative
import qualified Control.Arrow as Arr
import qualified Control.Arrow.Machine as P
import Data.Monoid (mconcat, mappend)

{-$type
This module should be imported manually. Qualified import is recommended.

This module provides an abstraction that continuous values with
finite number of changing points.

>>> import qualified Control.Arrow.Machine.Misc.Discrete as D
>>> P.run (D.hold "apple" >>> D.arr reverse >>> D.edge) ["orange", "grape"]
["elppa","egnaro","eparg"]

In above example, input data of "reverse" is continuous.
But the "D.edge" transducer extracts changing points without calling string comparison.

This is possible because the intermediate type `T` has the information of changes
together with the value information.
-}

-- |The discrete signal type.
data T a = T {
    updates :: (P.Event ()),
    value :: a
  }

makeT ::
    Monad m =>
    P.ProcessT m (P.Event (), b) (T b)
makeT = Arr.arr $ uncurry T


stimulate ::
    Monad m =>
    P.ProcessT m b (T c) ->
    P.ProcessT m b (T c)
stimulate sf = P.dgSwitch (id &&& id) sf body $ \sf' _ -> sf'
  where
    body = proc (dy, _) ->
      do
        n <- P.now -< ()
        disc <- makeT -< (updates dy `mappend` n, value dy)
        returnA -< (disc, updates disc)

arr ::
    Monad m =>
    (b->c) ->
    P.ProcessT m (T b) (T c)
arr f =
    Arr.arr $ \(T ev x) ->
        T ev (f x)

arr2 ::
    Monad m =>
    (b1->b2->c) ->
    P.ProcessT m (T b1, T b2) (T c)
arr2 f =
    Arr.arr $ \(T ev1 x1, T ev2 x2) ->
        T (mconcat [ev1, ev2]) (f x1 x2)

arr3 ::
    Monad m =>
    (b1->b2->b3->c) ->
    P.ProcessT m (T b1, T b2, T b3) (T c)
arr3 f =
    Arr.arr $ \(T ev1 x1, T ev2 x2, T ev3 x3) ->
        T (mconcat [ev1, ev2, ev3]) (f x1 x2 x3)

arr4 ::
    Monad m =>
    (b1->b2->b3->b4->c) ->
    P.ProcessT m (T b1, T b2, T b3, T b4) (T c)
arr4 f =
    Arr.arr $ \(T ev1 x1, T ev2 x2, T ev3 x3, T ev4 x4) ->
        T (mconcat [ev1, ev2, ev3, ev4]) (f x1 x2 x3 x4)

arr5 ::
    Monad m =>
    (b1->b2->b3->b4->b5->c) ->
    P.ProcessT m (T b1, T b2, T b3, T b4, T b5) (T c)
arr5 f =
    Arr.arr $ \(T ev1 x1, T ev2 x2, T ev3 x3, T ev4 x4, T ev5 x5) ->
        T (mconcat [ev1, ev2, ev3, ev4, ev5]) (f x1 x2 x3 x4 x5)

constant::
    Monad m =>
    c ->
    P.ProcessT m b (T c)
constant x =
    (P.now &&& Arr.arr (const x)) >>> makeT

-- |Constant without initial notifications.
-- Users must manage initialization manually.
unsafeConstant::
    Monad m =>
    c ->
    P.ProcessT m b (T c)
unsafeConstant x =
    (pure P.noEvent &&& Arr.arr (const x)) >>> makeT

onUpdate ::
    Monad m =>
    P.ProcessT m (P.Event b) (P.Event ())
onUpdate = proc ev ->
  do
    n <- P.now -< ()
    returnA -< n `mappend` P.collapse ev

hold ::
    Monad m =>
    b ->
    P.ProcessT m (P.Event b) (T b)
hold i =
    (onUpdate &&& P.hold i) >>> makeT

accum ::
    Monad m =>
    b ->
    P.ProcessT m (P.Event (b->b)) (T b)
accum i =
    (onUpdate &&& P.accum i) >>> makeT

fromEq ::
    (Monad m, Eq b) =>
    P.ProcessT m b (T b)
fromEq = proc x ->
  do
    ev <- P.edge -< x
    returnA -< T (P.collapse ev) x

edge ::
    Monad m =>
    P.ProcessT m (T b) (P.Event b)
edge = Arr.arr $ \(T ev x) -> x <$ ev

asUpdater ::
    Monad m =>
    (b -> m c) ->
    P.ProcessT m (T b) (P.Event c)
asUpdater fmx = edge >>> P.fire fmx


kSwitch ::
    Monad m =>
    P.ProcessT m b (T c) ->
    P.ProcessT m (b, T c) (P.Event t) ->
    (P.ProcessT m b (T c) -> t -> P.ProcessT m b (T c)) ->
    P.ProcessT m b (T c)
kSwitch sf test k = P.kSwitch sf test (\sf' x -> stimulate (k sf' x))

dkSwitch ::
    Monad m =>
    P.ProcessT m b (T c) ->
    P.ProcessT m (b, T c) (P.Event t) ->
    (P.ProcessT m b (T c) -> t -> P.ProcessT m b (T c)) ->
    P.ProcessT m b (T c)
dkSwitch sf test k = P.dkSwitch sf test (\sf' x -> stimulate (k sf' x))


{-$alg
Calculations between discrete types.

An example is below.

@
holdAdd ::
    (Monad m, Num b) =>
    ProcessT m (Event b, Event b) (Discrete b)
holdAdd = proc (evx, evy) ->
  do
    x <- D.hold 0 -< evx
    y <- D.hold 0 -< evy
    D.eval (refer fst + refer snd) -< (x, y)
@

The last line is equivalent to "arr2 (+) -< (x, y)".
Using Alg, you can construct more complex calculations
between discrete signals.
-}

-- |Discrete algebra type.
newtype Alg m i o =
    Alg { eval :: P.ProcessT m i (T o) }

refer ::
    Monad m =>
    (e -> T b) -> Alg m e b
refer = Alg . Arr.arr

instance
    Monad m => Functor (Alg m i)
  where
    fmap f alg = Alg $ eval alg >>> arr f

instance
    Monad m => Applicative (Alg m i)
  where
    pure = Alg . constant
    af <*> aa = Alg $ (eval af &&& eval aa) >>> arr2 ($)

instance
    (Monad m, Num o) =>
    Num (Alg m i o)
  where
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)

