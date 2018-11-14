{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.ByteString (ByteString, packCStringLen)

import Control.Monad.Trans (lift, MonadTrans(..))
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)

showIntegerZeros :: Int -> Integer -> String
showIntegerZeros digits a = replicate (digits - length s) '0' ++ s where
    s = show a

data TimeOfDay = TimeOfDay Int Int Integer

showFixed :: Integer -> String
showFixed a = show i ++ showIntegerZeros digits fracNum where
    res = 1000000000000
    (i,d) = divMod a res
    -- enough digits to be unambiguous
    digits = ceiling (logBase 10 (fromInteger res) :: Double)
    maxnum = 10 ^ digits
    -- read floors, so show must ceil for `read . show = id` to hold. See #9240
    fracNum = divCeil (d * maxnum) res
    divCeil x y = (x + y - 1) `div` y
{-# NOINLINE showFixed #-}
-- doesn't crash when inlined

instance Show TimeOfDay where
    show (TimeOfDay h m s) = show h ++ show m ++ showFixed s

bufsize :: Int
bufsize = 8192

readFromPtr :: MonadIO m => Ptr a -> m ByteString
readFromPtr buf = do
    bs <- liftIO $ packCStringLen (castPtr buf, bufsize)
    liftIO $ print bs -- required
    return bs

type Enumerator s m a = Iteratee s m a -> m (Iteratee s m a)

newtype Iteratee s m a = Iteratee { runIter :: forall r.
          (a -> s -> m r) ->
          ((s -> Iteratee s m a) -> m r) ->
          m r}

enumFromCallbackCatch :: (Monad m, Monoid s) => m s -> Enumerator s m a
enumFromCallbackCatch c = loop
  where
    loop iter = runIter iter (\a b -> return $ idone a b) onCont
    onCont k = c >>= loop . k

idone :: Monad m => a -> s -> Iteratee s m a
idone a s = Iteratee $ \onDone _ -> onDone a s

eneeCheckIfDone f inner = Iteratee $ \od oc ->
  let onDone x s = od (idone x s) (mempty)
      onCont k = runIter (f k) od oc
  in runIter inner onDone onCont

type Enumeratee sFrom sTo m a = Iteratee sTo m a -> Iteratee sFrom m (Iteratee sTo m a)

liftI :: Monad m => (s -> Iteratee s m a) -> Iteratee s m a
liftI k = Iteratee $ \_ onCont -> onCont k

decodePcap :: Enumeratee ByteString [(TimeOfDay, ByteString)] IO a
decodePcap = eneeCheckIfDone (liftI . go)
    where
        go k c = eneeCheckIfDone (liftI . go) (k [(TimeOfDay 0 0 0, c)])

-- needs to be a typeclass
instance Monoid s => MonadTrans (Iteratee s) where
  lift m = Iteratee $ \onDone _ -> m >>= flip onDone (mempty)

instance (Functor m, Monad m) => Functor (Iteratee s m) where
  fmap f m = Iteratee $ \onDone onCont ->
    let od = onDone . f
        oc = onCont . (fmap f .)
    in runIter m od oc

instance (Functor m, Monad m, Monoid s) => Applicative (Iteratee s m) where
    pure x  = idone x mempty
    m <*> a = m >>= flip fmap a

instance (Monad m, Monoid s) => Monad (Iteratee s m) where
  return x = Iteratee $ \onDone _ -> onDone x (mempty)
  (>>=) = bindIteratee

bindIteratee :: (Monad m, Monoid s) => Iteratee s m a -> (a -> Iteratee s m b) -> Iteratee s m b
bindIteratee = self
    where
        self m f = Iteratee $ \onDone onCont ->
             let m_done a stream = runIter (f a) (const . flip onDone stream) f_cont
                   where f_cont k = runIter (k stream) onDone onCont
             in runIter m m_done (onCont . (flip self f .))

mapMI_ :: Monad m => (el -> m b) -> Iteratee [el] m ()
mapMI_ f = liftI step
  where
    step [] = liftI step -- required
    step xs = lift (mapM_ f xs) >> liftI step

main :: IO ()
main = do
  p <- mallocBytes bufsize
  forM_ [0..1023] $ \i -> pokeElemOff (castPtr p) i i
  _ <- enumFromCallbackCatch (readFromPtr p) $ decodePcap (mapMI_ print)
  pure ()
