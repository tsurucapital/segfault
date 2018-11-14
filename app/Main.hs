{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Data.ByteString (ByteString, packCStringLen)

import Control.Monad
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)

showIntegerZeros :: Int -> Integer -> String
showIntegerZeros digits a = replicate (digits - length s) '0' ++ s where
    s = show a

newtype TimeOfDay = TimeOfDay Integer

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
    show (TimeOfDay s) = showFixed s

bufsize :: Int
bufsize = 8192

readFromPtr :: Ptr a -> IO ByteString
readFromPtr buf = do
    bs <- packCStringLen (castPtr buf, bufsize)
    print bs -- required
    return bs

type Enumerator s a = Iteratee s a -> IO (Iteratee s a)

newtype Iteratee s a = Iteratee { runIter :: forall r.
          (a -> s -> IO r) ->
          ((s -> Iteratee s a) -> IO r) ->
          IO r}

enumFromCallbackCatch :: (Monoid s) => IO s -> Enumerator s a
enumFromCallbackCatch c = loop
  where
    loop iter = runIter iter (\a b -> return $ idone a b) onCont
    onCont k = c >>= loop . k

idone :: a -> s -> Iteratee s a
idone a s = Iteratee $ \onDone _ -> onDone a s

eneeCheckIfDone f inner = Iteratee $ \od oc ->
  let onDone x s = od (idone x s) (mempty)
      onCont k = runIter (f k) od oc
  in runIter inner onDone onCont

type Enumeratee sFrom sTo a = Iteratee sTo a -> Iteratee sFrom (Iteratee sTo a)

liftI :: (s -> Iteratee s a) -> Iteratee s a
liftI k = Iteratee $ \_ onCont -> onCont k

decodePcap :: Enumeratee ByteString [(TimeOfDay, ByteString)] a
decodePcap = eneeCheckIfDone (liftI . go)
    where
        go k c = eneeCheckIfDone (liftI . go) (k [(TimeOfDay 0, c)])

lift :: (Monoid s) => IO a -> Iteratee s a
lift m = Iteratee $ \onDone _ -> m >>= flip onDone (mempty)

instance Functor (Iteratee s) where
  fmap f m = Iteratee $ \onDone onCont ->
    let od = onDone . f
        oc = onCont . (fmap f .)
    in runIter m od oc

instance (Monoid s) => Applicative (Iteratee s) where
    pure x  = idone x mempty
    m <*> a = m >>= flip fmap a

instance (Monoid s) => Monad (Iteratee s) where
  return x = Iteratee $ \onDone _ -> onDone x (mempty)
  (>>=) = bindIteratee

bindIteratee :: (Monoid s) => Iteratee s a -> (a -> Iteratee s b) -> Iteratee s b
bindIteratee = self
    where
        self m f = Iteratee $ \onDone onCont ->
             let m_done a stream = runIter (f a) (const . flip onDone stream) f_cont
                   where f_cont k = runIter (k stream) onDone onCont
             in runIter m m_done (onCont . (flip self f .))

mapMI_ :: (el -> IO b) -> Iteratee [el] ()
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
