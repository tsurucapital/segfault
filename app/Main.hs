{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wall -ddump-simpl -dsuppress-ticks -ddump-to-file #-}

import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.ForeignPtr
import GHC.Base (realWorld#)
import Data.Word (Word8)
import Foreign.Storable (peek)
import GHC.IO

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

data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) {-# UNPACK #-} !Int {-# UNPACK #-} !Int

instance Show ByteString where
  showsPrec p ps r = showsPrec p (unpackAppendCharsLazy ps []) r

{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

unpackAppendCharsLazy :: ByteString -> [Char] -> [Char]
unpackAppendCharsLazy (PS fp off len) cs
  | len <= 100 = unpackAppendCharsStrict (PS fp off len) cs
  | otherwise  = unpackAppendCharsStrict (PS fp off 100) remainder
  where
    remainder  = unpackAppendCharsLazy (PS fp (off+100) (len-100)) cs

unpackAppendCharsStrict :: ByteString -> [Char] -> [Char]
unpackAppendCharsStrict (PS fp off len) xs =
    accursedUnutterablePerformIO $ withForeignPtr fp $ \base ->
      loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (w2c x:acc)

w2c :: Word8 -> Char
w2c = toEnum . fromEnum

packCStringLen :: Int -> IO ByteString
packCStringLen l = do
  fp <- mallocPlainForeignPtrBytes l
  return $! PS fp 0 l
{-# NOINLINE packCStringLen #-}

instance Show TimeOfDay where
    show (TimeOfDay s) = showFixed s

bufsize :: Int
bufsize = 8192

readFromPtr :: IO ByteString
readFromPtr = do
    bs <- packCStringLen bufsize
    length (show bs) `seq` return bs

type Enumerator s a = Iteratee s a -> IO (Iteratee s a)

newtype Iteratee s a = Iteratee { runIter :: forall r.
          (a -> IO r) ->
          ((s -> Iteratee s a) -> IO r) ->
          IO r}

enumFromCallbackCatch :: IO s -> Enumerator s a
enumFromCallbackCatch c = loop
  where
    loop iter = runIter iter (\a -> return $ pureI a) onCont
    onCont k = c >>= loop . k

eneeCheckIfDone f inner = Iteratee $ \od oc ->
  let onDone x = od (pureI x)
      onCont k = runIter (f k) od oc
  in runIter inner onDone onCont

type Enumeratee sFrom sTo a = Iteratee sTo a -> Iteratee sFrom (Iteratee sTo a)

liftI :: (s -> Iteratee s a) -> Iteratee s a
liftI k = Iteratee $ \_ onCont -> onCont k

decodePcap :: Enumeratee ByteString [(TimeOfDay, ByteString)] a
decodePcap = eneeCheckIfDone (liftI . go)
    where
        go k c = eneeCheckIfDone (liftI . go) (k [(TimeOfDay 0, c)])

lift :: IO a -> Iteratee s a
lift m = Iteratee $ \onDone _ -> m >>= onDone

pureI :: a -> Iteratee s a
pureI x = Iteratee $ \onDone _ -> onDone x

bindIteratee :: Iteratee s a -> (a -> Iteratee s b) -> Iteratee s b
bindIteratee = self
    where
        self m f = Iteratee $ \onDone onCont ->
             let m_done a = runIter (f a) onDone onCont
             in runIter m m_done (onCont . (flip self f .))

mapMI_ :: (el -> IO b) -> Iteratee [el] ()
mapMI_ f = liftI step
  where
    step [] = liftI step -- required
    step xs = bindIteratee (lift (mapM_ f xs)) $ const $ liftI step

main :: IO ()
main = do
  _ <- enumFromCallbackCatch readFromPtr $ decodePcap
    $ mapMI_ $ \(t, bs) -> print (t, bs)
  pure ()
