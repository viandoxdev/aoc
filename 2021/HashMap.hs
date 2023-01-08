module HashMap (
    HashMap,
    Map,
    withHasherAndSize,
    withHasher,
    new,
    insert,
    used,
    toList,
    (!?),
    (!!!)
) where
import Hash
import Data.Array (Array, accumArray, elems, accum, (!))
import Data.Word (Word32)
import Data.Bits ((.&.))
import Data.List (nubBy, find, intercalate)
import Data.Maybe (fromJust)

data HashMap k v h = HashMapCons
    { buckets :: Array Word32 [(k, v)]
    , used :: Int
    , maxUsed :: Int
    , mask :: Word32
    , size :: Int
    , hasher :: h }

type Map k v = HashMap k v MurmurHash

instance (Show k, Show v) => Show (HashMap k v h) where
    show m = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " -> " ++ show v) $ toList m) ++ "}"

nextPo2 :: Int -> Int
nextPo2 0 = 1
nextPo2 x =  (^) 2 $ (ceiling :: Double -> Int) $ logBase 2 $ (fromIntegral :: Int -> Double) x

hash :: (Hashable k, Hasher h) => h -> Word32 -> k -> Word32
hash h m k = endHash (thenHash h k) .&. m

hashMap :: (Hashable k, Hasher h) => h -> Word32 -> (k, v) -> (Word32, (k, v))
hashMap h m (k, v) = (hash h m k, (k, v))

hashAccum :: Eq k => [(k, v)] -> (k, v) -> [(k, v)]
hashAccum l = nubBy (\(a,_) (b,_) -> a == b) . (:l)

-- | size must be a power of two
withHasherAndSize :: (Hashable k, Eq k, Hasher h) => h -> Int -> [(k, v)] -> HashMap k v h
withHasherAndSize h sz xs = HashMapCons
    { used = length xs
    , maxUsed = floor $ 0.8 * (fromIntegral :: Int -> Double) sz
    , mask = msk
    , hasher = h
    , size = sz
    , buckets = accumArray hashAccum [] (0, msk) $ map (hashMap h msk) xs}
    where msk = (fromIntegral $ sz - 1) :: Word32

withHasher :: (Hashable k, Eq k, Hasher h) => h -> [(k, v)] -> HashMap k v h
withHasher h xs = withHasherAndSize h (nextPo2 $ min 32 $ length xs) xs

new :: (Hashable k, Eq k) => [(k, v)] -> HashMap k v MurmurHash
new = withHasher newHash

toList :: HashMap k v h -> [(k, v)]
toList = concat . elems . buckets

grow :: (Hashable k, Eq k, Hasher h) => HashMap k v h -> HashMap k v h
grow m = withHasherAndSize (hasher m) (2 * size m) $ toList m

insert' :: (Hashable k, Eq k, Hasher h) => HashMap k v h -> [(k, v)] -> HashMap k v h
insert' m xs = m { used = used m + length xs,
                   buckets = accum hashAccum (buckets m) $ map (hashMap (hasher m) (mask m)) xs }

insert :: (Hashable k, Eq k, Hasher h) => HashMap k v h -> [(k, v)] -> HashMap k v h
insert m xs = if len + used m < maxUsed m then insert' m xs else insert' (grow m) xs
    where len = length xs

(!?) :: (Hashable k, Eq k, Hasher h) => HashMap k v h -> k -> Maybe v
(!?) m k = fmap snd $ find ((==k) . fst) $ buckets m!i where i = hash (hasher m) (mask m) k

(!!!) :: (Hashable k, Eq k, Hasher h) => HashMap k v h -> k -> v
(!!!) m = fromJust . (m !?)
