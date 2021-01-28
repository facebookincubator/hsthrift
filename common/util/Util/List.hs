-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.List
  ( blend
  , chunk
  , count
  , diff
  , equalChunksOf
  , atMostChunksOf
  , splitChunks
  , chunkBy
  , mapHead
  , mapLast
  , mostCommonElem
  , replace
  , sortOnM
  , stripSuffix
  , uniq
  , uniqBy
  ) where

import Data.List
import Data.Ord (comparing)
import qualified Data.Set as Set

-- This is an O(n+m) implementation for computing difference between two lists,
-- when the order of the result is irrelevant.
-- Returns those elements in the first list which are not present in the second.
-- All occurences of repeated elements are removed.
-- eg. [1,1,2,4,3] `List.diff` [1,2] -> [3,4]
diff :: (Eq a, Ord a) => [a] -> [a] -> [a]
diff xs ys = Set.toList $ Set.fromList xs `Set.difference` Set.fromList ys

-- | Returns the number of elements that satisfy the predicate
count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs) = f x : xs
mapHead _ [] = []

mapLast :: (a -> a) -> [a] -> [a]
mapLast f = reverse . mapHead f . reverse

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)

sortOnM :: (Applicative f, Ord b) => (a -> f b) -> [a] -> f [a]
sortOnM f xs =
  map fst . sortOn snd <$> traverse (\x -> (x,) <$> f x) xs

uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList

uniqBy :: (a -> a -> Ordering) -> [a] -> [a]
uniqBy cmp = map head . groupBy (\a b -> cmp a b == EQ) . sortBy cmp

-- | Splits list into chunks of roughly equal size 'Int'
-- optimising for variance, which means the chunks can be bigger than the 'Int'
-- and the last chunk can be 50% bigger or 50% smaller than the others.
--
-- > equalChunksOf 25 [1..99] == [ [1..33], [34..66], [67..99]]
-- > equalChunksOf 34 [1..99] == [ [1..49], [50..99] ]
-- > equalChunksOf 49 [1..99] == [ [1..49], [50..99] ]
-- > equalChunksOf 50 [1..99] == [ [1..99] ]
-- > equalChunksOf 51 [1..99] == [ [1..99] ]
--
-- > equalChunksOf 24 [1..99] == [ [1..24], [25..48], [49..72], [73..99] ]
-- >   -- lengths are 24, 24, 24, 27 where last chunk is 3 larger.
-- > map length $ equalChunksOf 6 [1..81] == [6,6,6,6,6,6,6,6,6,6,6,6,9]
-- > last $ map length $ equalChunksOf 24 [1..348] == 36 -- 50% bigger
-- > last $ map length $ equalChunksOf 24 [1..349] == 13 -- 50% smaller
--
-- >
-- > equalChunksOf i [] = [ [] ]
-- > equalChunksOf 0 xs -- *** Exception: divide by zero
-- > equalChunksOf (-1) xs == [ xs ]
equalChunksOf :: Int -> [a] -> [[a]]
equalChunksOf n as = go as size []
  where
    size = length as
    bestLen = size `div` max 1 (size `div` n)
    go bs bsLen acc | bsLen <= bestLen * 3 `div` 2 = reverse $ bs:acc
    go bs bsLen acc =
      let (chunk, rest) = splitAt bestLen bs in
        go rest (bsLen - bestLen) $ chunk:acc

-- | Splits list into non-empty pieces which are at most 'Int' size limit.
-- The actual chunk size will often be smaller (see splitChunks), using
-- the minimum number of chunks to not go over the 'Int' size limit.  The size
-- of chunks will all be equal (if possible) and otherwise vary by at most 1.
--
-- > concat (atMostChunksOf i xs) == xs
-- > all (not . null) (atMostChunksOf i xs)
--
-- > atMostChunksOf 24 [1..99] == [[1..20],[21..40],[41..60],[61..80],[81..99]]
-- > atMostChunksOf 34 [1..99] == [ [1..33], [34..66], [67..99] ]
-- > atMostChunksOf 49 [1..99] == [ [1..33], [34..66], [67..99] ]
-- > atMostChunksOf 50 [1..99] == [ [1..50], [51..99] ]
-- > atMostChunksOf 51 [1..99] == [ [1..50], [51..99] ]
-- >
-- > atMostChunksOf i [] = [] -- never make an empty chunk by returning [ [] ]
-- > atMostChunksOf 1 [x,y,z] = [ [x], [y], [z] ]
-- > atMostChunksOf 0 [x,y,z] = [ [x,y,z] ] -- nonsense, non-empty list
-- > atMostChunksOf (-1) [x,y,z] = [ [x,y,z] ]-- nonsense, non-empty list
atMostChunksOf :: Int -> [a] -> [[a]]
atMostChunksOf i as
    | null as = []
    | i <= 0 = [as]
    | size <= i = [as]
    | otherwise = splitChunks pieces as
  where
    size = length as
    pieces = (size + pred i) `div` i   -- assert size <= pieces * i

-- | Splits list into at most 'Int' non-empty chunks of nearly equal size.
-- Initial larger chunks can be 1 longer than later smaller chunks.
--
-- > concat (splitChunks i xs) == xs
-- > all (not . null) (splitChunks i xs)
--
-- > splitChunks _ [] = [] -- never make an empty chunk by returning [ [] ]
-- > splitChunks 4 [x,y,z] = [ [x], [y], [z] ] -- only 3 non-empty chunks
-- > splitChunks 3 [x,y,z] = [ [x], [y], [z] ]
-- > splitChunks 2 [x,y,z] = [ [x,y], [z] ] -- larger groups first
-- > splitChunks 1 [x,y,z] = [ [x,y,z] ]
-- > splitChunks 0 [x,y,z] = [ [x,y,z] ] -- nonsense, non-empty list
-- > splitChunks (-1) [x,y,z] = [ [x,y,z] ] -- nonsense, non-empty list
splitChunks :: Int -> [a] -> [[a]]
splitChunks b as
    | null as = []
    | b <= 0 = [as]
    | size <= b = map (:[]) as
    | otherwise = go as sizes []
  where
    size = length as
    larges = size `mod` b   -- assert 0 <= larges < b
    smalls = b - larges     -- assert 0 < smalls <= b
    -- assert larges + smalls == b
    smaller = size `div` b  -- assert 1 <= smaller ; since 0 < b < size
    larger = succ smaller   -- assert 2 <= larger
    sizes = replicate larges larger ++ replicate smalls smaller
    -- assert 0 < length sizes == b
    -- assert sum sizes == size
    go [] _ acc = reverse acc      -- base case
    go bs (x:xs) acc =
      let (chunk, rest) = splitAt x bs -- assert 1 <= length chunk
      in go rest xs (chunk:acc)
    go bs [] acc = reverse $ bs:acc -- should not happen

-- | @chunkBy n f xs@: divide @xs@ into chunks of at most size @n@,
-- where the size of an element @x@ of @xs@ is given by @f x@. If the size of
-- any element is greater than @n@, then it will be in a chunk on its own.
--
-- > chunkBy 4 length ["ab", "cde", "f", "ghijk"]
-- > [["ab"],["cde","f"],["ghijk"]]
--
chunkBy :: Int -> (a -> Int) -> [a] -> [[a]]
chunkBy _ _ [] = []
chunkBy chunkSize getSize (x:xs)
  | chunkSize <= 0 = [x:xs]
  | otherwise = go (getSize x) [x] xs
  where
    -- invariant: chunk has at least one element
    go _ chunk [] = [reverse chunk]
    go n chunk (x:xs)
      | n + xSize > chunkSize = reverse chunk : go xSize [x] xs
      | otherwise = go (n + xSize) (x:chunk) xs
      where xSize = getSize x

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys
  | Just zs <- stripPrefix (reverse xs) (reverse ys) = Just (reverse zs)
  | otherwise = Nothing

-- | Returns the most common element in the list.
-- Throws if given an empty list.
mostCommonElem :: Ord a => [a] -> a
mostCommonElem = head . maximumBy (comparing length) . group . sort

blend :: [a] -> [a] -> [a]
blend [] _ = []
blend _ [] = []
blend (x:xs) (y:ys) = x:y:blend xs ys

-- | Divide a range @0..(k-1)@ into @n@ chunks. Each pair in the result
-- list is the (start,length) of a chunk.
chunk
  :: Int                                -- ^ @n@
  -> Int                                -- ^ @k@
  -> [(Int,Int)]                        -- ^ @[(start,length)]@
chunk !n !k = zip (scanl (+) 0 lens) lens
  where
    lens = replicate r (q+1) ++ if q == 0 then [] else replicate (n-r) q
    !q = k `div` n
    !r = k `mod` n
