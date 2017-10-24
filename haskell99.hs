#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}

import Test.HUnit
import System.IO (stderr)
import System.Environment (getEnv)
import System.Random (RandomGen, mkStdGen, randomRs)
import qualified Data.List as L (groupBy, nub, permutations, sort, subsequences, sortOn)
import qualified Data.Set as Set (toList, fromList)

-- 1) last element of a list

myLast :: [x] -> x
myLast []     = error "no"
myLast [x]    = x
myLast (_:xs) = myLast xs

testMyLast :: Test
testMyLast =
  test [ 4   ~=? myLast [1 .. 4]
       , 'd' ~=? myLast ['a' .. 'd']]

-- 2) Find the last but one element of a list.

myButLast :: [x] -> x
myButLast []     = error "empty"
myButLast [_]    = error "not enough"
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

testMyButLast :: Test
testMyButLast =
  test [ 3   ~=? myButLast [1 .. 4]
       , 'c' ~=? myButLast ['a' .. 'd']]

-- 3) Find the K'th element of a list. The first element in the list
--    is number 1.

elementAt :: [x] -> Int -> x
elementAt [] _     = error "no"
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n - 1)

testElementAt :: Test
testElementAt =
  test [ 2   ~=? elementAt [1, 2, 3] 2
       , 'e' ~=? elementAt "haskell" 5]

-- 4) Find the number of elements of a list.

myLength :: [x] -> Int
--
-- myLength [] = 0
-- myLength (x:xs) = 1 + myLength xs
--
-- myLength xs = foldr (\x -> (+) 1) 0 xs
myLength = foldr (\_ -> (+) 1) 0

testMyLength :: Test
testMyLength =
  test [  3 ~=? myLength [123, 456, 789]
       , 13 ~=? myLength  "Hello, world!"]

-- 5) Reverse a list

myReverse :: [a] -> [a]
-- myReverse []     = []
-- myReverse (c:cs) = myReverse cs ++ [c]
--
--myReverse = foldl (\xs x -> x:xs) []
myReverse = foldl (flip (:)) []

testMyReverse :: Test
testMyReverse =
  test [ [4,3,2,1] ~=? myReverse [1..4]
       , "edcba"   ~=? myReverse "abcde"]

-- 6) Find out whether a list is a palindrome.

isPalindrome :: Eq x => [x] -> Bool
isPalindrome xs = xs == reverse xs

testIsPalindrome :: Test
testIsPalindrome =
  test
    [ False ~=? isPalindrome [1, 2, 3]
    , True  ~=? isPalindrome "madamimadam"
    , True  ~=? isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1]]

-- 7) Flatten a nested list structure.

data NestedList a
  = Elem a
  | List [NestedList a]

myFlatten :: NestedList a -> [a]

myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

testMyFlatten :: Test
testMyFlatten =
  test
    [ [5]           ~=? myFlatten (Elem 5)
    , [3 .. 4]      ~=? myFlatten (List [Elem 3, Elem 4])
    , [1 .. 5]      ~=? myFlatten myFlattenList
    , ([] :: [Int]) ~=? myFlatten (List [])] -- TODO: why the type hint?

myFlattenList :: NestedList Integer
myFlattenList = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

-- 8) Eliminate consecutive duplicates of list elements.

compress :: Eq a => [a] -> [a]
compress =
  foldr
    (\x xs ->
       if null xs || x /= head xs
         then x : xs
         else xs)
    []

testCompress :: Test
testCompress = test ["abcade" ~=? compress testUnencoded]

testUnencoded :: String
testUnencoded = "aaaabccaadeeee"

-- 9) Pack consecutive duplicates of list elements into sublists.

pack :: Eq a => [a] -> [[a]]
pack = foldr helper []
  where
    helper x [] = [[x]]
    helper x result@(y:ys)
      | x == head y = (x : y) : ys
      | otherwise   = [x] : result

testPack :: Test
testPack = test [["aaaa", "b", "cc", "aa", "d", "eeee"] ~=? pack testUnencoded]

-- 10) Run-length encoding of a list.

encode :: Eq a => [a] -> [(Int, a)]
encode xs = fmap (
  \x -> (length x, head x)) (pack xs)

testEncode :: Test
testEncode = test [testPacked ~=? encode testUnencoded]

testPacked :: [(Int, Char)]
testPacked = [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

-- 11) Modified run-length encoding

data RLE a = Multiple Int a
           | Single a
           deriving (Eq, Show, Ord)

encodeModified :: Eq a => [a] -> [RLE a]
encodeModified xs =
  fmap
    (\str ->
       let len = length str
           c = head str
       in if len == 1
            then Single c
            else Multiple len c)
    (pack xs)

testEncodeModified :: Test
testEncodeModified = test [testEncoded ~=? encodeModified testUnencoded]

testEncoded :: [RLE Char]
testEncoded =
  [ Multiple 4 'a'
  , Single 'b'
  , Multiple 2 'c'
  , Multiple 2 'a'
  , Single 'd'
  , Multiple 4 'e'
  ]

-- 12 Decode a run-length encoded list.

decodeModified :: Eq a => [RLE a] -> [a]
decodeModified [] = []
decodeModified (Single c:rest)     = c : decodeModified rest
-- decodeModified (Multiple n c:rest) = (take n (cycle [c])) ++ decodeModified rest
-- decodeModified (Multiple n c:rest) = take n (cycle [c])   ++ decodeModified rest
-- decodeModified (Multiple n c:rest) = take n (repeat c)    ++ decodeModified rest
-- decodeModified (Multiple n c:rest) = replicate n c        ++ decodeModified rest
decodeModified (Multiple n c:rest) = replicate n c ++ decodeModified rest

testDecodeModified :: Test
testDecodeModified = test [testUnencoded ~=? decodeModified testEncoded]

-- 13) Run-length encoding of a list (direct solution).

encodeDirect :: Eq a => [a] -> [RLE a]
encodeDirect [] = []
encodeDirect (x:xs) = val : encodeDirect tl
  where (hd, tl) = span (== x) xs
        len      = 1 + length hd
        val      = if len == 1 then Single x else Multiple len x

testEncodeDirect :: Test
testEncodeDirect = test [testEncoded ~=? encodeDirect testUnencoded]

-- 14) Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

testDupli :: Test
testDupli = test [[1, 1, 2, 2, 3, 3] ~=? dupli [1, 2, 3]]

-- 15) Replicate the elements of a list a given number of times.

repli :: Int -> [xs] -> [xs]
-- basic recursion:
--   repli n [] = []
--   repli n (x:xs) = replicate n x ++ repli n xs
-- switch to foldr
--   repli n xs = foldr ((++) . replicate n) [] xs
-- eta reduce
repli n = foldr ((++) . replicate n) []

testRepli :: Test
testRepli = test ["aaabbbccc" ~=? repli 3 "abc"]

-- 16) Drop every N'th element from a list.

dropEvery :: [xs] -> Int -> [xs]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

testDropEvery :: Test
testDropEvery = test ["abdeghk" ~=? dropEvery "abcdefghik" 3]

-- 17) Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs) -- initial good version
-- split xs 0 = ([], xs)            -- dumb version
-- split [] _ = ([], [])            -- can't really happen
-- split (x:xs) n = let (a,b) = split xs (n-1)
--                  in (x:a, b)

testSplit :: Test
testSplit = test [("abc", "defghik") ~=? split "abcdefghik" 3]

-- 18) Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
-- slice xs from to = fmap (\n -> xs !! (n-1)) [from..to]
slice xs from to = take (to-2) $ drop (from-1) xs

testSlice :: Test
testSlice = test ["cdefg" ~=? slice "abcdefghik" 3 7]

-- 19) Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate xs n = let len = if n > 0 then n else length xs + n
              in drop len xs ++ take len xs

testRotate :: Test
testRotate =
  test [ "defghabc" ~=? rotate "abcdefgh" 3,
         "ghabcdef" ~=? rotate "abcdefgh" (-2)]

-- 20) Remove the K'th element from a list.

removeAt :: Int -> [a] -> [a]
removeAt 1 (_:xs) = xs          -- 1 based? why?
removeAt _ []     = []
removeAt n (x:xs) = x : removeAt (n-1) xs

testRemoveAt :: Test
testRemoveAt = test ["acd" ~=? removeAt 2 "abcd"]

-- 21) Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt y (x:xs) 1 = y:x:xs
insertAt _ [] _ = [] -- can't happen
insertAt y (x:xs) n = x : insertAt y xs (n-1)

testInsertAt :: Test
testInsertAt = test ["aXbcd" ~=? insertAt 'X' "abcd" 2]

-- 22) Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
-- range from to = from : (if from < to then range (from + 1) to else [])
range from to = if from < to then [from..to] else reverse (range to from)

testRange :: Test
testRange =
  test [ [4, 5, 6, 7, 8, 9] ~=? range 4 9
       , [9, 8, 7, 6, 5, 4] ~=? range 9 4]

-- 23) Extract a given number of randomly selected elements from a list.

rndSelect :: RandomGen g => [a] -> Int -> g -> [a]
rndSelect xs n g = take n [xs !! x | x <- L.nub (randomRs (0, length xs - 1) g)]

testRndSelect :: Test
testRndSelect = test ["fdacebgh" ~=? rndSelect "abcdefgh" 8 (mkStdGen 42)]

-- 24) Lotto: Draw N different random numbers from the set 1..M.

randomList :: RandomGen g => Int -> Int -> g -> [Int]
randomList n max = rndSelect [1..max] n

testRandomList :: Test
testRandomList =
  test [[43, 41, 12, 46, 11, 19] ~=? randomList 6 49 (mkStdGen 42)]

-- 25) Generate a random permutation of the elements of a list.

randomPerm :: Eq a => RandomGen g => [a] -> g -> [a]
randomPerm xs = rndSelect xs (length xs)

testRandomPerm :: Test
testRandomPerm = test ["fdbeca" ~=? randomPerm "abcdef" (mkStdGen 42)]

-- 26) Generate the combinations of K distinct objects chosen from the
--     N elements of a list

combinations :: Ord a => Int -> [a] -> [[a]]
combinations k xs = L.sort $ filter ((k==).length) $ L.subsequences xs

testCombinations :: Test
testCombinations = test [["abc", "abd", "acd", "bcd"] ~=? combinations 3 "abcd"]

-- 27) Group the elements of a set into disjoint subsets.

multisplit :: [Int] -> [a] -> [[a]]
multisplit [] _ = []
multisplit (n:ns) xs = take n xs : multisplit ns (drop n xs)

uniq :: Ord a => [a] -> [a]
uniq xs = Set.toList $ Set.fromList xs
-- uniq = fmap head . L.group . L.sort

group :: Ord a => [Int] -> [a] -> [[[a]]]
group ns xs = uniq $ fmap (fmap uniq . multisplit ns) $ L.sort $ L.permutations xs

testGroup :: Test
testGroup =
  test
    [ ["ab", "cd", "efg"] ~=? multisplit [2, 2, 3] "abcdefghi"
    , "imps"              ~=? uniq "mississippi"
    , testGroups          ~=? take 5 (group [2, 2, 1] "abcde")
    -- , 1260             ~=? length (group [2,3,4] "abcdefghi")
    -- , 756              ~=? length (group [2,2,5] "abcdefghi")
    ]

testGroups :: [[String]]
testGroups =
  [ ["ab", "cd", "e"]
  , ["ab", "ce", "d"]
  , ["ab", "de", "c"]
  , ["ac", "bd", "e"]
  , ["ac", "be", "d"]
  ]

-- 28) Sorting a list of lists according to length of sublists

lsort :: Ord a => [[a]] -> [[a]]
lsort = L.sortOn (\x -> (length x, x))

lfsort :: Ord a => [[a]] -> [[a]]
lfsort xs = concat $ lsort $ L.groupBy byLength $ lsort xs
  where byLength as bs = length as == length bs

testLSort :: Test
testLSort =
  test
    [ ["o", "de", "de", "mn", "abc", "fgh", "ijkl"] ~=? lsort testLSortList
    , ["ijkl", "o", "abc", "fgh", "de", "de", "mn"] ~=? lfsort testLSortList
    ]

testLSortList :: [String]
testLSortList = ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]

------------------------------------------------------------------------
-- tests:

tests :: Test
tests =
  test
    [ testMyLast
    , testMyButLast
    , testElementAt
    , testMyLength
    , testMyReverse
    , testIsPalindrome
    , testMyFlatten
    , testCompress
    , testPack
    , testEncode
    , testEncodeModified
    , testDecodeModified
    , testEncodeDirect
    , testDupli
    , testRepli
    , testDropEvery
    , testSplit
    , testSlice
    , testRotate
    , testRemoveAt
    , testInsertAt
    , testRange
    , testRndSelect
    , testRandomList
    , testRandomPerm
    , testCombinations
    , testGroup
    , testLSort
    ]

runTests :: Test -> IO (Counts, Int)
runTests tests = do
  term <- getEnv "TERM"
  let smart = "dumb" /= term
  runTestText (putTextToHandle stderr smart) tests

main :: IO (Counts, Int)
main = runTests tests
