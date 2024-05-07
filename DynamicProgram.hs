import Test.HUnit
import Test.QuickCheck

import Data.Array
import Data.List (find, minimumBy, sort)
import Data.Ord (comparing)

-- editDistance :: Ord a => [a] -> [a] -> Integer
-- editDistance xs ys = table ! (m, n)
--     where 
--         m = length xs
--         n = length ys

-- Excercise 3.1 The Money Exchange Problem of Nadiria in Antarctica
money :: [Integer]
-- money = [1, 4, 7, 13, 28, 52, 91, 365]
money = [365, 91, 52, 28, 13, 7, 4, 1]

greedyChange :: Integer -> Maybe [Integer]
greedyChange amount
    | amount <= 0 = Nothing
    | otherwise = case change of
        Nothing -> Just (replicate (fromIntegral num) denominations)
        Just lst -> Just (replicate (fromIntegral num) denominations ++ lst)
    where 
        denominations = case find (<= amount) money of
            Just x -> x
            Nothing -> last money
        num = amount `div` denominations
        remaining = amount `mod` denominations
        change = greedyChange remaining

bestChange :: Integer -> Maybe [Integer]
bestChange amount
    | amount <= 0 = Nothing
    | otherwise = Just $ bestChange' amount

-- Ex 3.1 (b) The recursive one
-- This recusive function is not efficient
-- bestChange' :: Integer -> [Integer]
-- bestChange' amount
--     | amount <= 0 = []
--     | otherwise = minimumBy (comparing length) [ n : bestChange' (amount - n) | n <- money, n <= amount]
-- Ex 3.1 (c) The dyanmic programming one
bestChange' :: Integer -> [Integer]
bestChange' amount = table ! amount
    where 
        table = listArray (0, amount) [change n | n <- [0..amount]]
        change 0 = []
        change n = minimumBy (comparing length) [ m : table ! (n - m) | m <- money, m <= n]

-- Ex 3.1 (a)
-- Find the minimum amount to make the greedy algorithm not the best
smallestGreedyNotBest :: Integer -> Maybe Integer
smallestGreedyNotBest maxSearch
    = case filter (\x -> greedyChange x /= bestChange x) [1..maxSearch] of
        [] -> Nothing
        (x:_) -> Just x

-- Ex 3.2 The Word Break Problem
dict :: [String]
dict = sort ["cat", "dog", "rat", "bat", "mat", "hat", "art", "is", "oil", "toil", "artist", "bot", "heart", "hand", "sat", "urns", "pin", "start", "raps", "and", "rags", "lap"]

isWord :: String -> Bool
isWord word = word `elem` dict

-- Ex 3.2(a) find all possible splits of a string
allSplits :: String -> [[String]]
allSplits "" = [[]]
allSplits string
    = [word : rest | n <- [1..length string],
                     let word = take n string, 
                     isWord word, 
                     rest <- allSplits (drop n string) ]
-- Time complexity:
-- Worst case: O(n^n), every character and its combinations are valid words
-- Best case: O(n), no valid words

-- | Ex 3.2(b) find if 2 strings share the same split (index)
--
-- BOT·HEART·HAND·SAT·URNS·PIN
-- PIN·START·RAPS·AND·RAGS·LAP
--  
-- >>> sameSplit "bothearthandsaturnspin" "pinstartrapsandragslap" 
-- Just True
-- >>> sameSplit "bothe" "pinstartrapsandlagslapx" 
-- Nothing
sameSplit :: String -> String -> Maybe Bool
sameSplit str1 str2
    | length str1 /= length str2 = Nothing
    | otherwise = Just $ sameSplit' str1 str2

sameSplit' :: String -> String -> Bool
sameSplit' "" "" = True
sameSplit' str1 str2
    = or [ ok && restOk  | n <- [1..strlen], 
        let ok = isWord (take n str1) && isWord (take n str2),
        let restOk = sameSplit' (drop n str1) (drop n str2)
        ]
    where
        strlen = length str1
-- Time complexity:
-- Same as allSplits, but with a constant factor of 2 ?

allSameSplit :: String -> String -> Maybe [([String], [String])]
allSameSplit str1 str2
    | length str1 /= length str2 = Nothing
    | otherwise = Just $ allSameSplit' str1 str2

allSameSplit' :: String -> String -> [([String], [String])]
allSameSplit' "" "" = [([], [])]
allSameSplit' str1 str2
    = [ (word1 : rest1, word2 : rest2) | n <- [1..strlen], 
        let word1 = take n str1, let word2 = take n str2,
        isWord word1, isWord word2,
        let rest1 = drop n str1, let rest2 = drop n str2,
        (rest1, rest2) <- allSameSplit' rest1 rest2
        ]
    where
        strlen = length str1

-- Ex 3.3 Find max sum and product of a CONTIGUOUS subsequence

-- | Max Sum
--
-- >>> maxContSum [-6, 12, -7, 0, 14, -7, 5]
-- 19 
maxContSum :: [Integer] -> Integer
maxContSum [] = 0
maxContSum nums = max (maximum table) 0
    where 
        numlen = length nums
        table = [contSum n1 n2 | n1 <- [1..numlen], n2 <- [n1..numlen]]
        contSum n1 n2 
            | n1 == n2 = nums !! (n1 - 1)
            | otherwise = contSum n1 (n2 - 1) + nums !! (n2 - 1)
-- The time complexity is O(n^2), optimized brutal force

kadaneMaxContSum :: [Integer] -> Integer
kadaneMaxContSum nums = kadaneMaxContSum' nums 0 0

kadaneMaxContSum' :: [Integer] -> Integer -> Integer -> Integer
kadaneMaxContSum' [] gMax lMax = gMax
kadaneMaxContSum' (x:xs) gMax lMax 
    -- lMax is always bigger than 0
    | x + lMax < 0 = kadaneMaxContSum' xs gMax 0
    | otherwise = kadaneMaxContSum' xs (max gMax $ x + lMax) (x + lMax)
-- Time complexity: O(n) 


-- | Max Product
--
-- >>> maxContProduct [2, 3, -2, 4, -8]
-- 384
-- >>wkh unpackWord32X16#d
-- > maxContProduct [-6, 12, -7, 0, 14, -7, 5]
-- 504
maxContProduct :: [Integer] -> Integer
maxContProduct nums = maxContProduct' nums 0 1 1

maxContProduct' :: [Integer] -> Integer -> Integer -> Integer -> Integer 
maxContProduct' [] gMax lMax lnMax = gMax
maxContProduct' (x:xs) gMax lMax lnMax
    | nlMax < -1 = maxContProduct' xs (max gMax nlnMax) nlMax nlnMax
    | nlMax > 1  = maxContProduct' xs (max gMax nlMax) nlMax nlMax
    -- [-1, 1]
    | otherwise  = maxContProduct' xs (max gMax lnMax) 1 1
    where nlMax = x * lMax
          nlnMax = x * lnMax

brutalMaxContProduct :: [Integer] -> Integer
brutalMaxContProduct [] = 0
brutalMaxContProduct nums = max (maximum table) 0
    where 
        numlen = length nums
        table = [product n1 n2 | n1 <- [1..numlen], n2 <- [n1..numlen]]
        product n1 n2 
            | n1 == n2 = nums !! (n1 - 1)
            | otherwise = product n1 (n2 - 1) * nums !! (n2 - 1)
 
-- Ex 3.4 Maximum Subarray 

-- Ex 3.4 (a)

------------------- Testing -------------------
-----------------------------------------------

add :: Int -> Int -> Int
add x y = x + y

-- HUnit test cases
test1 = TestCase (assertEqual "add 1 2" 3 (add 1 2))
test2 = TestCase (assertEqual "add 5 (-2)" 3 (add 5 (-2)))
test3 = TestCase (assertEqual "maxContSum" 6 (maxContSum [1, 2, 3]))
test4 = TestCase (assertEqual "maxContSum" 7 (maxContSum [1, 2, 3, -3, 4]))
test5 = TestCase (assertEqual "maxContSum" 9 (maxContSum [1, 2, 3, -7, 9]))
test3b_1 = TestCase (assertEqual "edge case" 1 (maxContProduct [1]))
test3b_2 = TestCase (assertEqual "edge case" 1 (maxContProduct [-1, 1]))
test3b_3 = TestCase (assertEqual "edge case" 0 (maxContProduct [-1]))


-- Grouping HUnit test cases
tests = TestList
    [
        TestLabel "test1" test1,
        TestLabel "test2" test2,
        TestLabel "maxContSum" test3,
        TestLabel "maxContSum" test4,
        TestLabel "maxContSum" test5
    ]

-- Ex 3.2(b)
prop_exchangeString s1 s2 = sameSplit s1 s2 == sameSplit s2 s1

-- Ex 3.2(c)
swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y, x)
prop_exchangeStringPair s1 s2 =
    case (allSameSplit s1 s2, allSameSplit s2 s1) of
        -- Do the order of the splits matter?
        (Just splits1, Just splits2) ->
            let swappedParis1 = map swapPair splits1
            in swappedParis1 == splits2
        (Nothing, Nothing) -> True
        _ -> False

-- Ex 3.3(a)
prop_maxContSum nums = maxContSum nums == kadaneMaxContSum nums
prop_maxContProduct nums = maxContProduct nums == brutalMaxContProduct nums

runQuickCheck = do
    quickCheck prop_exchangeString
    quickCheck prop_exchangeStringPair
    quickCheck prop_maxContSum
    quickCheck prop_maxContProduct

-- Main runs the tests
main :: IO ()
main = do
    -- Run HUnit tests
    putStrLn "Running HUnit tests:"
    _ <- runTestTT tests
    putStrLn ""

    -- Run QuickCheck tests
    putStrLn "Running QuickCheck tests:"
    runQuickCheck
