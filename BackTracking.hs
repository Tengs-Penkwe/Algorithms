-- import Test.HUnit
-- import Test.QuickCheck

dict :: [String]
dict = ["cat", "dog", "rat", "bat", "mat", "hat"]

isWord :: String -> Bool
isWord word = elem word dict

-- splitString :: String -> [[String]]
-- splitString 

additionChain :: Integer -> [Integer]
additionChain target
    | target == 0 = []
    | target == 1 = [1]
    | even target = additionChain (target `div` 2) ++ [target]
    | otherwise = additionChain (target - 1) ++ [target]

--- Common Sequence

-- | Longest Common Subsequence
--
-- >>> lcs "hsdfdfaaapp" "dsfsfaaaaaa"
-- "sffaaa"
lcs :: Eq a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs (ah:at) (bh:bt)
    | ah == bh = ah : lcs at bt
    | otherwise = maxlength (lcs at (bh : bt)) (lcs (ah : at) bt)
    where maxlength a b = if length a > length b then a else b


-- | Common Super Sequence
--
-- >>> csc "hsdfdfapp" "dsfsfaaaaaa"
-- "dhsdfsdfaaaaaapp"
csc :: Eq a => [a] -> [a] -> [a]
csc [] l = l
csc l [] = l
csc (ah:at) (bh:bt) 
    | ah == bh = ah : csc at bt
    | otherwise = minlength (ah : csc at (bh:bt)) (bh : csc (ah:at) bt)
    where minlength a b = if length a < length b then a else b

-- | Longest Bitonic Subsequence
--
-- Bitonic: Increasing then Decreasing
-- >>> lbs [1, 11, 2, 10, 4, 5, 2, 1]
-- 6
-- >>> lbs []
-- 0
-- >>> lbs [1]
-- 0
-- >>> lbs [1, 2]
-- 0
-- >>> lbs [1, 2, 3]
-- 0
-- >>> lbs [3, 2, 1]
-- 0
-- >>> lbs [1, 2, 2]
-- 0
lbs :: Ord a => [a] -> Integer
lbs [] = 0
lbs [_] = 0
lbs [_, _] = 0
lbs [x, y, z] 
    | x < y && y > z = 3
    | otherwise = 0
lbs lst
    = lbsIncrease lst
    -- = max4 (lbs ai) (1 + lbs at) (1 + lbs (ah:ai)) (2 + lbs ai)
    -- where
    --     ai = init at
    --     al = last at
    --     max4 a b c d = max a (max b (max c d))
        
lbsIncrease :: Ord a => [a] -> Integer
lbsIncrease [] = -1000
lbsIncrease [a] = -1000
lbsIncrease [a1, a2]
    | a1 < a2 = -1000
    | otherwise = 1
lbsIncrease (a1:a2:at)
    | a1 < a2   = 1 + lbsIncrease (a2:at)
    | a1 == a2  = lbsIncrease (a2:at)
    | otherwise = max (lbsIncrease (a1:at)) (1 + lbsDecrease (a2:at))

lbsDecrease :: Ord a => [a] -> Integer
-- lbsDecrease [] = 0
lbsDecrease [a] = 0
lbsDecrease [a1, a2]
    | a1 > a2 = 1
    | otherwise = 0
lbsDecrease (a1:a2:at)
    | a1 > a2   = 1 + lbsDecrease (a2:at)
    | otherwise = lbsDecrease (a1:at)
        

-- Increase or descrease
-- lbs' :: Ord a => [a] -> Bool -> Integer
-- lbs' [] False = -100
-- lbs' [] True = -100
-- lbs' [a] False = 0
-- lbs' [a] True = -100
-- lbs' (a1:a2:at) isIncrease
--     | isIncrease && a1 < a2 = 1 + max (lbs' (a2:at) True) (lbs' (a2:at) False)
--     | isIncrease && a1 >= a2  = max (lbs' (a2:at) True) (1 + lbs' (a1:at) False)
--     | not isIncrease && a1 > a2 = 1 + lbs' (a2:at) False
--     | not isIncrease && a1 <= a2  = lbs' (a2:at) False
--     


-- | Longest Osciilating Subsequence
--
-- >>> los [1, 11, 2, 10, 4, 5, 2, 1]
-- 7
-- >>> los [1, 2]
-- 2
-- >>> los [1, 2, 3]
-- 2
-- >>> los [3, 2, 1]
-- 0
-- >>> los [1, 3, 1]
-- 3
los :: Ord a => [a] -> Integer
los [] = 0
los [_] = 0
los [a1, a2] 
    | a1 < a2 = 2
    | otherwise = 0
los (a1:a2:at)
    | a1 < a2   = max (1 + los' (a2:at)) (los at)
    | otherwise = max (los (a2:at)) (los at)

los' :: Ord a => [a] -> Integer
los' [] = 0
los' [_] = 1
los' [a1, a2] 
    | a1 > a2 = 2
    | otherwise = 1
los' (a1:a2:at)
    | a1 > a2   = max (1 + los (a2:at)) (los at)
    | otherwise = max (los (a2:at)) (los at)

-- |is Subsequence
--
-- >>> isSub "abc" "ahbgdc"
-- True
-- >>> isSub "axc" "ahbgdc"
-- False
-- >>> isSub "abc" "asfdfbfsdfdfsf"
-- False
isSub :: Eq a => [a] -> [a] -> Bool
isSub [] _ = True
isSub _ [] = False
isSub (ah:at) (bh:bt) 
    | ah == bh  = isSub at bt
    | otherwise = isSub (ah:at) bt

-- | Longest Subequences of Y that's not a Supersequence of X
--
-- >>> leastRemove "abc" "ahbgdc"
-- 1
-- >>> leastRemove "ppap" "penpineappleapplepen"
leastRemove :: Eq a => [a] -> [a] -> Integer
leastRemove [] _ = 0
leastRemove _ [] = 10000
leastRemove (xh:xt) (yh:yt)
    | xh == yh =
        if isSub xt yt 
        then min (1 + leastRemove xt yt) (leastRemove (xh:xt) yt)
        else 1
    | otherwise = leastRemove (xh:xt) yt


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)
-- | Construct AVL Tree

avlInsert :: (Ord a) => a -> Tree a -> Tree a
avlInsert x Empty = Node x Empty Empty
avlInsert x (Node y left right)
    | x >= y = Node y left (avlInsert x right)
    | otherwise = Node y (avlInsert x left) right

depth :: Tree a -> Integer
depth Empty = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

-- We need to balance the tree
balance :: Tree a -> Tree a
balance Empty = Empty
balance (Node x left right) = error "Not Implemented"



