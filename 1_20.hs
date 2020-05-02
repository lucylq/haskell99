-- Problems 1 - 20: Lists

-- 1
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2: find the second last element
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "One element in list"
myButLast (x:xs) = case xs of 
                        [_] -> x
                        _ -> myButLast xs

-- 3: find the kth element
elementAt :: [a] -> Int -> a 
elementAt [] n = error "index out of range"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == reverse xs

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8: eliminate consecutive duplicates 
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if (x == y) then compress (x:xs) else x:compress(y:xs)

-- 9: pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:xs) = pack' [x] xs

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' x [] = [x]
pack' (x:xs) [y] = if (x == y) then [(x:y:xs)] else (x:xs):[[y]]
pack' (x:xs) (y:ys) = if (x == y) then pack' (y:x:xs) (ys) else (x:xs):pack' [y] ys

-- 10: run length encoding data compression. duplicates are encoded as (N E)
encode :: Eq a => [a] -> [(Int, a)]
encode x = encode' $ pack x

encode' :: Eq a => [[a]] -> [(Int, a)]
encode' [] = []
encode' (x:xs) = (myLength x, head x):encode' xs 

-- 11: modified run-length encoding. unique elements not encoded.
data Encoded a = Single a | Multiple (Int, a) deriving Show
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified x = encodeModified' $ pack x

encodeModified' :: Eq a => [[a]] -> [Encoded a] 
encodeModified' [] = []
encodeModified' (x:xs) = case (myLength x) of
                            1 -> (Single $ head x) : encodeModified' xs
                            n -> (Multiple (n, head x)) : encodeModified' xs

-- 12: decode modified run-length encoding.
-- decodeModified :: Eq a => [(Encoded a)] -> [a]
-- decodeModified [] = []
-- decodeModified 

