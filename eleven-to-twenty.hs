import Text.Printf (errorBadArgument)
import Debug.Trace (trace)

-- Help functions
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = takeWhile (== x) (x:xs) : pack (dropWhile (== x) xs)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = package $ pack xs
    where
        package :: [[a]] -> [(Int, a)]
        package [] = []
        package (x:xs) = (length x, head x) : package xs

-- (11) Modified run-length encoding.
-- need to load 'one-to-ten.hs' to use funcitons
data EncodeData a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [EncodeData a]
encodeModified = map (\x -> if fst x == 1 then Single $ snd x else uncurry Multiple x) . encode
-- In the function to map we get a (Int, a) which we transform to a EncodeData type
-- Since it is done with map, then it can be done with foldr too

encodeModified' :: Eq a => [a] -> [EncodeData a]
encodeModified' = map f . encode
    where
        f (1,x) = Single x
        f (n,x) = Multiple n x
-- A cleaner looking map where we can pattern match the f function


-- (12) Decode a run-length encoded list.
decodeModified :: [EncodeData a] -> [a]
decodeModified = foldr f []
    where
        f (Single x) = (:) x
        f (Multiple n x) = (++) (replicate n x)

decodeModified' :: [EncodeData a] -> [a]
decodeModified' = concatMap f
    where
        f (Single x) = [x]
        f (Multiple n x) = replicate n x
-- Without (concat . map) or (concatMap) we get a nested list
-- [[1,1,1],[2],[3,3]]

decodeModified'' :: [EncodeData a] -> [a]
decodeModified'' [] = []
decodeModified'' ((Single x):xs) = x : decodeModified'' xs
decodeModified'' ((Multiple 2 x):xs) = x : x: decodeModified'' xs
decodeModified'' ((Multiple n x):xs) = x : decodeModified'' (Multiple (n-1) x:xs)
-- We can specify pattern match with data types by using two paranthesies.


-- (13) Run-length encoding of a list (direct solution).
encodeDirect :: Eq a => [a] -> [EncodeData a]
encodeDirect xs = foldr f [Single (last xs)] (init xs)
    where
        f :: Eq a => a -> [EncodeData a] -> [EncodeData a]
        f x [] = [Single x]
        f x (Single a : accs)
            | x == a     = Multiple 2 x : accs
            | otherwise  = Single x : Single a : accs
        f x (Multiple n a : accs)
            | x == a     = Multiple (n+1) x : accs
            | otherwise  = Single x : Multiple n a : accs
-- First iteration below was to convert count all items and save the one item. Then
-- I transformed it to handle EncodeData instead.
        -- f :: Eq a => a -> [(Int, a)] -> [(Int, a)]
        -- f x [] = [(1, x)]
        -- f x acc
        --     | x == snd (head acc) = (fst (head acc) +1, snd (head acc)) : tail acc
        --     | otherwise = (:) (1,x) acc

encodeDirect' :: Eq a => [a] -> [EncodeData a]
encodeDirect' [] = []
encodeDirect' (x:xs)
    | count == 1  = Single x : encodeDirect' xs
    | otherwise   = Multiple count x : encodeDirect' rest
    where
        (sequence, rest) = span (==x) xs
        count = 1 + length sequence


-- (14) Duplicate the elements of a list. 
dupli :: [a] -> [a]
dupli = foldr (\x acc -> replicate 2 x ++ acc) []

dupli' :: [a] -> [a]
dupli' = concatMap (replicate 2)

dupli'' :: [a] -> [a]
dupli'' xs = concat [[x,x] | x <- xs]


-- (15) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs
-- pointfree style: flip $ concatMap . replicate


-- (16) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs 0 = errorBadArgument "n cannot be zero"
dropEvery xs n = reverse $ foldl (\acc x -> if snd x `mod` n == 0 then acc else fst x : acc) [] (zip xs [1..])
--f (f ( f [] (a,1 )) (b,2)) (c,3) == fst a : []
--f (f [a] (b,2)) (c,3) ==  [a]
--f [a] (c,3) == [c,a]

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = map fst $ filter (\(x,i) -> i `mod` n /= 0) $ zip xs [1..]


-- (17) Split a list into two parts; the length of the first part is given. Do not use any predefined predicates. 
split :: [a] -> Int -> ([a], [a])
split xs n = splitAtN [] xs n 0
    where
        splitAtN :: [a] -> [a] -> Int -> Int -> ([a],[a])
        splitAtN xs [] n i = (xs, [])
        splitAtN xs (y:ys) n i
            | n == i    = (xs, y:ys)
            | otherwise = splitAtN (xs ++ [y]) ys n (i+1)


-- (18) Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) i k
    | k < i || i < 1 || k < 1   = errorBadArgument "k < i"
    | k == 0                    = []
    | i <= 1                    = x : slice xs 0 (k-1)
    | otherwise                 = slice xs (i-1) (k-1)
-- Should do this with Maybe!

slice' :: [a] -> Int -> Int -> [a]
slice' xs i k = take k $ drop (i-1) xs


-- (19)  Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs n = uncurry (flip (++)) $ splitAt shiftNrMod xs
    where
        shiftNr = if n < 0 then length xs + n else n
        shiftNrMod = n `mod` length xs

rotate' :: [a] -> Int -> [a]
rotate' xs n = take shiftNr . drop (n `mod` shiftNr) . cycle $ xs
    where
        shiftNr = length xs
-- rotate [1,2,3] 1 == take 3 . drop (1%3) . cycle ([1,2,3])
-- == take 3 . drop 1 [1,2,3,1,2,3,...]
-- == take 3 [2,3,1,2,3,...]
-- == [2,3,1]


-- (20) Remove the K'th element from a list.
removeAt :: Show a => Int -> [a] -> (a, [a])
removeAt _ [] = error "index out of bound"
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (removed, x : keep)
    where
        (removed, keep) = removeAt (n-1) xs