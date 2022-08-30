import GHC.List (errorEmptyList)
import GhcPlugins (xFlags, aP_STACK_SPLIM)
import Data.List (group)


-- (1) Find the last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = errorEmptyList "myLast"

myLast' :: [a] -> a
myLast' l = foldr1 const $ reverse l
-- myLast' l = head . reverse l (faster than the above solution)
-- foldr1 const (reverse [1,2,3,4]) == foldr1 const [4,3,2,1] == const (const (const 4 3) 2 ) 1 
-- const :: a -> b -> a

myLast'' :: [a] -> a
myLast'' = foldr1 $ const id
-- flip const :: a -> b -> b


-- (2) Find the last but one element of a list
myButLast :: [a] -> a
myButLast (x:[xs]) = x
myButLast (_:xs) = myButLast xs
myButLast _ = errorWithoutStackTrace "empty or single element in list"

myButLast' :: [a] -> a
myButLast' = last . init
-- last (init xs) == last (init [1,2,3,4]) == last [1,2,3] == 3

myButLast'' :: [a] -> Maybe a
myButLast'' = snd . foldl (\(a,b) x -> (Just x, a)) (Nothing, Nothing)
-- f (f (f (f (Nothing,Nothing) 1) 2) 3) 4
-- (Just 1, Nothing) 2
-- (Just 2, Just 1) 3
-- (Just 3, Just 2) 4
-- (Just 4, Just 3)


-- (3) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (x:xs) i = elementAt xs (i-1)
elementAt [] _ = errorEmptyList "elementAt"
-- Does not work on infinate lists (elementAt [1..] 0)

elementAt' :: [a] -> Int -> a
elementAt' l n = fst . last $ zip l [1..n]

elementAt'' :: [a] -> Int -> a
elementAt'' l i = l !! (i-1)

elementAt''' :: [a] -> Int -> a
elementAt''' l i = head $ drop (i-1) l

elementAt'''' :: Int -> [a] -> a
elementAt'''' = (last .) . take . (+ 1)


-- (4) Find the number of elements of a list. 
myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

myLength' :: [a] -> Int
myLength' = foldr (\x -> (+) 1) 0

myLength'' :: [a] -> Int
myLength'' = fst . last . zip [1..]
-- take the n:th element of the endless integer list in zip
-- [(1,a),(2,a') ... (n,a'')] -> (n,a'') -> n


-- (5) Reverse a list.
myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

myReverse' :: [a] -> [a]
myReverse' = foldr (\x -> flip (++) [x]) []
-- (++) :: [a] -> [a] -> [a]
-- f 1 (f 2 (f 3 [])) == flip (++) [1] (flip (++) [2] (flip (++) [3] []))
-- flip (++) [3] [] == (++) [] [3] == [3]
-- flip (++) [2] [3] == (++) [2] [3]  ==[3, 2]
-- flip (++) [1] [3,2] == (++) [3,2] [1] == [3,2,1]

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []
-- (:) :: a -> [a] -> [a] | flip always append IN FRONT of the list
-- f (f (f [] 1) 2 ) 3 = flip (:) (flip (:) (flip (:) [] 1) 2) 3
-- flip (:) [] 1 == [1]
-- flip (:) [1] 2 == [2,1]
-- flip (:) [2,1] 3 == [3,2,1]


-- (6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l =  and . zipWith (==) l $ reverse l

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = and [x == rx | (x, rx) <- zip xs $ reverse xs]

isPalindrome'' :: Eq a => [a] -> Bool
isPalindrome'' [] = True
isPalindrome'' [_] = True
isPalindrome'' xs = head xs == last xs && isPalindrome'' (init $ tail xs)


-- (7) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) =  foldr (++) [] $ map flatten' xs
-- foldr concatinates the list generated by the map function (recursive)


-- (8) Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress (x:xs) = x : compress (dropWhile (== x) xs)
compress [] = []

compress' :: Eq a => [a] -> [a]
compress' xs = foldr f [last xs] xs
    where
        f a b = if a == head b then b else a:b
-- we have to start with [last xs] since we do a foldr

compress'' :: Eq a => [a] -> [a]
compress'' = map head . group
-- 'group' groups all elements equal to each other as [[a]], then
-- we take the first element from each [a].


-- (9) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists. 
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = takeWhile (== x) (x:xs) : pack (dropWhile (== x) xs)
-- gives an empty end (["aaaa","b","cc","aa","d","eeee",""])

-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

