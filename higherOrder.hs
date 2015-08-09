applyTwice :: (a->a)->a->a
applyTwice f a = f (f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c)->(b->a->c)
flip' f x y = f y x

multiThree :: (Num a)=>a->(a->(a->a))
multiThree x y z = x * y * z

map' :: (a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x: filter' f xs
    | otherwise = filter' f xs

quickSortFilter :: (Ord a)=> [a]->[a]
quickSortFilter [] = []
quickSortFilter (x:xs) =
    let smallerArray = quickSortFilter (filter (<x) xs)
        biggerArray = quickSortFilter (filter (>=x) xs)
    in smallerArray++[x]++biggerArray

largestDivisible :: (Integral a)=>a
largestDivisible = head ( filter p [1000,999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a)=>a->[a]
chain 1 = [1]
chain n
    | n `mod` 2 == 0 = n:chain(n `div` 2)
    | otherwise = n:chain(3*n + 1)

numLongChains :: Int
numLongChains = length [x| x<- map length (map chain [1..100]), x>15]
-- Lambda, shake you head with \

elem' :: (Eq a)=>a->[a]->Bool
elem' y ys = foldl (\acc x -> if x==y then True else acc) False ys
