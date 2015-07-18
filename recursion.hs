import Data.List
import Data.Maybe
quicksort :: (Ord a)=>[a]->[a]
quicksort [] = []
quicksort (x:xs)=
  let leftList = quicksort [a| a<-xs, a <= x];
      rightList = quicksort [a|a<-xs, a>x]
  in leftList++[x]++rightList

merge :: (Ord a)=> [a]->[a]->[a]
merge l [] = l
merge [] l = l
merge allx@(x:xs) ally@(y:ys) =
  if x<=y then x : merge xs ally
  else y : merge allx ys

splitHalf::[a]->([a],[a])
splitHalf xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2

mergeSort :: (Ord a)=>[a]->[a]
mergeSort []  = []
mergeSort x
  | length x > 1 = merge (mergeSort ls) (mergeSort rs)
  |otherwise = x
  where (ls, rs) = splitHalf x
