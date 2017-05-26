module Permutation
( rotate
, permute
) where

rotate :: [a] -> [[a]]
rotate xs = scanl (\(y:ys) _ -> ys ++ [y]) xs [2..length xs]

permute :: [a] -> [[a]]
permute [] = []
permute [x] = [[x]]
permute xs = concat [map (y:) $ permute ys | (y:ys) <- rotate xs]