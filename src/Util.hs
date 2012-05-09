-----------------------------------------------------------------------------
--
-- Module      :  Util
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Util where

splitEvery:: Int -> [a] -> [[a]]
splitEvery n list =
    tail $ splitter list [[]]
    where
        splitter [] ret = ret
        splitter list ret =
            let
                (left, right) = splitAt n list
            in
                splitter right (ret ++ [left])

replaceItem:: Int -> [a] -> a -> [a]
replaceItem n list item =
  let
    (left, _, right) = splitAt3 n list
  in
    left ++ [item] ++ right

removeItem:: Int -> [a] -> [a]
removeItem n list =
  let
    (left, _, right) = splitAt3 n list
  in
    left ++ right

splitAt3:: Int -> [a] -> ([a], a, [a])
splitAt3 n list =
    let
        (left, (x:xs)) = splitAt n list
    in
        (left, x, xs)

