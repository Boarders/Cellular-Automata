{-# LANGUAGE LambdaCase #-}
module Automata where

import Control.Comonad
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe(fromJust)

-- to do: clean up how converting to map works
--        add rule generator for 256 rules      

-- Based on blog post by Sigpfe here: http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

data ListZipper a = L' [a] a [a]

-- Black or White
data BW = Bl | Wh
  deriving Eq

instance Show BW where
  show Bl = "*"
  show Wh = " "

instance Show a => Show (ListZipper a) where
  show = show.toList 50

instance Functor ListZipper where
  fmap f (L' ls x rs) = L' (fmap f ls) (f x) (fmap f rs)

instance Comonad ListZipper where
  extract = listRead
  duplicate lz@(L' ls a rs) = L' (tail $ iterate leftShift lz) lz (tail $ iterate rightShift lz)

toListIndexes :: Int -> (ListZipper a) -> [(Int,a)]
toListIndexes n (L' ls x rs) = (zip [-1,-2..] (take n' ls) ) ++ [(0,x)] ++ (zip [1..] (take n' rs))
  where n' = n `div` 2

toMap :: Int -> (ListZipper a) -> M.Map Int a
toMap n = M.fromList.(toListIndexes n)


toList :: Int -> (ListZipper a) -> [a]
toList n (L' ls x rs) = (reverse $ take n' ls) ++ [x] ++ (take n' rs)
  where n' = n `div` 2

toList' :: (ListZipper a) -> [a]
toList' (L' ls x rs) = (reverse ls) ++ [x] ++ rs

segmentToList :: (ListZipper a) -> Int -> [a]
segmentToList (L' ls x rs) n = (take n (reverse ls)) ++ [x] ++ (take n rs)
  

leftShift :: ListZipper a -> ListZipper a
leftShift (L' (l:ls) x rs) = L' ls l (x:rs)

rightShift :: ListZipper a -> ListZipper a
rightShift (L' (ls) x (r:rs)) = L' (x:ls) r rs

listRead :: ListZipper a -> a
listRead (L' _ x _) = x

listReassign :: a -> ListZipper a -> ListZipper a
listReassign x (L' ls _ rs) = L' ls x rs

update :: (ListZipper a -> a) -> ListZipper a -> ListZipper a
update = extend

rule30 :: (ListZipper BW) -> BW
rule30 (L' ls@(l:ls') x rs@(r:rs'))
  | (l == Bl && x == Bl && r == Bl) = Wh
  | (l == Bl && x == Bl && r == Wh) = Wh
  | (l == Bl && x == Wh && r == Bl) = Wh
  | (l == Bl && x == Wh && r == Wh) = Bl
  | (l == Wh && x == Bl && r == Bl) = Bl
  | (l == Wh && x == Bl && r == Wh) = Bl
  | (l == Wh && x == Wh && r == Bl) = Bl
  | (l == Wh && x == Wh && r == Wh) = Wh

runRule30 = update rule30

startState :: ListZipper BW
startState = L' (repeat Wh) Bl (repeat Wh)

startStateMap = toMap width startState
startStateMap' = M.insert (-25) Bl startStateMap

test = L' (Bl:repeat Wh) Bl (Bl:repeat Wh)

stateFromMap :: (M.Map Int BW) -> ListZipper BW
stateFromMap mp = L' ((reverse ls) ++ (repeat Wh) ) centre (rs ++ (repeat Wh))
  where ls = (map snd).(M.toList) $ (M.filterWithKey (\k a -> (k< 0)) mp)
        rs = (map snd).(M.toList) $ (M.filterWithKey (\k a -> (k> 0)) mp)
        centre = fromJust $ M.lookup 0 mp

width = 100

iterateUpdate :: Int -> M.Map Int BW -> M.Map Int BW
iterateUpdate 0 state = state
iterateUpdate n state = iterateUpdate (n-1) newStateMap
  where newStateMap = toMap width newStateZipper
        newStateZipper = runRule30.stateFromMap $ state


recurs :: Integer -> Integer
recurs n
  | n == 1 = 2
  | n <= 100 = 2*recurs (n-1)
  | otherwise = recurs 100

        
  


