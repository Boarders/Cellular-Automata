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
  extract = listZipRead
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
  

leftShift :: ListZipper a -> ListZipper a
leftShift (L' (l:ls) x rs) = L' ls l (x:rs)

rightShift :: ListZipper a -> ListZipper a
rightShift (L' (ls) x (r:rs)) = L' (x:ls) r rs

listZipRead :: ListZipper a -> a
listZipRead (L' _ x _) = x

listReassign :: a -> ListZipper a -> ListZipper a
listReassign x (L' ls _ rs) = L' ls x rs

update :: (ListZipper a -> a) -> ListZipper a -> ListZipper a
update = extend

convertTo256Binary' :: Int -> [Int]
convertTo256Binary' 0 = [0]
convertTo256Binary' 1 = [1]
convertTo256Binary' n = (n' `mod` 2) : (convertTo256Binary (n `div` 2))
  where n' = n `mod` 256

convertTo256Binary = reverse.convertTo256Binary'

code :: Int -> BW
code 0 = Wh
code 1 = Bl
code n = error "Cannot convert non-binary input to black or white." 

wolframCode :: Int -> [BW]
wolframCode n = map code $  replicate l' 0 ++ ns
  where ns = convertTo256Binary n
        l = length ns
        l'= 8 - l 
        
wolframRule :: Int -> (ListZipper BW) -> BW
wolframRule n (L' ls@(l:ls') x rs@(r:rs'))
  | (l == Bl && x == Bl && r == Bl) = wolfm !! 0
  | (l == Bl && x == Bl && r == Wh) = wolfm !! 1
  | (l == Bl && x == Wh && r == Bl) = wolfm !! 2
  | (l == Bl && x == Wh && r == Wh) = wolfm !! 3
  | (l == Wh && x == Bl && r == Bl) = wolfm !! 4
  | (l == Wh && x == Bl && r == Wh) = wolfm !! 5
  | (l == Wh && x == Wh && r == Bl) = wolfm !! 6
  | (l == Wh && x == Wh && r == Wh) = wolfm !! 7
    where wolfm = wolframCode n


rule30 = wolframRule 30
runRule30 = update rule30
runRule = update.wolframRule
ruleNumber = 30

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

iterateUpdate :: Int -> Int -> M.Map Int BW -> M.Map Int BW
iterateUpdate k 0 state = state
iterateUpdate k n state = iterateUpdate k (n-1) newStateMap
  where newStateMap = toMap width newStateZipper
        newStateZipper = (runRule k).stateFromMap $ state






        
  


