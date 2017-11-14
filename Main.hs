module Main where

import Graphics.Gloss
import Control.Applicative
import Automata
import qualified Data.Map.Strict as M

window :: Display
window = InWindow "Nice Window" (windowWidth,windowHeight) (offset,offset)

windowWidth = 510
windowHeight = 510
offset = 200

background :: Color
background = white

gray = greyN 0.7

square = rectangleSolid 11 11

vertRect :: [Picture]
vertRect = [color gray (rectangleSolid 1 510)]

horzRect :: [Picture]
horzRect = [color gray (rectangleSolid 510 1)]

horzTranslates = [Translate 0 n | n<- [-255,-245..255]]
vertTranslates = [Translate n 0 | n<- [-255,-245..255]]

grid = (horzTranslates <*> horzRect) ++ (vertTranslates <*> vertRect)

render :: M.Map Int BW -> [Picture]
render st = [Translate (fromIntegral $ 10*k) 250 | (k,_)<-alivesList] <*> (pure square)
  where truncateState = M.filterWithKey (\k _ -> (abs k) < 26) st
        alives = M.filter (== Bl) truncateState
        alivesList = M.toList alives


main :: IO ()
main = animation


animation = animate window background frame
  where
  frame :: Float -> Picture
  frame sec
    | sec <  0 = Pictures []
    | sec <= upper  = Pictures $ grid ++ [frame (sec-(1/5))] ++ ((Translate 0 (-10*ticker')) <$> (render $ iterateUpdate ticker startStateMap))
    | otherwise = final
      where speed = 5
            upper = (51/speed)
            ticker = floor $ sec*speed
            ticker' = fromIntegral ticker
            final = frame upper
    