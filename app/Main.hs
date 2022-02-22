module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Complex
import Data.ByteString (pack)

width = 1200 :: Integer
height = 800 :: Integer

type Pixel = Int
type PixelCoords = (Pixel, Pixel)
type RealCoords = (Double, Double)

type Range a = (a, a)

data State = State {
  xMin :: Float,
  xMax :: Float,
  yMin :: Float,
  yMax :: Float
} deriving (Show)

-- [0, 1024] —> [-1, 1]
mapRange (a, b) (c, d) t = c + ((d - c) / (b - a)) * (t - a)

mapPixelToComplex (xMin, xMax) (yMax, yMin) (n, m) = mapX n :+ mapY m
  where mapX = mapRange (0, fromIntegral width) (xMin, xMax)
        mapY = mapRange (0, fromIntegral height) (yMin, yMax)

window = InWindow "Mandelbrot" (fromIntegral width, fromIntegral height) (0, 0)

backgroundColor = white

fps = 1

initialState = State {
  xMin = -2.25,
  xMax = 1.125,
  yMin = -1.125,
  yMax = 1.125
}

computeNumberOfSteps :: (RealFloat a, Num a, Ord a) => Complex a -> Integer
computeNumberOfSteps z = toInteger $ length $ take 50 iterationsInsideRadius2
  where iterationsInsideRadius2 = takeWhile ((<2) . magnitude) (iterate ((+z) . (**2)) 0)

quadrants (a,b)
  | a == 0 = 1
  | otherwise = 0

-- intsToColors :: [Int] -> [Int]
intsToColors = foldr (\n acc -> (parseColor n) ++ acc) []
  where shade n = ((fromIntegral n) `mod` 10) * 25
        parseColor n = [shade n, shade n, shade n, fromIntegral 255]
    
steps State { xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax } = map (computeNumberOfSteps . toComplex) pixels
    where pixels = [(x, y) | y <- [0..(fromIntegral $ height - 1)], x <- [0..(fromIntegral $ width - 1)] ]
          toComplex = mapPixelToComplex (xMin, xMax) (yMax, yMin)

byteString state = pack . intsToColors $ steps state

draw :: State -> Picture
draw state = bitmapOfByteString (fromIntegral width) (fromIntegral height) bitmapFormat (byteString state) True          
  where bitmapFormat = BitmapFormat TopToBottom PxRGBA

-- handleInputs = undefined

main :: IO ()
main = display window white $ draw initialState
-- main = play window backgroundColor fps initialState draw (const id) (const id)

