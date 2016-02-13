module Bowling where

import Test.QuickCheck
import Test.Hspec

data PinSet = Strike | Spare Int | NoMarks Int Int deriving (Eq)

instance Show PinSet where
  show Strike = "X"
  show (Spare first)= (show first) ++ "/"
  show (NoMarks fd sd) = show fd ++ show sd

throw :: Int -> Gen Int
throw pinsLeft = choose (0, pinsLeft)

pinSet :: Gen PinSet
pinSet = do
  firstDown <- throw 10
  if firstDown == 10 then
    return Strike
  else do
    secondDown <- throw (10 - firstDown)
    if firstDown + secondDown == 10 then
      return (Spare firstDown)
    else
      return (NoMarks firstDown secondDown)

normalFrame = pinSet

firstBallOfPinSet :: PinSet -> PinSet
firstBallOfPinSet Strike = Strike
firstBallOfPinSet (Spare n) = NoMarks n 0
firstBallOfPinSet (NoMarks n1 n2) = NoMarks n1 0

isSpare :: PinSet -> Bool
isSpare (Spare _) = True
isSpare _ = False

lastFrame :: Gen [PinSet]
lastFrame = do
  (f:s:t:[]) <- vectorOf 3 pinSet
  if f == Strike && s == Strike then
    return [f, s, firstBallOfPinSet t]
  else if f == Strike then
         return [f, s]
       else if isSpare f then
              return [f, firstBallOfPinSet s]
            else
              return [f]

game :: Gen [PinSet]
game = do
  first <- vectorOf 9 normalFrame
  last <- lastFrame
  return (first ++ last)

newtype BowlingGame = BowlingGame { g :: [PinSet] } deriving Show

instance Arbitrary BowlingGame where
  arbitrary = fmap BowlingGame game


firstName :: Gen String
firstName = elements ["Bob","Sam"]
