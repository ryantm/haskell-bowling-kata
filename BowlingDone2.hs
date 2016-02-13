module Bowling where

import Test.QuickCheck
import Test.Hspec

strike = "X"
spare = "/"

data Delivery = Strike | Spare | Down Int deriving (Eq)

instance Show Delivery where
  show Strike = "X"
  show Spare = "/"
  show (Down i) = show i

delivery :: Int -> Gen Delivery
delivery upTo = do
  d <- choose (0, upTo)
  if d == 10 then
    return Strike
  else if d == upTo then
    return Spare
  else
    return (Down d)

frame :: Gen [Delivery]
frame =
  let pins = 10 in do
   first <- delivery pins
   if first == Strike then
      return [Strike]
   else do
     let (Down f) = first
     second <- delivery (pins - f)
     return [first, second]

lastFrame :: Gen [Delivery]
lastFrame = do
  ds <- fmap concat (infiniteListOf frame)
  if ds !! 0 == Strike || ds !! 1 == Spare then
    return (take 3 ds)
  else
    return (take 2 ds)

game :: Gen [Delivery]
game = do
  fs <- vectorOf 9 frame
  lf <- lastFrame
  return (concat (fs ++ [lf]))

newtype BowlingGame = BowlingGame { deliveries :: [Delivery]} deriving Show

instance Arbitrary BowlingGame where
  arbitrary = fmap BowlingGame game

tests :: IO()
tests = do
  quickCheck (\ (BowlingGame g) -> length g >= 13 && length g <= 21)
  hspec $ do
    describe "score" $ do
      it "should return 0 for the zero game" $ do
        score (replicate 20 (Down 0)) `shouldBe` 0
      it "should return 20 for the 1 game" $ do
        score (replicate 20 (Down 1)) `shouldBe` 20
      it "should return handle a spare in last frame" $ do
        score (replicate 18 (Down 0) ++ [Down 5, Spare, Down 5]) `shouldBe` 15
      it "should return handle a strike in last frame" $ do
        score (replicate 18 (Down 0) ++ [Strike, Strike, Strike]) `shouldBe` 30
      it "should return handle a strike in last frame" $ do
        score (replicate 18 (Down 0) ++ [Strike, Down 4, Spare]) `shouldBe` 20
      it "should return handle a strike in last frame" $ do
        score (replicate 18 (Down 0) ++ [Strike, Down 4, Down 2]) `shouldBe` 16

numScore :: [Delivery] -> [Int]
numScore [] = []
numScore (Down d1 : Spare : xs) = d1 : 10 - d1 : numScore xs
numScore (Strike : xs) = 10 : numScore xs
numScore (Down d : xs) = d : numScore xs

score :: [Delivery] -> Int
score = doScore . numScore


doScore [] = 0
doScore (x : y : z : []) = x + y + z
doScore (x : ds) = x + doScore ds

--deepCheck = quickCheckWith stdArgs { maxSuccess = 10000 }
