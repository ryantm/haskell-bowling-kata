module Bowling where

import Test.QuickCheck
import Test.Hspec

strike = "X"
spare = "/"

frame :: Gen [String]
frame = do
  first <- choose (0, 10)
  if first == 10 then
    return [strike]
  else do
    second <- choose (0, 10 - first) :: Gen Int
    if second + first == 10 then
      return [show first, spare]
    else
      return [show first, show second]

lastFrame :: Gen [String]
lastFrame = do
  threeDeliveries <- fmap (take 3) (fmap concat (infiniteListOf frame))
  if (threeDeliveries !! 0 == strike || threeDeliveries !! 1 == spare) then
     return threeDeliveries
  else
     return (take 2 threeDeliveries)

game :: Gen String
game = do
   firstNine <- vectorOf 9 frame
   last <- lastFrame
   return (concat (concat (firstNine ++ [last])))

newtype BowlingGame = BowlingGame { deliveries :: String} deriving Show

instance Arbitrary BowlingGame where
  arbitrary = fmap BowlingGame game

--deepCheck = quickCheckWith stdArgs { maxSuccess = 10000 }

score :: String -> Int
score =  doScore . convertToRolls

convertToRolls :: String -> [Int]
convertToRolls [] = []
convertToRolls ('X':xs) = 10 : convertToRolls xs
convertToRolls (x:'/':xs) = read [x] : (10 - (read [x])) : convertToRolls xs
convertToRolls (x:xs) = (read [x]) : convertToRolls xs

doScore :: [Int] -> Int
doScore [] = 0
doScore (x:[]) = x
doScore (x:y:[]) = x + y
doScore (x:y:z:[]) = x + y + z
doScore (x:y:z:xs) | x == 10  = x + y + z  + doScore (y:z:xs)
                   | x + y == 10  = x + y + z  + doScore (z:xs)
                   | otherwise = x + y  + doScore (z:xs)

tests :: IO ()
tests = do
  quickCheck (\ (BowlingGame g) -> length g >= 13 && length g <= 21)
  quickCheck (\ (BowlingGame g) -> length g >= 13 && length g <= 21)
  hspec (do
    describe "score" (do
     it "returns zero for a zero game" (do
       score (replicate 20 '0') `shouldBe` 0)
     it "returns 20 for a game of all one pins" (do
       score (replicate 20 '1') `shouldBe` 20)
     it "can handle a spare" (do
       score ("5/" ++ replicate 18 '0') `shouldBe` 10)
     it "can handle a spare" (do
       score ("5/4" ++ replicate 17 '0') `shouldBe` 18)
     it "can handle a strike" (do
       score ("X43" ++ replicate 16 '0') `shouldBe` 24)
     it "can handle a strike" (do
       score (replicate 12 'X') `shouldBe` 300)
     ))
  quickCheck (\ (BowlingGame g) -> score g >= 0 && score g <= 300)
