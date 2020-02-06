import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Planet" $ do
    it "shall show" $ do
      showPlanet (Planet 2) (Rover ((0,0), North)) `shouldBe` "..\n^.\n"
      showPlanet (Planet 2) (Rover ((1,0), South)) `shouldBe` "..\n.v\n"
  describe "Pilot" $ do
    it "shall apply moves" $ do
      pilot "FFLB" (Rover ((2,2), North)) `shouldBe` "F, F, L, B -> Rover ((3,4),West)"
  describe "Rover" $ do
    it "shall move forward" $ do
      execute 'F' (Rover ((2,2), North))  `shouldBe` (Rover ((2,3), North))
      execute 'F' (Rover ((2,2), South))  `shouldBe` (Rover ((2,1), South))
      execute 'F' (Rover ((2,2), West))  `shouldBe` (Rover ((1,2), West))
      execute 'F' (Rover ((2,2), East))  `shouldBe` (Rover ((3,2), East))
    it "shall move backward" $ do
      execute 'B' (Rover ((2,2), North))  `shouldBe` (Rover ((2,1), North))
      execute 'B' (Rover ((2,2), South))  `shouldBe` (Rover ((2,3), South))
      execute 'B' (Rover ((2,2), West))  `shouldBe` (Rover ((3,2), West))
      execute 'B' (Rover ((2,2), East))  `shouldBe` (Rover ((1,2), East))
    it "shall turn left" $ do
      execute 'L' (Rover ((2,2), North))  `shouldBe` (Rover ((2,2), West))
      execute 'L' (Rover ((2,2), West))  `shouldBe` (Rover ((2,2), South))
      execute 'L' (Rover ((2,2), South))  `shouldBe` (Rover ((2,2), East))
      execute 'L' (Rover ((2,2), East))  `shouldBe` (Rover ((2,2), North))
    it "shall turn right" $ do
      execute 'R' (Rover ((2,2), North))  `shouldBe` (Rover ((2,2), East))
      execute 'R' (Rover ((2,2), East))  `shouldBe` (Rover ((2,2), South))
      execute 'R' (Rover ((2,2), South))  `shouldBe` (Rover ((2,2), West))
      execute 'R' (Rover ((2,2), West))  `shouldBe` (Rover ((2,2), North))
