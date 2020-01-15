import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Rover" $ do
    it "shall initially by at origin, facing north" $ do
      initialRover `shouldBe` ((0,0), North)
    it "shall move forward" $ do
      execute 'F' ((2,2), North)  `shouldBe` ((2,3), North)
      execute 'F' ((2,2), South)  `shouldBe` ((2,1), South)
      execute 'F' ((2,2), West)  `shouldBe` ((1,2), West)
      execute 'F' ((2,2), East)  `shouldBe` ((3,2), East)
    it "shall move backward" $ do
      execute 'B' ((2,2), North)  `shouldBe` ((2,1), North)
      execute 'B' ((2,2), South)  `shouldBe` ((2,3), South)
      execute 'B' ((2,2), West)  `shouldBe` ((3,2), West)
      execute 'B' ((2,2), East)  `shouldBe` ((1,2), East)
    it "shall turn left" $ do
      execute 'L' ((2,2), North)  `shouldBe` ((2,2), East)
      execute 'L' ((2,2), East)  `shouldBe` ((2,2), South)
      execute 'L' ((2,2), South)  `shouldBe` ((2,2), West)
      execute 'L' ((2,2), West)  `shouldBe` ((2,2), North)
    it "shall turn right" $ do
      execute 'R' ((2,2), North)  `shouldBe` ((2,2), West)
      execute 'R' ((2,2), West)  `shouldBe` ((2,2), South)
      execute 'R' ((2,2), South)  `shouldBe` ((2,2), East)
      execute 'R' ((2,2), East)  `shouldBe` ((2,2), North)
