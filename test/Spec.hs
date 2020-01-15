import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Rover" $ do
    it "shall initially by at origin, facing north" $ do
      rover `shouldBe` ((0,0), North)
