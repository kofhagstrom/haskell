import LondonToHeathrow (RoadSystem, optimalPath, Section)
import MyLib (replicate'', reverse'')
import Test.Hspec (describe, hspec, it, shouldBe)

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "reverses a list" $ do
      reverse'' [1 .. 10] `shouldBe` [10, 9 .. 1]
      reverse'' "hej" `shouldBe` "jeh"
      reverse'' [1] `shouldBe` [1]
    it "replicates something" $ do
      replicate'' 2 1 `shouldBe` [1, 1]
      replicate'' 1 1 `shouldBe` [1]
    it "the optimal path from heathrow to london is" $ do
      optimalPath heathrowToLondon `shouldBe` [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8), (C, 0)]