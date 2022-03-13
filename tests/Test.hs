import Test.Hspec
import MyLib (reverse',replicate')

main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "reverses a list" $ do
            reverse' [1..10] `shouldBe` [10,9..1]
            reverse' "hej" `shouldBe` "jeh"
            reverse' [1] `shouldBe` [1]
        it "replicates something" $ do
            replicate' 2 1 `shouldBe` [1,1]
            replicate' 1 1 `shouldBe` [1]