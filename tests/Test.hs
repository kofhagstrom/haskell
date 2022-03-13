import Test.Hspec
import MyLib (reverse')

main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "reverses a list" $ do
            reverse' [1..10] `shouldBe` [10,9..1]
    