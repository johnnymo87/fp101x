module ContSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Control.Monad.Cont
import Cont

main :: IO ()
main = hspec $ do
  describe "pythagoras" $ do
    it "applies the algorithm to the inputs" $ do
      pythagoras 2 3 `shouldBe` (2*2)+(3*3)

    it "has a CPS-like implementation" $ do
      pythagoras 2 3 `shouldBe` pythagoras_cps 2 3 id

    it "works with the Cont monad" $ do
      pythagoras 2 3 `shouldBe` runCont (pythagoras_cont 2 3) id

  describe "unwrap" $ do
    it "un-suspends a suspended computation" $ do
      unwrap ($ 1) `shouldBe` 1

  describe "callCC" $ do
    describe "foo" $ do
      it "returns a shown number when x ^ 2 + 3 < 20" $ do
        runCont (foo 2) id `shouldBe` "3"

      it "otherwise returns 'over twenty'" $ do
        runCont (foo 5) id `shouldBe` "over twenty"

    describe "bar" $ do
      it "returns a length 15 string when the args == 'hello'" $ do
        runCont (bar 'h' "ello") id `shouldBe` 15

      it "otherwise returns length 32 string" $ do
        runCont (bar 'h' "illo") id `shouldBe` 32

    describe "divExcpt" $ do
      it "divides if the denominator is not zero" $ do
        runCont (divExcpt 10 2 error) id `shouldBe` 5

      it "otherwise raises an error" $ do
        evaluate (runCont (divExcpt 10 0 error) id) `shouldThrow` errorCall "Denominator 0"
