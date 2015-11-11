{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Yesod.Csp

main :: IO ()
main = hspec $
  describe "Basics" $
    it "works" $ do
      let header = getCspPolicy [ScriptSrc [Self], StyleSrc [Https, Self]]
      header `shouldBe` "foo"
