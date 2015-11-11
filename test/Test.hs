{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Yesod.Csp

main :: IO ()
main = hspec $
  describe "Basics" $ do
    it "works" $ do
      let header = getCspPolicy [ScriptSrc [Self], StyleSrc [Https, Self]]
      header `shouldBe` "script-src 'self'; style-src https: 'self'"
    it "excludes blank source lists" $ do
      let header = getCspPolicy [ScriptSrc [], StyleSrc [Https, Self]]
      header `shouldBe` "style-src https: 'self'"
