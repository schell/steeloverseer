module Sos.TemplateSpec where

import Sos.Template

import Test.Hspec

spec :: Spec
spec = do
  describe "parseTemplate" $ do
    it "fails on the empty string" $
      parseTemplate "" `shouldSatisfy` isLeft

    it "parses templates" $ do
      parseTemplate "hi"      `shouldBe` Right [Right "hi"]
      parseTemplate "foo bar" `shouldBe` Right [Right "foo bar"]
      parseTemplate "\\25"    `shouldBe` Right [Left 25]
      parseTemplate "gcc \\0" `shouldBe` Right [Right "gcc ", Left 0]

  describe "instantiateTemplate" $ do
    it "ignores capture groups in templates with no captures" $ do
      instantiateTemplate []            [Right "z"] `shouldBe` Right "z"
      instantiateTemplate ["a"]         [Right "z"] `shouldBe` Right "z"
      instantiateTemplate ["a","b","c"] [Right "z"] `shouldBe` Right "z"

    it "substitutes capture groups" $ do
      instantiateTemplate ["a","b","c"] [Left 2, Left 1, Left 0] `shouldBe` Right "cba"

    it "errors when there are not enough capture groups" $ do
      instantiateTemplate []    [Left 0] `shouldSatisfy` isLeft
      instantiateTemplate ["a"] [Left 1] `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
