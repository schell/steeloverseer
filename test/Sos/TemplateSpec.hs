module Sos.TemplateSpec where

import Sos.Exception
import Sos.Template

import Test.Hspec

spec :: Spec
spec = do
  describe "parseTemplate" $ do
    it "fails on the empty string" $
      parseTemplate "" `shouldThrow` anySosException

    it "parses templates" $ do
      parseTemplate "hi"      `shouldReturn` [Right "hi"]
      parseTemplate "foo bar" `shouldReturn` [Right "foo bar"]
      parseTemplate "\\25"    `shouldReturn` [Left 25]
      parseTemplate "gcc \\0" `shouldReturn` [Right "gcc ", Left 0]

  describe "instantiateTemplate" $ do
    it "ignores capture groups in templates with no captures" $ do
      instantiateTemplate []            [Right "z"] `shouldReturn` "z"
      instantiateTemplate ["a"]         [Right "z"] `shouldReturn` "z"
      instantiateTemplate ["a","b","c"] [Right "z"] `shouldReturn` "z"

    it "substitutes capture groups" $ do
      instantiateTemplate ["a","b","c"] [Left 2, Left 1, Left 0]
        `shouldReturn` "cba"

    it "errors when there are not enough capture groups" $ do
      instantiateTemplate []    [Left 0] `shouldThrow` anySosException
      instantiateTemplate ["a"] [Left 1] `shouldThrow` anySosException

anySosException :: Selector SosException
anySosException = const True
