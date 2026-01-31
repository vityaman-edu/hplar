module PropCNFSpec (spec) where

import Data.List (sort)
import Formula (Formula (..))
import PropCNF (PropCNF (..), tseitinCNF)
import PropFormula (PropFormula, p)
import Test.Hspec

spec :: Spec
spec = describe "PropCNF" $ do
  it "works on @saloed example" $ do
    let input = Not (p 1 :& (p 2 :| Not (p 3))) :: PropFormula Int
    let (PropCNF cnf) = tseitinCNF input
    show ((PropCNF . sort) cnf) `shouldBe` "(!1 | !4 | 5) & (!2 | 4) & (3 | 4) & (!4 | 2 | !3) & (!5) & (!5 | 1) & (!5 | 4)"
