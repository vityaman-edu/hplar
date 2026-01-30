module PropFormulaSpec (spec) where

import Formula (Formula (..))
import PropFormula (Prop (..), PropFormula)
import Test.Hspec

spec :: Spec
spec = describe "Formula Show instance" $ do
  describe "Basic constructors" $ do
    it "shows Const False as F" $ do
      show (Const False :: PropFormula String) `shouldBe` "F"

    it "shows Const True as T" $ do
      show (Const True :: PropFormula String) `shouldBe` "T"

    it "shows Atom with its value" $ do
      show (Atom (Prop "P") :: PropFormula String) `shouldBe` "P"
      show (Atom (Prop 42) :: PropFormula Int) `shouldBe` "42"

  describe "Not operator" $ do
    it "shows Not with exclamation mark" $ do
      show (Not (Atom (Prop "P"))) `shouldBe` "!P"

    it "shows nested Not correctly" $ do
      show (Not (Not (Atom (Prop "P")))) `shouldBe` "!!P"

    it "shows Not with parentheses when needed" $ do
      show (Not (Atom (Prop "P") :& Atom (Prop "Q"))) `shouldBe` "!(P & Q)"

  describe "Binary operators" $ do
    it "shows :& operator with ampersand" $ do
      show (Atom (Prop "P") :& Atom (Prop "Q")) `shouldBe` "P & Q"

    it "shows :| operator with pipe" $ do
      show (Atom (Prop "P") :| Atom (Prop "Q")) `shouldBe` "P | Q"

    it "shows :-> operator with arrow" $ do
      show (Atom (Prop "P") :-> Atom (Prop "Q")) `shouldBe` "P -> Q"

    it "shows :<-> operator with double arrow" $ do
      show (Atom (Prop "P") :<-> Atom (Prop "Q")) `shouldBe` "P <-> Q"

  describe "Operator precedence and parentheses" $ do
    it "adds parentheses for lower precedence operators" $ do
      show (Atom (Prop "P") :& (Atom (Prop "Q") :| Atom (Prop "R"))) `shouldBe` "P & (Q | R)"
      show ((Atom (Prop "P") :| Atom (Prop "Q")) :& Atom (Prop "R")) `shouldBe` "(P | Q) & R"

    it "handles complex nested formulas" $ do
      let formula = (Atom (Prop "P") :& Atom (Prop "Q")) :-> (Atom (Prop "R") :| Not (Atom (Prop "S")))
      show formula `shouldBe` "P & Q -> R | !S"

    it "handles deeply nested formulas" $ do
      let formula = Atom (Prop "P") :<-> (Atom (Prop "Q") :-> (Atom (Prop "R") :& Not (Atom (Prop "S"))))
      show formula `shouldBe` "P <-> Q -> R & !S"

    it "handles multiple negations" $ do
      let formula = Not (Not (Not (Atom (Prop "P"))))
      show formula `shouldBe` "!!!P"

    it "handles complex boolean combinations" $ do
      let formula = (Const True :& Atom (Prop "P")) :| (Const False :-> Atom (Prop "Q"))
      show formula `shouldBe` "T & P | (F -> Q)"
