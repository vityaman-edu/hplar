module PropFormulaSpec (spec) where

import Formula (Formula (..))
import PropFormula (PropFormula, p, tautology, valuation, (|=>))
import Test.Hspec

spec :: Spec
spec = describe "PropFormula" $ do
  specShow
  specValuation
  specTautology
  specSubstitution

specShow :: SpecWith ()
specShow = describe "Formula Show instance" $ do
  describe "Basic constructors" $ do
    it "shows Atom with its value" $ do
      show (p "P" :: PropFormula String) `shouldBe` "P"
      show (p 42 :: PropFormula Int) `shouldBe` "42"

  describe "Not operator" $ do
    it "shows Not with exclamation mark" $
      show (Not (p "P")) `shouldBe` "!P"

    it "shows nested Not correctly" $
      show (Not (Not (p "P"))) `shouldBe` "!!P"

    it "shows Not with parentheses when needed" $
      show (Not (p "P" :& p "Q")) `shouldBe` "!(P & Q)"

  describe "Binary operators" $ do
    it "shows :& operator with ampersand" $
      show (p "P" :& p "Q") `shouldBe` "P & Q"

    it "shows :| operator with pipe" $
      show (p "P" :| p "Q") `shouldBe` "P | Q"

    it "shows :-> operator with arrow" $
      show (p "P" :-> p "Q") `shouldBe` "P -> Q"

  describe "Operator precedence and parentheses" $ do
    it "adds parentheses for lower precedence operators" $ do
      show (p "P" :& (p "Q" :| p "R")) `shouldBe` "P & (Q | R)"
      show ((p "P" :| p "Q") :& p "R") `shouldBe` "(P | Q) & R"

    it "handles complex nested formulas" $ do
      let formula = (p "P" :& p "Q") :-> (p "R" :| Not (p "S"))
      show formula `shouldBe` "P & Q -> R | !S"

    it "handles multiple negations" $ do
      let formula = Not (Not (Not (p "P")))
      show formula `shouldBe` "!!!P"

specValuation :: SpecWith ()
specValuation =
  describe "valuation" $ do
    it "evaluates Atom using the interpretation function" $ do
      valuation (== "P") (p "P") `shouldBe` True
      valuation (== "Q") (p "P") `shouldBe` False

    it "evaluates :& operator correctly" $ do
      let formula = p "P" :& p "Q"
      valuation (== "P") formula `shouldBe` False
      valuation (== "Q") formula `shouldBe` False
      valuation (\v -> v == "P" || v == "Q") formula `shouldBe` True

    it "evaluates :| operator correctly" $ do
      let formula = p "P" :| p "Q"
      valuation (== "P") formula `shouldBe` True
      valuation (== "Q") formula `shouldBe` True
      valuation (== "R") formula `shouldBe` False

    it "evaluates :-> operator correctly" $ do
      let formula = p "P" :-> p "Q"
      valuation (== "P") formula `shouldBe` False
      valuation (== "Q") formula `shouldBe` True
      valuation (== "R") formula `shouldBe` True

specTautology :: SpecWith ()
specTautology =
  describe "tautology function" $ do
    it "identifies P | !P as a tautology" $ do
      let formula = p "P" :| Not (p "P")
      tautology formula `shouldBe` True

    it "identifies P & !P as not a tautology" $ do
      let formula = p "P" :& Not (p "P")
      tautology formula `shouldBe` False

    it "identifies P -> P as a tautology" $ do
      let formula = p "P" :-> p "P"
      tautology formula `shouldBe` True

    it "identifies complex tautologies correctly" $ do
      let formula = (p "P" :-> p "Q") :| (p "Q" :-> p "P")
      tautology formula `shouldBe` True

    it "identifies non-tautologies correctly" $ do
      let formula = p "P" :& p "Q"
      tautology formula `shouldBe` False

specSubstitution :: SpecWith ()
specSubstitution =
  describe "substitution operator |=>" $ do
    it "substitutes atoms in formulas correctly" $ do
      let substitution = "p" |=> p "q"
      let formula = p "p" :& p "q" :& p "p" :& p "q"
      let result = substitution formula
      show result `shouldBe` "q & (q & (q & q))"

    it "substitutes atoms to formulas correctly" $ do
      let substitution = "p" |=> (p "p" :| p "q")
      let formula = p "p" :& p "q" :& p "p" :& p "q"
      let result = substitution formula
      show result `shouldBe` "(p | q) & (q & ((p | q) & q))"
