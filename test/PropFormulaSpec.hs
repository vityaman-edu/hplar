{-# LANGUAGE InstanceSigs #-}

module PropFormulaSpec (spec) where

import Formula (Formula (..))
import PropFormula (PropFormula)
import Test.Hspec

newtype P = P String

instance Show P where
  showsPrec :: Int -> P -> ShowS
  showsPrec _ (P s) = showString s

spec :: Spec
spec = describe "Formula Show instance" $ do
  describe "Basic constructors" $ do
    it "shows Const False as F" $ do
      show (Const False :: PropFormula P) `shouldBe` "F"

    it "shows Const True as T" $ do
      show (Const True :: PropFormula P) `shouldBe` "T"

    it "shows Atom with its value" $ do
      show (Atom (P "P") :: PropFormula P) `shouldBe` "P"
      show (Atom 42 :: PropFormula Int) `shouldBe` "42"

  describe "Not operator" $ do
    it "shows Not with exclamation mark" $ do
      show (Not (Atom (P "P"))) `shouldBe` "!P"

    it "shows nested Not correctly" $ do
      show (Not (Not (Atom (P "P")))) `shouldBe` "!!P"

    it "shows Not with parentheses when needed" $ do
      show (Not (Atom (P "P") :& Atom (P "Q"))) `shouldBe` "!(P & Q)"

  describe "Binary operators" $ do
    it "shows :& operator with ampersand" $ do
      show (Atom (P "P") :& Atom (P "Q")) `shouldBe` "P & Q"

    it "shows :| operator with pipe" $ do
      show (Atom (P "P") :| Atom (P "Q")) `shouldBe` "P | Q"

    it "shows :-> operator with arrow" $ do
      show (Atom (P "P") :-> Atom (P "Q")) `shouldBe` "P -> Q"

    it "shows :<-> operator with double arrow" $ do
      show (Atom (P "P") :<-> Atom (P "Q")) `shouldBe` "P <-> Q"

  describe "Operator precedence and parentheses" $ do
    it "adds parentheses for lower precedence operators" $ do
      show (Atom (P "P") :& (Atom (P "Q") :| Atom (P "R"))) `shouldBe` "P & (Q | R)"
      show ((Atom (P "P") :| Atom (P "Q")) :& Atom (P "R")) `shouldBe` "(P | Q) & R"

    it "handles complex nested formulas" $ do
      let formula = (Atom (P "P") :& Atom (P "Q")) :-> (Atom (P "R") :| Not (Atom (P "S")))
      show formula `shouldBe` "P & Q -> R | !S"

    it "handles deeply nested formulas" $ do
      let formula = Atom (P "P") :<-> (Atom (P "Q") :-> (Atom (P "R") :& Not (Atom (P "S"))))
      show formula `shouldBe` "P <-> Q -> R & !S"

    it "handles multiple negations" $ do
      let formula = Not (Not (Not (Atom (P "P"))))
      show formula `shouldBe` "!!!P"

    it "handles complex boolean combinations" $ do
      let formula = (Const True :& Atom (P "P")) :| (Const False :-> Atom (P "Q"))
      show formula `shouldBe` "T & P | (F -> Q)"
