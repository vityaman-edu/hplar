import qualified PropCNFSpec
import qualified PropFormulaSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  PropFormulaSpec.spec
  PropCNFSpec.spec
