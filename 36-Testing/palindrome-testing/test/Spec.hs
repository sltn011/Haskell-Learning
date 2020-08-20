import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Data.Text as T

prop_reverseInvariant text =
    isPalindrome text == isPalindrome (T.reverse text)

main :: IO ()
main = do
    quickCheckWith stdArgs {maxSuccess = 10000} prop_reverseInvariant