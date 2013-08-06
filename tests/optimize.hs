
import Test.QuickCheck
import Graphics.Web.Processing.Core.Types
import Graphics.Web.Processing.Optimize
import Data.Text.Lazy (unpack)
import qualified Data.Text.Lazy.IO as T

instance Show ProcScript where
 show = unpack . renderScript

main :: IO ()
main = quickCheckWith (stdArgs {maxSuccess = 1000}) prop_optimizeBySubstitution_projection
