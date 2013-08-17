
-- | Collection of types (@Proc_*@ types and others), and
--   some functions on these types as well.
module Graphics.Web.Processing.Core.Types (
  -- * Processing Script
    ProcScript (..)
  , emptyScript
  -- ** Script rendering
  , renderScript
  , renderFile
  -- ** Processing Code
  , ProcCode
  -- * Contexts
  , Preamble (..)
  , Setup (..)
  , Draw (..)
  , MouseClicked (..)
  , MouseReleased (..)
  , KeyPressed (..)
  -- * @Proc_*@ types
  , ProcType
  -- ** Bool
  , Proc_Bool, true, false
  , fromBool
  , pnot, (#||), (#&&)
  -- ** Int
  , Proc_Int
  , fromInt
  , intToFloat
  -- ** Float
  , Proc_Float
  , fromFloat
  , pfloor
  , pround
  -- ** Char
  , Proc_Char
  , fromChar
  -- ** Text
  , Proc_Text
  , fromStText
  , (+.+)
  , Proc_Show (..)
  -- ** Image
  , Proc_Image
  -- * Processing classes
  , Proc_Eq (..)
  , Proc_Ord (..)
  -- * Conditional values
  , if_
  ) where

import Graphics.Web.Processing.Core.Primal
import Text.PrettyPrint.Mainland
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T

charsPerLine :: Int
charsPerLine = 80

-- | Render a script as a lazy 'Text'.
renderScript :: ProcScript -> Text
renderScript = prettyLazyText charsPerLine . ppr

-- | Render a script using 'renderScript' and
--   write it directly in a file.
renderFile :: FilePath -> ProcScript -> IO ()
renderFile fp = T.writeFile fp . renderScript

-- | Conditional value. For example:
--
-- > if_ (x #> 3) "X is greater than 3."
-- >              "X is less than or equal to 3."
--
if_ :: ProcType a => Proc_Bool -> a -> a -> a
if_ = proc_cond
