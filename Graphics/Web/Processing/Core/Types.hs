
-- | Collection of types.
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
  -- * Processing types
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
  -- ** Char
  , Proc_Char
  , fromChar
  -- ** Text
  , Proc_Text
  , fromStText
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

-- | Conditional value.
if_ :: ProcType a => Proc_Bool -> a -> a -> a
if_ = proc_cond
