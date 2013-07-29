
-- | The /basic/ interface is the closest to the original.
--   Although it contains some variations too.
--
--   For several reasons, it is recommended to use the /mid/
--   interface instead, which can be found in the
--   "Graphics.Web.Processing.Mid" module.
module Graphics.Web.Processing.Basic (
    -- * Types
    module Graphics.Web.Processing.Core.Types
    -- * Monad
  , ProcM , runProcM, execProcM
    -- * Variables
  , module Graphics.Web.Processing.Core.Var
    -- * Interface
  , module Graphics.Web.Processing.Core.Interface
  ) where

-- Internal
import Graphics.Web.Processing.Core.Types
import Graphics.Web.Processing.Core.Monad
import Graphics.Web.Processing.Core.Var
import Graphics.Web.Processing.Core.Interface


