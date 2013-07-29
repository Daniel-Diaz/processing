
-- | In addition to the "Graphics.Web.Processing.Basic" module, you may also
--   import this one to take advantage of some templates that will ease
--   your processing code production.
module Graphics.Web.Processing.Basic.Baked (
  -- * Basic interface
    module Graphics.Web.Processing.Basic
  -- * Baked scripts
  , animationScript
  ) where

import Graphics.Web.Processing.Basic

-- | Function to create a time-dependent execution.
--   Useful for animations.
animationScript ::
      Proc_Float                  -- ^ Speed.
                                  --   How much time increases each frame.
  ->  ProcCode Setup              -- ^ Setup code.
  -> (Proc_Float -> ProcM Draw a) -- ^ Drawing code, in respect to time.
  ->  ProcScript
animationScript q cSetup f =
  let (vt,cPreamble) = runProcM $ newVar 0
  in  emptyScript {
          proc_setup = cSetup
        , proc_preamble = cPreamble
        , proc_draw = Just $ execProcM $ readVar vt >>=
                          \t -> f t >> writeVar vt (t + q)
          }
