
-- | Internal module defining 'Var' type and functions.
--   The /basic/ interface uses these variable functions.
--   However, they introduce a not-really-coherent
--   behavior (see 'readVar').
module Graphics.Web.Processing.Core.Var (
  -- * Variables

  -- $vars

  -- ** Functions
    Var
  , varName
  , newVar, readVar, writeVar
    ) where

import Graphics.Web.Processing.Core.Primal
import Graphics.Web.Processing.Core.Monad
import Data.Text (Text)
import Data.Monoid
import Data.String

{-$vars
The variable system presented here is actually an artifact to make explicit
which values may change over time and which don't. It also guides the user
to create variables before use them and do so in a more natural way. Somehow,
it tries to be similar to 'IORef's, while they aren't. Strictly speaking, they
do not contain any value, but it /will/ contain a value when processing.js executes
the resulting code. To make sure you are doing the right thing, variables are
typed, thus forcing you to feed the correct functions with the correct values.
However, this also leads to weird things like the one described in 'readVar'.
-}

intVarName :: Int -> Text
intVarName n = "v_" <> fromString (show n)

-- | Create a new variable with a starting value.
--   The creation of variables is restricted to the
--   'Preamble'.
newVar :: (Monad (m Preamble), ProcMonad m, ProcType a) => a -> m Preamble (Var a)
newVar x = do
  n <- liftProc newVarNumber
  let v = intVarName n
  createVarM (proc_asign v x)
  return $ varFromText v

-- | Read a variable.
--
--   Funny fact: /it does not matter when you execute this function/.
--   The result will /always/ hold the last value asigned to the variable.
--   For example, this code
--
-- > v <- newVar 10
-- > ten <- readVar v
-- > writeVar v 20
-- > point (10,ten)
--
--   will draw a point at (10,20).
readVar :: (Monad (m c), ProcMonad m, ProcType a) => Var a -> m c a
readVar = return . proc_read

-- | Write a value to a variable.
writeVar :: (ProcMonad m, ProcType a) => Var a -> a -> m c ()
writeVar v x = assignM $ proc_asign (varName v) x
