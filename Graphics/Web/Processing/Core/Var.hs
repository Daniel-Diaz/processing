
-- | Module exporting 'Var' and 'ArrayVar' type and functions.
module Graphics.Web.Processing.Core.Var (
  -- * Variables

  -- $vars

    Var
  , ArrayVar
  , arraySize

  -- ** Functions
  , varName, arrayVarName
  , newVar, readVar, writeVar
  , newArrayVar, readArrayVar, writeArrayVar
    ) where

import Graphics.Web.Processing.Core.Primal
import Graphics.Web.Processing.Core.Monad

{-$vars
The variable system presented here is actually an artifact to make explicit
which values may change over time and which don't. It also guides the user
to create variables before use them and do so in a more natural way. Somehow,
it tries to be similar to 'IORef's, while they aren't. Strictly speaking, they
do not contain any value, but it /will/ contain a value when processing.js executes
the resulting code. To make sure you are doing the right thing, variables are
typed, thus forcing you to feed the correct functions with the correct values.
-}


