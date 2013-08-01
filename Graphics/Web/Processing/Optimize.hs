
-- | This module is still in progress.
module Graphics.Web.Processing.Optimize (
   ) where

import Graphics.Web.Processing.Core.Primal
import Data.HashMap.Lazy
import Control.Monad (when)
import Control.Monad.Trans.State

-- | Number of operations needed to calculate the
--   value of a given 'Proc_Float' value.
numOps :: Proc_Float -> Int
-- Really boring function!
numOps (Proc_Float _) = 0
numOps (Float_Sum x y) = 1 + numOps x + numOps y
numOps (Float_Substract x y) = 1 + numOps x + numOps y
numOps (Float_Divide x y) = 1 + numOps x + numOps y
numOps (Float_Mult x y) = 1 + numOps x + numOps y
numOps (Float_Mod x y) = 1 + numOps x + numOps y
numOps (Float_Var _) = 0
numOps (Float_Abs x) = 1 + numOps x
numOps (Float_Exp x) = 1 + numOps x
numOps (Float_Sqrt x) = 1 + numOps x
numOps (Float_Log x) = 1 + numOps x
numOps (Float_Sine x) = 1 + numOps x
numOps (Float_Cosine x) = 1 + numOps x
numOps (Float_Arcsine x) = 1 + numOps x
numOps (Float_Arccosine x) = 1 + numOps x
numOps (Float_Arctangent x) = 1 + numOps x
numOps (Float_Floor x) = 1 + numOps x
numOps (Float_Noise x y) = 1 + numOps x + numOps y

-- | Number that indicates the maximum number of
--   operations allowed for a 'Proc_Float' calculation
--   to consider it cheap.
limitNumber :: Int
limitNumber = 3

-- | Check if a 'Proc_Float' calculation is expensive,
--   depending on 'limitNumber'.
isExpensive :: Proc_Float -> Bool
isExpensive = (> limitNumber) . numOps

-- | Check if a 'Proc_Float' calculation is cheap,
--   depending on 'limitNumber'.
isCheap :: Proc_Float -> Bool
isCheap = not . isExpensive

type FloatSet = HashMap Proc_Float Int

type FloatCounter = State FloatSet

-- | Add a 'Proc_Float' to the /float counter/.
addFloat :: Proc_Float -> FloatCounter ()
addFloat x = when (isExpensive x) $ modify $ insertWith (+) x 1

-- | Add each expression contained in a 'Proc_Float' to the
--   /float counter/.
browseFloat :: Proc_Float -> FloatCounter ()
-- Really boring function!
browseFloat f@(Float_Sum x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Substract x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Divide x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Mult x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Mod x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Abs x) = addFloat f >> browseFloat x
browseFloat f@(Float_Exp x) = addFloat f >> browseFloat x
browseFloat f@(Float_Sqrt x) = addFloat f >> browseFloat x
browseFloat f@(Float_Log x) = addFloat f >> browseFloat x
browseFloat f@(Float_Sine x) = addFloat f >> browseFloat x
browseFloat f@(Float_Cosine x) = addFloat f >> browseFloat x
browseFloat f@(Float_Arcsine x) = addFloat f >> browseFloat x
browseFloat f@(Float_Arccosine x) = addFloat f >> browseFloat x
browseFloat f@(Float_Arctangent x) = addFloat f >> browseFloat x
browseFloat f@(Float_Floor x) = addFloat f >> browseFloat x
browseFloat f@(Float_Noise x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat _ = return ()

execCounter :: FloatCounter a -> FloatSet
execCounter c = execState c empty
