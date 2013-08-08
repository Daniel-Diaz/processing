
{-# LANGUAGE OverloadedStrings #-}

-- | Code optimization module.
module Graphics.Web.Processing.Optimize (
   -- * Substitution Optimization
   -- ** Algorithm
   optimizeBySubstitution
   -- ** Properties
 , prop_optimizeBySubstitution_projection
   ) where

import Graphics.Web.Processing.Core.Primal
import Data.MultiSet (MultiSet, insert, empty
                     , occur, union, filter)
import Control.Monad (when)
import Control.Monad.Trans.State
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.String
import Data.Text (Text)
import Control.Applicative ((<$>))
import Control.Arrow (second)

boolops :: Proc_Bool -> Int
boolops (Proc_Neg x) = 1 + boolops x
boolops (Proc_Or x y) = 1 + boolops x + boolops y
boolops (Proc_And x y) = 1 + boolops x + boolops y
boolops (Float_Eq x y) = 1 + numOps x + numOps y
boolops (Float_NEq x y) = 1 + numOps x + numOps y
boolops (Float_LE x y) = 1 + numOps x + numOps y
boolops (Float_L x y) = 1 + numOps x + numOps y
boolops (Float_GE x y) = 1 + numOps x + numOps y
boolops (Float_G x y) = 1 + numOps x + numOps y
boolops _ = 0

-- | Number of operations needed to calculate the
--   value of a given 'Proc_Float' value.
numOps :: Proc_Float -> Int
-- Really boring function!
numOps (Proc_Float _) = 0
numOps (Float_Sum x y) = 1 + numOps x + numOps y
numOps (Float_Substract x y) = 1 + numOps x + numOps y
numOps (Float_Divide x y) = 1 + numOps x + numOps y
numOps (Float_Mult x y) = 1 + numOps x + numOps y
numOps (Float_Neg x) = 1 + numOps x
numOps (Float_Mod x y) = 1 + numOps x + numOps y
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
numOps (Float_Round x) = 1 + numOps x
numOps (Float_Noise x y) = 1 + numOps x + numOps y
numOps (Float_Cond b x y) = boolops b + max (numOps x) (numOps y)
-- Variable things are worth zero.
numOps (Float_Var _) = 0
numOps (Float_Random _ _) = 0

-----------------------------------------------------
-----------------------------------------------------
---- SUBSTITUTION OPTIMIZATION SETTINGS

-- | Number that indicates the maximum number of
--   operations allowed for a 'Proc_Float' calculation
--   to consider it cheap.
limitNumber :: Int
limitNumber = 0

-- | Number of times an expression is considered
--   repeated enough to be substituted.
occurNumber :: Int
occurNumber = 3

-----------------------------------------------------
-----------------------------------------------------

-- | Check if a 'Proc_Float' calculation is expensive,
--   depending on 'limitNumber'.
isExpensive :: Proc_Float -> Bool
isExpensive = (> limitNumber) . numOps

{- Currently unused

-- | Check if a 'Proc_Float' calculation is cheap,
--   depending on 'limitNumber'.
isCheap :: Proc_Float -> Bool
isCheap = not . isExpensive

-}

type FloatSet = MultiSet Proc_Float

type FloatCounter = State FloatSet

-- | Add a 'Proc_Float' to the /float counter/.
addFloat :: Proc_Float -> FloatCounter ()
addFloat x = when (isExpensive x) $ modify $ insert x

-- | Add each expression contained in a 'Proc_Float' to the
--   /float counter/.
browseFloat :: Proc_Float -> FloatCounter ()
-- Really boring function!
browseFloat f@(Float_Sum x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Substract x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Divide x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Mult x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Neg x) = addFloat f >> browseFloat x
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
browseFloat f@(Float_Round x) = addFloat f >> browseFloat x
browseFloat f@(Float_Noise x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat f@(Float_Cond _ x y) = addFloat f >> browseFloat x >> browseFloat y
browseFloat _ = return ()

execCounter :: FloatCounter a -> FloatSet
execCounter c = execState c empty

-- | Most frequent expensive expression within a list
--   of expressions.
--   It returns 'Nothing' when no expensive expression
--   was found, or they are not repeated enough (see 'occurNumber').
--   If there are more than one most frequent expression,
--   it returns one of them.
mostFreq :: Seq Proc_Float -> Maybe Proc_Float
mostFreq xs = maxOccur mset
  where
    mset_ = F.foldr (\x y -> union y $ execCounter $ browseFloat x) empty xs
    mset  = Data.MultiSet.filter (\x -> occur x mset_ >= occurNumber) mset_
    maxOccur = F.foldr f Nothing
    f a (Just b) =
       if occur a mset >= occur b mset
          then Just a
          else Just b
    f a Nothing = Just a

-- | Apply a substitution.
floatsubs :: Proc_Float -- ^ Origin.
          -> Proc_Float -- ^ Target.
          -> Proc_Float -- ^ Expression.
          -> Proc_Float -- ^ Result.
floatsubs o t x = if x == o then t else recFloat (floatsubs o t) x

-- | From a list of arguments, create a sequence of the
--   arguments of type 'Proc_Float' (which may be empty).
getFloatArgs :: [ProcArg] -> Seq Proc_Float
getFloatArgs = F.foldr (
  \x xs -> case x of
    FloatArg a -> a Seq.<| xs
    _ -> xs) mempty

-- | Gather all the float expressions in a piece of code.
floatsInCode :: ProcCode c -> Seq Proc_Float
floatsInCode (Command _ xs) = getFloatArgs xs
floatsInCode (Conditional _ c1 c2) = floatsInCode c1 <> floatsInCode c2
floatsInCode (Sequence xs) = F.foldMap floatsInCode xs
floatsInCode (Assignment (FloatAsign _ x)) = Seq.singleton x
floatsInCode _ = mempty

-- | Like 'mostFreq', but applied to a piece of code.
mostFreqCode :: ProcCode c -> Maybe Proc_Float
mostFreqCode = mostFreq . floatsInCode

optVarName :: Int -- ^ Index.
           -> Text -- ^ Optimization variable name.
optVarName n = "subs_" <> fromString (show n)

-- | Assign a /substitution variable/ a expression,
--   and use that variable in the rest of the code.
varForExp :: Int -- ^ Substitution variable index.
          -> Proc_Float -- ^ Expressions to be substituted.
          -> ProcCode c -- ^ Original code.
          -> (ProcCode c, ProcCode c) -- ^ Assignment and result code.
varForExp n e c =
 ( Assignment (FloatAsign v e) , codesubs e (Float_Var v) c )
   where
     v = optVarName n

-- | Apply a substitution to a floating argument.
--   To other arguments, it does nothing.
argsubs :: Proc_Float -- ^ Origin.
        -> Proc_Float -- ^ Target.
        -> ProcArg    -- ^ Original argument.
        -> ProcArg    -- ^ Result argument.
argsubs o t (FloatArg x) = FloatArg $ floatsubs o t x
argsubs _ _ x = x

-- | Apply a substitution to a piece of code.
codesubs :: Proc_Float -- ^ Origin.
         -> Proc_Float -- ^ Target.
         -> ProcCode c -- ^ Original code.
         -> ProcCode c -- ^ Result code.
codesubs o t (Command n xs) = Command n $ fmap (argsubs o t) xs
codesubs o t (Conditional b c1 c2) = Conditional b (codesubs o t c1) (codesubs o t c2)
codesubs o t (Sequence xs) = Sequence $ fmap (codesubs o t) xs
codesubs o t (Assignment (FloatAsign n x)) = Assignment $ FloatAsign n $ floatsubs o t x
codesubs _ _ c = c

substitutionOver :: Int -> ProcCode c -> (ProcCode c, Int)
substitutionOver = substitutionOverAux mempty

substitutionOverAux :: Seq (ProcCode c) -> Int -> ProcCode c -> (ProcCode c, Int)
substitutionOverAux as n c =
  case mostFreqCode c of
    Nothing -> (addSubsComments (F.fold as) <> c,n)
    Just e  -> let (a,c') = varForExp n e c
               in  substitutionOverAux (as Seq.|> a) (n+1) c'

addSubsComments :: ProcCode c -> ProcCode c
addSubsComments c =
  if c == mempty then mempty
                 else subsPrevComment <> c <> subsPostComment

subsPrevComment :: ProcCode c
subsPrevComment = Comment "Substitution Optimization settings."

subsPostComment :: ProcCode c
subsPostComment = Comment " "

-- Substitution optimization monad.

data SubsState c = SubsState { codeWritten :: ProcCode c
                             , codeStack :: ProcCode c
                             , substitutionIndex :: Int }

type SubsM c = State (SubsState c)

addToStack :: ProcCode c -> SubsM c ()
addToStack c = modify $ \s -> s { codeStack = codeStack s <> c }

addToWritten :: ProcCode c -> SubsM c ()
addToWritten c = modify $ \s -> s { codeWritten = codeWritten s <> c }

setIndex :: Int -> SubsM c ()
setIndex n = modify $ \s -> s { substitutionIndex = n }

resetStack :: SubsM c ()
resetStack = modify $ \s -> s { codeStack = mempty }

applySubstitution :: SubsM c ()
applySubstitution = do
  stack <- codeStack <$> get
  n <- substitutionIndex <$> get
  let (c,m) = substitutionOver n stack
  addToWritten c
  setIndex m
  resetStack

codeSubstitution :: ProcCode c -> SubsM c ()
codeSubstitution a@(Assignment _) = addToStack a >> applySubstitution
codeSubstitution (Conditional b c1 c2) = do
  applySubstitution
  n0 <- substitutionIndex <$> get
  let (n1,c1') = runSubstitution n0 $ codeSubstitution c1
      (n2,c2') = runSubstitution n1 $ codeSubstitution c2
  setIndex n2
  addToWritten $ Conditional b c1' c2'
codeSubstitution (Sequence xs) = F.mapM_ codeSubstitution xs
codeSubstitution x = addToStack x

runSubstitution :: Int -> SubsM c a -> (Int,ProcCode c)
runSubstitution n m = (substitutionIndex s, codeWritten s)
  where
    (_,s) = runState m $ SubsState mempty mempty n

subsOptimize :: Int -> ProcCode c -> (Int,ProcCode c)
subsOptimize n c = runSubstitution n $ codeSubstitution c >> applySubstitution

-- | Optimization by substitution. It looks for commonly repeated operations and
--   create variables for them so they are only calculated once.
--
--   This optimization is applied automatically when using 'execScriptM'.
--
--   Look at the generated to code to see which substitutions have been made.
--   They are delimited by comments, with title /Substitution Optimization settings/.
--   If this is not present, no substitution has been made.
optimizeBySubstitution :: ProcScript -> ProcScript
optimizeBySubstitution
  (ProcScript _preamble
              _setup
              _draw
              _mouseClicked
              _mouseReleased
              _keyPressed
               )
  = let (n1,_setup')         = subsOptimize 1 _setup
        (n2,_draw')          = maybe (n1,Nothing) (second Just . subsOptimize n1) _draw
        (n3,_mouseClicked')  = maybe (n2,Nothing) (second Just . subsOptimize n2) _mouseClicked
        (n4,_mouseReleased') = maybe (n3,Nothing) (second Just . subsOptimize n3) _mouseReleased
        (n5,_keyPressed')    = maybe (n4,Nothing) (second Just . subsOptimize n4) _keyPressed
        vs = fmap (\n -> CreateVar $ FloatAsign (optVarName n) 0) [1 .. n5 - 1]
    in ProcScript (_preamble <> subsComment (mconcat vs))
                   _setup'
                   _draw'
                   _mouseClicked'
                   _mouseReleased'
                   _keyPressed'

subsComment :: ProcCode Preamble -> ProcCode Preamble
subsComment c =
 if c == mempty then mempty
                else Comment "Variables from the Substitution Optimization." <> c

-- | Optimizations are projections. In particular:
--
-- > let f = optimizeBySubstitution
-- > in  f x == f (f x)
--
--   This function checks that this equality holds for a given @x@.
--   Apply it to your own script to check that the property is true.
--   Tests has been applied to randomly generated scripts, but for
--   them, @f@ ≈ @id@.
prop_optimizeBySubstitution_projection :: ProcScript -> Bool
prop_optimizeBySubstitution_projection x =
 let f = optimizeBySubstitution
     y = f x
 in  y == f y
