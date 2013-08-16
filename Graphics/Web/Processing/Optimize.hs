
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

-- | Code optimization module.
module Graphics.Web.Processing.Optimize (
   -- * Substitution Optimization
   -- ** Algorithm
   optimizeBySubstitution
   -- ** Properties
 , prop_optimizeBySubstitution_projection
   ) where

import Graphics.Web.Processing.Core.Primal
import Graphics.Web.Processing.Core.TH
import Data.MultiSet (MultiSet, insert, empty
                     , occur, filter)
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

class (Ord e, Recursive e, ProcType e) => Optimizable e where
 numOps :: e -> Int
 -- Browsing
 browseBool    :: Proc_Bool  -> ExpCounter e ()
 browseInt     :: Proc_Int   -> ExpCounter e ()
 browseFloat   :: Proc_Float -> ExpCounter e ()
 -- Replacing (replaceIn* :: original exp -> target exp
 --                       -> Proc_* -> Proc_*)
 replaceInBool  :: e -> e -> Proc_Bool  -> Proc_Bool
 replaceInInt   :: e -> e -> Proc_Int   -> Proc_Int
 replaceInFloat :: e -> e -> Proc_Float -> Proc_Float
 -- Defaults
 browseBool _  = return ()
 browseInt  _  = return ()
 browseFloat _ = return ()

browseArgs :: Optimizable e => [ProcArg] -> ExpCounter e ()
browseArgs [] = return ()
browseArgs (x:xs) = case x of
  BoolArg  e -> browseBool  e >> browseArgs xs
  IntArg   e -> browseInt   e >> browseArgs xs
  FloatArg e -> browseFloat e >> browseArgs xs
  _          -> return ()

browseAssign :: Optimizable e => ProcAssign -> ExpCounter e ()
browseAssign (BoolAssign  _ e) = browseBool  e
browseAssign (IntAssign   _ e) = browseInt   e
browseAssign (FloatAssign _ e) = browseFloat e
browseAssign _ = return ()

browseCode :: Optimizable e => ProcCode c -> ExpCounter e ()
browseCode (Command _ xs) = browseArgs xs
browseCode (Conditional b c1 c2) = browseBool b >> browseCode c1 >> browseCode c2
browseCode (Sequence xs) = F.mapM_ browseCode xs
browseCode (Assignment a) = browseAssign a
browseCode _ = return ()

replaceInArg :: Optimizable e => e -> e -> ProcArg -> ProcArg
replaceInArg o t (BoolArg  e) = BoolArg  $ replaceInBool  o t e
replaceInArg o t (IntArg   e) = IntArg   $ replaceInInt   o t e
replaceInArg o t (FloatArg e) = FloatArg $ replaceInFloat o t e
replaceInArg _ _ a = a

replaceInAssign :: Optimizable e => e -> e -> ProcAssign -> ProcAssign
replaceInAssign o t (BoolAssign  n e) = BoolAssign  n $ replaceInBool  o t e
replaceInAssign o t (IntAssign   n e) = IntAssign   n $ replaceInInt   o t e
replaceInAssign o t (FloatAssign n e) = FloatAssign n $ replaceInFloat o t e
replaceInAssign _ _ a = a

replaceInCode :: Optimizable e => e -> e -> ProcCode c -> ProcCode c
replaceInCode o t (Command n xs) = Command n $ fmap (replaceInArg o t) xs
replaceInCode o t (Conditional b c1 c2) =
  Conditional (replaceInBool o t b)
              (replaceInCode o t c1)
              (replaceInCode o t c2)
replaceInCode o t (Sequence xs) = Sequence $ fmap (replaceInCode o t) xs
replaceInCode o t (Assignment a) = Assignment $ replaceInAssign o t a
replaceInCode _ _ c = c

-----------------------------------------------------
-----------------------------------------------------
---- SUBSTITUTION OPTIMIZATION SETTINGS

-- | Maximum number of operations allowed for a
--   'Proc_Float' calculation to be considered cheap.
limitNumber :: Int
limitNumber = 1

-- | Number of times an expression is considered
--   repeated enough to be substituted.
occurNumber :: Int
occurNumber = 2

-----------------------------------------------------
-----------------------------------------------------

-- | Check if a 'Proc_Float' calculation is expensive,
--   depending on 'limitNumber'.
isExpensive :: Optimizable e => e -> Bool
isExpensive = (> limitNumber) . numOps

type ExpCounter e = State (MultiSet e)

-- | Add a 'Proc_Float' to the /float counter/.
addExp :: Optimizable e => e -> ExpCounter e ()
addExp x = when (isExpensive x) $ modify $ insert x

execCounter :: ExpCounter e a -> MultiSet e
execCounter c = execState c empty

-- | Most frequent expensive expression within a piece of code.
--   It returns 'Nothing' when no expensive expression
--   was found, or they are not repeated enough (see 'occurNumber').
--   If there are more than one most frequent expression,
--   it returns one of them.
mostFreq :: Optimizable e => e -> ProcCode c -> Maybe e
mostFreq _ c = maxOccur mset
  where
    mset_ = execCounter $ browseCode c
    mset  = Data.MultiSet.filter (\x -> occur x mset_ >= occurNumber) mset_
    maxOccur = F.foldr f Nothing
    f a (Just b) =
       if occur a mset >= occur b mset
          then Just a
          else Just b
    f a Nothing = Just a

optVarName :: Int -- ^ Index.
           -> Text -- ^ Optimization variable name.
optVarName n = "subs_" <> fromString (show n)

-- | Assign a /substitution variable/ a expression,
--   and use that variable in the rest of the code
--   instead of the original expression.
varForExp :: Optimizable e
          => Int -- ^ Substitution variable index.
          -> e   -- ^ Expression to be substituted.
          -> ProcCode c -- ^ Original code.
          -> (ProcCode c, ProcCode c) -- ^ Assignment and result code.
varForExp n e c =
 ( Assignment (proc_assign v e) , replaceInCode e (proc_read $ varFromText v) c )
   where
     v = optVarName n

substitutionOver :: Optimizable e => e -> Int -> ProcCode c
                 -> (ProcCode c,ProcCode c, Int) -- (Assignments, Code substituted, Updated counter)
substitutionOver aux = substitutionOverAux aux mempty

substitutionOverAux :: Optimizable e => e -> Seq (ProcCode c) -> Int -> ProcCode c -> (ProcCode c, ProcCode c, Int)
substitutionOverAux aux as n c =
  case mostFreq aux c of
    Nothing -> (addSubsComments (F.fold as), c,n)
    Just e  -> let (a,c') = varForExp n e c
               in  substitutionOverAux aux (as Seq.|> a) (n+1) c'

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
                             , substitutionIndex :: Int
                             , mutatedVariables :: [Text] }

type SubsM c = State (SubsState c)

addToStack :: ProcCode c -> SubsM c ()
addToStack c = modify $ \s -> s { codeStack = codeStack s <> c }

addToWritten :: ProcCode c -> SubsM c ()
addToWritten c = modify $ \s -> s { codeWritten = codeWritten s <> c }

setIndex :: Int -> SubsM c ()
setIndex n = modify $ \s -> s { substitutionIndex = n }

resetStack :: SubsM c ()
resetStack = modify $ \s -> s { codeStack = mempty }

mutateVariable :: Text -> SubsM c ()
mutateVariable t = modify $ \s -> s { mutatedVariables = t : mutatedVariables s }

cleanVariables :: SubsM c ()
cleanVariables = modify $ \s -> s { mutatedVariables = [] }

isVarInCode :: Text -> ProcCode c -> Bool
isVarInCode t (Command _ as) = foldr (\a r -> isVarInArg t a || r) False as
isVarInCode t (Assignment a) = isVarInAssign t a
isVarInCode t (Conditional b c1 c2) = checkForVar t b || isVarInCode t c1 || isVarInCode t c2
isVarInCode t (Sequence xs) = F.foldr (\c r -> isVarInCode t c || r) False xs
isVarInCode _ _ = False

{- Apply substitution

Get the current stack, apply the optimization to it,
append the result as written code and reset the stack.

The order in which the substitutions for the different
types are made matters. The most frequent type should
be first.

-}
applySubstitution :: SubsM c ()
applySubstitution = do
  stack <- codeStack <$> get
  n <- substitutionIndex <$> get
  let (s1,c1,n1) = substitutionOver (undefined :: Proc_Float) n stack
  addToWritten s1
  let (s2,c2,n2) = substitutionOver (undefined :: Proc_Int) n1 c1
  addToWritten s2
  let (s3,c3,n3) = substitutionOver (undefined :: Proc_Bool) n2 c2
  addToWritten s3
  addToWritten c3
  setIndex n3
  resetStack

addWithMutations :: ProcCode c -> SubsM c ()
addWithMutations c = do
  vs <- mutatedVariables <$> get
  let b = any (\v -> isVarInCode v c) vs
  if b then applySubstitution >> cleanVariables >> addToStack c
       else addToStack c

codeSubstitution :: ProcCode c -> SubsM c ()
codeSubstitution c@(Command _ _) = addWithMutations c
codeSubstitution c@(Assignment a) = addWithMutations c >> mutateVariable (assignVarName a)
codeSubstitution (Conditional b c1 c2) = do
  applySubstitution
  n0 <- substitutionIndex <$> get
  let (n1,c1') = runSubstitution n0 $ codeSubstitution c1 >> applySubstitution
      (n2,c2') = runSubstitution n1 $ codeSubstitution c2 >> applySubstitution
  setIndex n2
  addToWritten $ Conditional b c1' c2'
codeSubstitution (Sequence xs) = F.mapM_ codeSubstitution xs
codeSubstitution c = addToStack c

runSubstitution :: Int -> SubsM c a -> (Int,ProcCode c)
runSubstitution n m = (substitutionIndex s, codeWritten s)
  where
    (_,s) = runState m $ SubsState mempty mempty n []

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
        (_,_keyPressed')    = maybe (n4,Nothing) (second Just . subsOptimize n4) _keyPressed
        -- vs = fmap (\n -> CreateVar $ FloatAssign (optVarName n) 0) [1 .. n5 - 1]
    in ProcScript (_preamble {-<> subsComment (mconcat vs)-})
                   _setup'
                   _draw'
                   _mouseClicked'
                   _mouseReleased'
                   _keyPressed'

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


$(deriveOptimizable)
