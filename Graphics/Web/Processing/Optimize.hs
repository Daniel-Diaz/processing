
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

class (Ord e, Recursive e, ProcType e) => Optimizable e where
 numOps :: e -> Int
 browseExp :: e -> ExpCounter e ()
 getFromArg :: ProcArg -> Maybe e
 getFromAssign :: ProcAssign -> Maybe e

-- BOOL INSTANCE

instance Optimizable Proc_Bool where
 numOps (Proc_Neg x) = 1 + numOps x
 numOps (Proc_Or x y) = 1 + numOps x + numOps y
 numOps (Proc_And x y) = 1 + numOps x + numOps y
 numOps (Float_Eq x y) = 1 + numOps x + numOps y
 numOps (Float_NEq x y) = 1 + numOps x + numOps y
 numOps (Float_LE x y) = 1 + numOps x + numOps y
 numOps (Float_L x y) = 1 + numOps x + numOps y
 numOps (Float_GE x y) = 1 + numOps x + numOps y
 numOps (Float_G x y) = 1 + numOps x + numOps y
 numOps _ = 0
 getFromArg (BoolArg x) = Just x
 getFromArg _ = Nothing
 getFromAssign (BoolAssign _ x) = Just x
 getFromAssign _ = Nothing
 -- TODO
 browseExp b@(Proc_Neg x) = addExp b >> browseExp b
 browseExp b@(Proc_Or x y) = addExp b >> browseExp x >> browseExp y
 browseExp b@(Proc_And x y) = addExp b >> browseExp x >> browseExp y
 browseExp b = addExp b

-- INT INSTANCE

instance Optimizable Proc_Int where
 numOps (Int_Sum x y) = 1 + numOps x + numOps y
 numOps (Int_Substract x y) = 1 + numOps x + numOps y
 numOps (Int_Divide x y) = 1 + numOps x + numOps y
 numOps (Int_Mult x y) = 1 + numOps x + numOps y
 numOps (Int_Mod x y) = 1 + numOps x + numOps y
 numOps (Int_Abs x) = 1 + numOps x
 numOps (Int_Floor x) = 1 + numOps x
 numOps (Int_Round x) = 1 + numOps x
 numOps (Int_Cond b x y) = numOps b + max (numOps x) (numOps y)
 numOps _ = 0
 browseExp i@(Int_Sum x y) = addExp i >> browseExp x >> browseExp y
 browseExp i@(Int_Substract x y) = addExp i >> browseExp x >> browseExp y
 browseExp i@(Int_Divide x y) = addExp i >> browseExp x >> browseExp y
 browseExp i@(Int_Mult x y) = addExp i >> browseExp x >> browseExp y
 browseExp i@(Int_Mod x y) = addExp i >> browseExp x >> browseExp y
 browseExp i@(Int_Abs x) = addExp i
 browseExp i@(Int_Floor x) = addExp i
 browseExp i@(Int_Round x) = addExp i
 browseExp i@(Int_Cond b x y) = addExp i >> browseExp x >> browseExp y
 browseExp _ = return ()
 getFromArg (IntArg x) = Just x
 getFromArg _ = Nothing
 getFromAssign (IntAssign _ x) = Just x
 getFromAssign _ = Nothing

-- FLOAT INSTANCE

instance Optimizable Proc_Float where
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
 numOps (Float_Cond b x y) = numOps b + max (numOps x) (numOps y)
 -- Variable things are worth zero.
 numOps (Float_Var _) = 0
 numOps (Float_Random _ _) = 0
 -- Browsing a floating expression.
 browseExp f@(Float_Sum x y) = addExp f >> browseExp x >> browseExp y
 browseExp f@(Float_Substract x y) = addExp f >> browseExp x >> browseExp y
 browseExp f@(Float_Divide x y) = addExp f >> browseExp x >> browseExp y
 browseExp f@(Float_Mult x y) = addExp f >> browseExp x >> browseExp y
 browseExp f@(Float_Neg x) = addExp f >> browseExp x
 browseExp f@(Float_Mod x y) = addExp f >> browseExp x >> browseExp y
 browseExp f@(Float_Abs x) = addExp f >> browseExp x
 browseExp f@(Float_Exp x) = addExp f >> browseExp x
 browseExp f@(Float_Sqrt x) = addExp f >> browseExp x
 browseExp f@(Float_Log x) = addExp f >> browseExp x
 browseExp f@(Float_Sine x) = addExp f >> browseExp x
 browseExp f@(Float_Cosine x) = addExp f >> browseExp x
 browseExp f@(Float_Arcsine x) = addExp f >> browseExp x
 browseExp f@(Float_Arccosine x) = addExp f >> browseExp x
 browseExp f@(Float_Arctangent x) = addExp f >> browseExp x
 browseExp f@(Float_Floor x) = addExp f >> browseExp x
 browseExp f@(Float_Round x) = addExp f >> browseExp x
 browseExp f@(Float_Noise x y) = addExp f >> browseExp x >> browseExp y
 browseExp f@(Float_Cond _ x y) = addExp f >> browseExp x >> browseExp y
 browseExp _ = return ()
 --
 getFromArg (FloatArg x) = Just x
 getFromArg _ = Nothing
 getFromAssign (FloatAssign _ x) = Just x
 getFromAssign _ = Nothing


-----------------------------------------------------
-----------------------------------------------------
---- SUBSTITUTION OPTIMIZATION SETTINGS

-- | Maximum number of operations allowed for a
--   'Proc_Float' calculation to be considered cheap.
limitNumber :: Int
limitNumber = 0

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

getExpArgs :: Optimizable e => [ProcArg] -> Seq e
getExpArgs = F.foldr (
  \x xs -> case getFromArg x of
    Just a -> a Seq.<| xs
    _ -> xs) mempty

expsInCode :: Optimizable e => ProcCode c -> Seq e
expsInCode (Command _ xs) = getExpArgs xs
expsInCode (Conditional _ c1 c2) = expsInCode c1 <> expsInCode c2
expsInCode (Sequence xs) = F.foldMap expsInCode xs
expsInCode (Assignment a) =
 case getFromAssign a of
  Just x  -> Seq.singleton x
  _ -> mempty
expsInCode _ = mempty

type ExpCounter e = State (MultiSet e)

-- | Add a 'Proc_Float' to the /float counter/.
addExp :: Optimizable e => e -> ExpCounter e ()
addExp x = when (isExpensive x) $ modify $ insert x

execCounter :: ExpCounter e a -> MultiSet e
execCounter c = execState c empty

-- | Most frequent expensive expression within a list
--   of expressions.
--   It returns 'Nothing' when no expensive expression
--   was found, or they are not repeated enough (see 'occurNumber').
--   If there are more than one most frequent expression,
--   it returns one of them.
mostFreq :: Optimizable e => Seq e -> Maybe e
mostFreq xs = maxOccur mset
  where
    mset_ = F.foldr (\x y -> union y $ execCounter $ browseExp x) empty xs
    mset  = Data.MultiSet.filter (\x -> occur x mset_ >= occurNumber) mset_
    maxOccur = F.foldr f Nothing
    f a (Just b) =
       if occur a mset >= occur b mset
          then Just a
          else Just b
    f a Nothing = Just a

-- | Apply a substitution.
expsubs :: (Eq e, Recursive e)
          => e -- ^ Origin.
          -> e -- ^ Target.
          -> e -- ^ Expression.
          -> e -- ^ Result.
expsubs o t x = if x == o then t else recursor (expsubs o t) x

-- | Like 'mostFreq', but applied to a piece of code.
mostFreqCode :: Optimizable e => e -> ProcCode c -> Maybe e
mostFreqCode _ = mostFreq . expsInCode

optVarName :: Int -- ^ Index.
           -> Text -- ^ Optimization variable name.
optVarName n = "subs_" <> fromString (show n)

-- | Assign a /substitution variable/ a expression,
--   and use that variable in the rest of the code
--   instead of the original expression.
varForExp :: (ProcType e, Eq e, Recursive e)
          => Int -- ^ Substitution variable index.
          -> e -- ^ Expression to be substituted.
          -> ProcCode c -- ^ Original code.
          -> (ProcCode c, ProcCode c) -- ^ Assignment and result code.
varForExp n e c =
 ( Assignment (proc_assign v e) , codesubs e (proc_read $ varFromText v) c )
   where
     v = optVarName n

-- | Apply a substitution to a piece of code.
codesubs :: (ProcType e, Eq e, Recursive e)
         => e -- ^ Origin.
         -> e -- ^ Target.
         -> ProcCode c -- ^ Original code.
         -> ProcCode c -- ^ Result code.
codesubs o t (Command n xs) = Command n $ fmap (mapArg $ expsubs o t) xs
codesubs o t (Conditional b c1 c2) = Conditional b (codesubs o t c1) (codesubs o t c2)
codesubs o t (Sequence xs) = Sequence $ fmap (codesubs o t) xs
codesubs o t (Assignment a) = Assignment $ mapAssign (expsubs o t) a
codesubs _ _ c = c

substitutionOver :: Optimizable e => e -> Int -> ProcCode c -> (ProcCode c, Int)
substitutionOver aux = substitutionOverAux aux mempty

substitutionOverAux :: Optimizable e => e -> Seq (ProcCode c) -> Int -> ProcCode c -> (ProcCode c, Int)
substitutionOverAux aux as n c =
  case mostFreqCode aux c of
    Nothing -> (addSubsComments (F.fold as) <> c,n)
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
  let (c1,n1) = substitutionOver (undefined :: Proc_Float) n stack
      (c2,n2) = substitutionOver (undefined :: Proc_Int) n1 c1
      (c3,n3) = substitutionOver (undefined :: Proc_Bool) n2 c2
  addToWritten c3
  setIndex n3
  resetStack

codeSubstitution :: ProcCode c -> SubsM c ()
codeSubstitution a@(Assignment _) = addToStack a >> applySubstitution
codeSubstitution (Conditional b c1 c2) = do
  applySubstitution
  n0 <- substitutionIndex <$> get
  let (n1,c1') = runSubstitution n0 $ codeSubstitution c1 >> applySubstitution
      (n2,c2') = runSubstitution n1 $ codeSubstitution c2 >> applySubstitution
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
        -- vs = fmap (\n -> CreateVar $ FloatAssign (optVarName n) 0) [1 .. n5 - 1]
    in ProcScript (_preamble {-<> subsComment (mconcat vs)-})
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
