
-- | Processing code writer monad.
module Graphics.Web.Processing.Core.Monad (
    ProcM
  , runProcM, execProcM
  , runProcMWith
  , ProcMonad (..)
  , newVarNumber
  , getVarNumber
  , setVarNumber
  ) where

import Control.Arrow (second)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State.Strict
import Graphics.Web.Processing.Core.Primal
import Control.Applicative (Applicative (..))
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.String (fromString)

-- | Processing script producer monad. The context @c@ indicates the context
--   of the underlying 'ProcCode'. This context restricts the use of certain
--   commands only to places where they are expected.
--
--   The commands that you can run under this monad are mostly defined in
--   "Graphics.Web.Processing.Interface".
--
--   Once you have all the commands you want, use 'runProcM' or 'execProcM'
--   to generate the corresponding Processing code under the 'ProcCode' type.
newtype ProcM c a = ProcM { unProcM :: StateT Int (Writer (ProcCode c)) a }

-- | Generate Processing code using the 'ProcM' monad.
--   The code output is reduced.
runProcM :: ProcM c a -> (a,ProcCode c)
runProcM = runProcMWith 0

-- | Run a 'ProcM' computation with an initial var number.
--   It also applies a reduction to the output Processing code.
runProcMWith :: Int -> ProcM c a -> (a,ProcCode c)
runProcMWith n = second reduce . runWriter . (\sw -> evalStateT sw n) . unProcM

-- | Generate Processing code using the 'ProcM' monad, discarding the final
--   value.
--
-- > execProcM = snd . runProcM
--
execProcM :: ProcM c a -> ProcCode c
execProcM = snd . runProcM

instance Functor (ProcM c) where
 fmap f (ProcM w) = ProcM $ fmap f w
 
instance Applicative (ProcM c) where
 pure x = ProcM $ pure x
 pf <*> p = ProcM $ unProcM pf <*> unProcM p

instance Monad (ProcM c) where
 return = pure
 (ProcM w) >>= f = ProcM $ w >>= unProcM . f

-- | Adds @1@ to the variable counter and returns the result.
newVarNumber :: ProcM c Int
newVarNumber = ProcM $ modify (+1) >> get

-- | Get the current variable number.
getVarNumber :: ProcM c Int
getVarNumber = ProcM get

-- | Set the current variable number.
setVarNumber :: Int -> ProcM c ()
setVarNumber = ProcM . put

intVarName :: Int -> Text
intVarName n = "v_" <> fromString (show n)

-- Processing Monad class

-- | Types in this instance form a monad when they are applied
--   to a context @c@. Then, they are used to write Processing
--   code.
class ProcMonad m where
 -- | Internal function to process commands in the target monad.
 commandM :: Text -> [ProcArg] -> m c ()
 -- | Internal function to process asignments in the target monad.
 assignM :: ProcAsign -> m c ()
 -- | Internal function to process variable creations in the target monad.
 createVarM :: ProcAsign -> m c ()
 -- | Write a comment in the code.
 writeComment :: Text -> m c ()
 -- | Conditional execution.
 iff :: Proc_Bool -- ^ Condition.
     -> m c a -- ^ Execution when the condition is 'true'.
     -> m c b -- ^ Execution when the condition is 'false'.
     -> m c ()
 -- | Lift a 'ProcM' computation.
 liftProc :: ProcM c a -> m c a
 -- | Create a new variable with a starting value.
 newVar :: ProcType a => a -> m Preamble (Var a)
 -- | Read a variable.
 readVar :: ProcType a => Var a -> m c a
 -- | Write a new value to a variable.
 writeVar :: ProcType a => Var a -> a -> m c ()

-- | When using this instance, please, be aware of the
--   behavior of 'readVar'.
--
--   /It does not matter when read the variable/.
--   The result will /always/ hold the last value asigned to the variable.
--   For example, this code
--
-- > v <- newVar 10
-- > ten <- readVar v
-- > writeVar v 20
-- > point (10,ten)
--
--   will draw a point at (10,20).
instance ProcMonad ProcM where
 commandM n as = ProcM $ lift $ tell $ Command n as
 assignM = ProcM . lift . tell . Assignment
 createVarM = ProcM . lift . tell . CreateVar
 writeComment = ProcM . lift . tell . Comment
 iff b (ProcM e1) (ProcM e2) = ProcM $ do
   i0 <- get
   let (i1,c1) = runWriter $ execStateT e1 i0
       (i2,c2) = runWriter $ execStateT e2 i1
   put i2
   lift $ tell $ Conditional b c1 c2
 liftProc = id
 newVar x = do
   n <- newVarNumber
   let v = intVarName n
   createVarM (proc_asign v x)
   return $ varFromText v
 readVar = return . proc_read
 writeVar v x = assignM $ proc_asign (varName v) x
