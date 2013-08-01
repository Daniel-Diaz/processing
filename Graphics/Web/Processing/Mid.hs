
-- | Processing scripting, /mid/ interface.
--   Unlike the /basic/ interface (see "Graphics.Web.Processing.Basic")
--   the script is more guided by the types. However, the output is
--   less predictable, since it does some tricks in order to obtain
--   semantics that are more coherent with Haskell. The difference is
--   small, but let's say that this module has more freedom writing
--   the output code.
--
--   /How to work with it?/
--
--   Everything is done within
--   the 'ScriptM' monad, a state monad that controls the entire script,
--   including the preamble, draw loop, setup, etc.
--   The interaction with the different parts of the script is done
--   via /events/ (see 'EventM'). For example, the 'Draw' event controls the draw
--   loop.
--
-- > mouse :: ScriptM Preamble ()
-- > mouse = do
-- >   on Setup $ do
-- >      size screenWidth screenHeight
-- >      fill $ Color 255 255 255 255
-- >   on Draw  $ do
-- >      background $ Color 0 0 0 255
-- >      p <- getMousePoint
-- >      circle p 10
--
--   Note that to make it work, the context of the script /must/ be
--   'Preamble'.
--
--   Interaction with variables is done via the 'ProcVarMonad' class.
--   This class defines methods to interact with variables in both the
--   'ScriptM' monad and the 'EventM' monad.
--   To store custom types in variables, see the
--   "Graphics.Web.Processing.Mid.CustomVar" module.
--
--   Once your script is complete, use 'execScriptM' to get the result
--   code.
module Graphics.Web.Processing.Mid (
    -- * Types
    module Graphics.Web.Processing.Core.Types
    -- * Contexts
  , Context
    -- * Events
  , EventM
    -- * Script
  , ScriptM
  , on
  , execScriptM
    -- * Variables
  , Var
  , varName
  , ProcVarMonad (..)
    -- * Interface
  , module Graphics.Web.Processing.Core.Interface
  ) where

import Graphics.Web.Processing.Core.Monad
import Graphics.Web.Processing.Core.Types
import Graphics.Web.Processing.Core.Interface
-- variables
import Graphics.Web.Processing.Core.Var (Var,varName)
import qualified Graphics.Web.Processing.Core.Var as Var
-- optimization
import Graphics.Web.Processing.Optimize
-- transformers
import Control.Monad (void)
import Control.Applicative
import Control.Monad.Trans.State.Strict
-- monoids
import Data.Monoid
import Data.Foldable (foldMap)
-- unsafe!
import Unsafe.Coerce

data EventState c =
  EventState
    { -- This field allows to append preamble code
      -- during an event. Only to be used for internal
      -- functions.
      event_preamble :: ProcM Preamble ()
    , event_code :: ProcM c ()
      }

-- | Plain empty event state.
emptyEventState :: EventState c
emptyEventState = EventState (return ()) (return ())

-- | Monad of events. Use 'on' to insert an event in a script ('ScriptM').
--   To write the event code, use the functions in
--   "Graphics.Web.Processing.Core.Interface", since 'EventM' is an instance
--   of 'ProcMonad'.
newtype EventM c a = EventM { unEventM :: State (EventState c) a }

instance Functor (EventM c) where
 fmap f (EventM s) = EventM $ fmap f s

instance Applicative (EventM c) where
 pure x = EventM $ pure x
 ef <*> e = EventM $ unEventM ef <*> unEventM e

instance Monad (EventM c) where
 return = pure
 (EventM s) >>= f = EventM $ s >>= unEventM . f

addCode :: ProcM c () -> EventM c ()
addCode = liftProc

addPCode :: ProcM Preamble () -> EventM c ()
addPCode p = EventM $ modify $ \es -> es { event_preamble = event_preamble es >> p }

instance ProcMonad EventM where
 liftProc p = EventM $ do
   es <- get
   let c = event_code es >> p
   put $ es { event_code = void c }
   return $ fst $ runProcM c
 commandM t as = addCode $ commandM t as
 assignM = addCode . assignM
 writeComment = addCode . writeComment
 iff b (EventM e1) (EventM e2) = do
   let s1 = execState e1 emptyEventState
       s2 = execState e2 emptyEventState
   addPCode $ event_preamble s1
   addPCode $ event_preamble s2
   addCode $ iff b (event_code s1) (event_code s2)
 -- Create variables in an event? That should never happen, really.
 createVarM = fail "EventM(createVarM): This error should never be called. Report this as an issue."

data ScriptState c =
  ScriptState
    { script_code :: ProcM c () -- This code should actually be the preamble
    , script_setup :: Maybe (ProcM Setup ())
    , script_draw :: Maybe (ProcM Draw ())
    , script_mouseClicked :: Maybe (ProcM MouseClicked ())
      }

emptyScriptState :: ScriptState c
emptyScriptState = ScriptState (return ()) Nothing Nothing Nothing

-- | Scripter monad. This monad is where Processing code is written.
--   Because of some implementation details, 'ScriptM' has a context @c@.
--   However, this context is /always/ 'Preamble'.
newtype ScriptM c a = ScriptM { unScriptM :: State (ScriptState c) a }

instance Functor (ScriptM c) where
 fmap f (ScriptM s) = ScriptM $ fmap f s

instance Applicative (ScriptM c) where
 pure x = ScriptM $ pure x
 ef <*> e = ScriptM $ unScriptM ef <*> unScriptM e

instance Monad (ScriptM c) where
 return = pure
 (ScriptM s) >>= f = ScriptM $ s >>= unScriptM . f

instance ProcMonad ScriptM where
 liftProc p = ScriptM $ do
   ss <- get
   let c = script_code ss >> p
   put $ ss { script_code = void c }
   return $ fst $ runProcM $ c
 commandM t as = liftProc $ commandM t as
 assignM = liftProc . assignM
 createVarM = liftProc . createVarM
 writeComment = liftProc . writeComment
 iff b (ScriptM e1) (ScriptM e2) = ScriptM $ do
   s0 <- get
   let s1  = execState e1 emptyScriptState
       s2  = execState e2 emptyScriptState
       f g = getLast $ foldMap (Last . g) [s0,s1,s2]
   put $ ScriptState (script_code s0 >> iff b (script_code s1) (script_code s2))
                     (f script_setup)
                     (f script_draw)
                     (f script_mouseClicked)

-- | Context of an event. The context determines which functions can be used.
--   'Preamble' is not an instance of 'Context' to avoid using 'Preamble' as
--   an event (see 'on').
class Context c where
 addEvent :: c -> ProcM c () -> ScriptState d -> ScriptState d

instance Context Setup where
 addEvent _ c s = s { script_setup = Just c }

instance Context Draw where
 addEvent _ c s = s { script_draw = Just c }

instance Context MouseClicked where
 addEvent _ c s = s { script_mouseClicked = Just c }

-- | Set an event. Different events are specified by the instances of the
--   'Context' class.
--
--   For example, the following code sets the 'fill' pattern in the setup event (the event
--   that is called once at the beginning of the execution).
--
-- > on Setup $ fill $ Color 0 0 0 255
--
on :: Context c => c -> EventM c () -> ScriptM Preamble ()
on c (EventM e) = ScriptM $ modify $ \ss -> 
 let n = fst $ runProcM $ script_code ss >> getVarNumber
     es = execState e $ EventState (return ()) $ setVarNumber n
     f  = addEvent c $ event_code es
 in  f $ ss { script_code = script_code ss >> event_preamble es }

-- | Execute the scripter monad to get the full Processing script.
--   Use 'renderScript' or 'renderFile' to render it.
execScriptM :: ScriptM Preamble () -> ProcScript
execScriptM (ScriptM s0) =
  let s = execState s0 emptyScriptState
  in  optimizeBySubstitution $ ProcScript
    { proc_preamble = execProcM $ script_code s
    , proc_setup = maybe mempty execProcM $ script_setup s
    , proc_draw = fmap execProcM $ script_draw s
    , proc_mouseClicked = fmap execProcM $ script_mouseClicked s
      }

-- Variables

-- | Class of monads where variables can be set in a natural
--   way (similar to 'IORef'). Instances of this class behave
--   in a proper way, without the weird behavior of the original
--   'Var.readVar'.
class ProcMonad m => ProcVarMonad m where
 -- | Create a new variable with a starting value.
 newVar :: ProcType a => a -> m Preamble (Var a)
 -- | Read a variable.
 readVar :: ProcType a => Var a -> m c a
 -- | Write a new value to a variable.
 writeVar :: ProcType a => Var a -> a -> m c ()

-- | Magic! Keep it private, it's our secret!
switchContext :: ScriptM c a -> ScriptM d a
switchContext = unsafeCoerce

-- | Magic! Keep it private, it's our secret!
switchContextE :: EventM c a -> EventM d a
switchContextE = unsafeCoerce

instance ProcVarMonad ScriptM where
 newVar = Var.newVar
 writeVar = Var.writeVar
 readVar v = do
  x  <- Var.readVar v
  v' <- switchContext $ Var.newVar x
  Var.readVar v'

instance ProcVarMonad EventM where
 writeVar = Var.writeVar
 readVar v = do
  x <- Var.readVar v
  addPCode $ void $ Var.newVar x
  n <- switchContextE $ liftProc getVarNumber
  let v' = fst $ runProcMWith n $ Var.newVar x
  liftProc $ setVarNumber $ n + 1
  writeVar v' x
  Var.readVar v'
 -- New variable in an event? That should not happen, really.
 newVar = fail "EventM(newVar): This error should never be called. Report this as an issue."
