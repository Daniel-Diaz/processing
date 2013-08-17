
-- | Processing scripting, /mid/ interface.
--   Unlike the /basic/ interface (see "Graphics.Web.Processing.Basic")
--   the script is more guided by the types. However, the output is
--   less predictable, since it does some tricks in order to obtain
--   semantics that are more coherent with Haskell. The difference is
--   small, but let's say that this module has more freedom writing
--   the output code. It also applies code optimizations, so the output
--   code may look different (see 'execScriptM' and
--   "Graphics.Web.Processing.Optimize").
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
--   Interaction with variables is done via the interface provided by
--   the "Graphics.Web.Processing.Core.Var" module.
--   This module defines functions to interact with variables in both the
--   'ScriptM' monad and the 'EventM' monad.
--   To store custom types in variables, see the
--   "Graphics.Web.Processing.Mid.CustomVar" module (you have to import
--   this module separately).
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
  , execScriptMFast
    -- * Variables
  , module Graphics.Web.Processing.Core.Var
    -- * Interface
  , module Graphics.Web.Processing.Core.Interface
  ) where

import Graphics.Web.Processing.Core.Monad
import Graphics.Web.Processing.Core.Types
import Graphics.Web.Processing.Core.Interface
-- variables
import Graphics.Web.Processing.Core.Var
-- optimization
import Graphics.Web.Processing.Optimize
-- transformers
import Control.Monad (void)
import Control.Applicative
import Control.Monad.Trans.State.Strict
-- monoids
import Data.Monoid
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
   n0 <- liftProc getVarNumber
   let s1 = execState e1 $ emptyEventState { event_code = setVarNumber n0 }
       n1 = fst $ runProcMWith n0 $ event_code s1 >> getVarNumber
       s2 = execState e2 $ EventState { event_preamble = return () , event_code = setVarNumber n1 }
   addPCode $ event_preamble s1
   addPCode $ event_preamble s2
   addCode $ iff b (event_code s1) (event_code s2)
 -- Create variables in an event? That should never happen, really.
 createVarM = fail "EventM(createVarM): This error should never be called. Report this as a bug."
 createArrayVarM = fail "EventM(createArrayVarM): This error should never be called. Report this as a bug."
 writeVar v x = liftProc $ writeVar v x
 readVar v = do
  x <- liftProc $ readVar v
  addPCode $ void $ liftProc $ newVar x
  n <- switchContextE $ liftProc getVarNumber
  let v' = fst $ runProcMWith n $ liftProc $ newVar x
  liftProc $ setVarNumber $ n + 1
  writeVar v' x
  liftProc $ readVar v'
 -- New variable in an event? That should not happen, really.
 newVar = fail "EventM(newVar): This error should never be called. Report this as an issue."
 newArrayVar = fail "EventM(newArrayVar): This error should never be called. Report this as an issue."

data ScriptState c =
  ScriptState
    { script_code :: ProcM c () -- This code should actually be the preamble
    , script_setup :: Maybe (ProcM Setup ())
    , script_draw :: Maybe (ProcM Draw ())
    , script_mouseClicked :: Maybe (ProcM MouseClicked ())
    , script_mouseReleased :: Maybe (ProcM MouseReleased ())
    , script_keyPressed :: Maybe (ProcM KeyPressed ())
      }

emptyScriptState :: ScriptState c
emptyScriptState = ScriptState (return ()) Nothing Nothing Nothing Nothing Nothing

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

-- | Events created inside a conditional will be automatically deleted.
--   They must be /unconditional/.
instance ProcMonad ScriptM where
 liftProc p = ScriptM $ do
   ss <- get
   let c = script_code ss >> p
   put $ ss { script_code = void c }
   return $ fst $ runProcM c
 commandM t as = liftProc $ commandM t as
 assignM = liftProc . assignM
 createVarM = liftProc . createVarM
 createArrayVarM n xs = liftProc $ createArrayVarM n xs
 writeComment = liftProc . writeComment
 iff b (ScriptM e1) (ScriptM e2) = do
   c0 <- script_code <$> ScriptM get
   let n  = fst $ runProcM $ c0 >> getVarNumber
       s1 = execState e1 $ emptyScriptState { script_code = setVarNumber n  }
       c1 = script_code s1
       n1 = fst $ runProcM $ c1 >> getVarNumber
       s2 = execState e2 $ emptyScriptState { script_code = setVarNumber n1 }
       c2 = script_code s2
       n2 = fst $ runProcM $ c2 >> getVarNumber
   liftProc $ setVarNumber n2
   liftProc $ iff b c1 c2
 newVar = liftProc . newVar
 newArrayVar = liftProc . newArrayVar
 writeVar v x = liftProc $ writeVar v x
 readVar v = do
   x  <- liftProc $ readVar v
   v' <- switchContext $ newVar x
   liftProc $ readVar v'

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

instance Context MouseReleased where
 addEvent _ c s = s { script_mouseReleased = Just c }

instance Context KeyPressed where
 addEvent _ c s = s { script_keyPressed = Just c }

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

-- | Like 'execScriptM', but skips optimizations.
execScriptMFast :: ScriptM Preamble () -> ProcScript
execScriptMFast (ScriptM s0) =
  let s = execState s0 emptyScriptState
  in  ProcScript
    { proc_preamble = execProcM $ script_code s
    , proc_setup = maybe mempty execProcM $ script_setup s
    , proc_draw = fmap execProcM $ script_draw s
    , proc_mouseClicked = fmap execProcM $ script_mouseClicked s
    , proc_mouseReleased = fmap execProcM $ script_mouseReleased s
    , proc_keyPressed = fmap execProcM $ script_keyPressed s
      }

-- | Execute the scripter monad to get the full Processing script.
--   Use 'renderScript' or 'renderFile' to render it.
--
--   After generating the script, the output code is optimized
--   using 'optimizeBySubstitution'.
execScriptM :: ScriptM Preamble () -> ProcScript
execScriptM = optimizeBySubstitution . execScriptMFast

-- Coercions

-- | Magic! Keep it private, it's our secret!
switchContext :: ScriptM c a -> ScriptM d a
switchContext = unsafeCoerce

-- | Magic! Keep it private, it's our secret!
switchContextE :: EventM c a -> EventM d a
switchContextE = unsafeCoerce

