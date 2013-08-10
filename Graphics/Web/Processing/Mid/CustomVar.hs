
{-# LANGUAGE DeriveGeneric, TypeOperators, DefaultSignatures, FlexibleContexts #-}

-- | This module implements variables which may contain values from
--   types different from the native types (@Proc_*@ types).
--
--   To make a type available to custom variables, it needs to be
--   instantiated in the 'CustomValue' class, which is subclass
--   of the 'VarLength' class. These instances are derivables using
--   the @DeriveGeneric@ extension. Things you need are: enable the
--   @DeriveGeneric@ language extension, import "GHC.Generics", derive
--   a 'Generic' instance of your type and then write the following
--   instances (where @Foo@ is any type of interest):
--
-- > instance VarLength Foo
-- > instance CustomValue Foo
--
--   Note that @Foo@ must be made from other types that are instances
--   of 'CustomValue'. Also, note that instances of 'VarLength' or
--   'CustomValue' can /not/ be recursive or sum types.
--   An example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Graphics.Web.Processing.Mid
-- > import Graphics.Web.Processing.Mid.CustomVar
-- > import GHC.Generics
-- >
-- > data Point = Point Proc_Float Proc_Float
-- >                deriving Generic
-- >
-- > instance VarLength Point
-- > instance CustomValue Point
--
--   Types instance of the 'CustomValue' class can be contained by
--   a special type of variables, called 'CustomVar' (Custom Variable).
--   Functions for custom variables are equal to the function for regular
--   variables, except that they all end in @C@. For example, 'newVar' is
--   called 'newVarC' for custom variables.
--
--   The dependency of this module in several language extensions was
--   the reason to make it separate from the rest of the /mid/ interface
--   where it belongs to. Somehow, it forces the user to use @DeriveGeneric@
--   and import "GHC.Generics" to do something useful with it (more than use
--   custom variables for tuples).
module Graphics.Web.Processing.Mid.CustomVar (
    CustomVar
  , VarLength (..)
  , CustomValue (..)
    ) where

import Data.Text (Text)
import Graphics.Web.Processing.Mid
import Graphics.Web.Processing.Core.Primal (varFromText)
import Control.Monad (liftM)
-- generics
import GHC.Generics

-- | Variable with custom values.
data CustomVar a = CustomVar [Text] deriving Generic

-- | Typeclass of custom values, which can be stored in custom variables ('CustomVar').
class VarLength a => CustomValue a where
 -- | Version of 'newVar' for custom variables.
 newVarC :: (Monad (m Preamble), ProcMonad m) => a -> m Preamble (CustomVar a)
 default newVarC :: (Monad (m Preamble), ProcMonad m, Generic a, GCustomValue (Rep a))
                 => a -> m Preamble (CustomVar a)
 newVarC x = liftM castCVar $ gnewVarC (from x)
 -- | Version of 'readVar' for custom variables.
 readVarC :: (Monad (m c), ProcMonad m) => CustomVar a -> m c a
 default readVarC :: (Monad (m c), ProcMonad m, Generic a, GCustomValue (Rep a))
                  => CustomVar a -> m c a
 readVarC v = liftM to $ greadVarC (castCVar v)
 -- | Version of 'writeVar' for custom variables.
 writeVarC :: (Monad (m c), ProcMonad m) => CustomVar a -> a -> m c ()
 default writeVarC :: (Monad (m c), ProcMonad m, Generic a, GCustomValue (Rep a)) => CustomVar a -> a -> m c ()
 writeVarC v x = gwriteVarC (castCVar v) (from x)

fromVar :: Var a -> CustomVar a
fromVar = CustomVar . (:[]) . varName

fromCustomVar :: CustomVar a -> [Var a]
fromCustomVar (CustomVar xs) = fmap varFromText xs

-- This instances are really boring (they are all equal).
-- Candidate for Template Haskell.

instance CustomValue Proc_Bool where
 newVarC = liftM fromVar . newVar
 readVarC = readVar . head . fromCustomVar
 writeVarC v x = writeVar (head $ fromCustomVar v) x

instance CustomValue Proc_Int where
 newVarC = liftM fromVar . newVar
 readVarC = readVar . head . fromCustomVar
 writeVarC v x = writeVar (head $ fromCustomVar v) x

instance CustomValue Proc_Float where
 newVarC = liftM fromVar . newVar
 readVarC = readVar . head . fromCustomVar
 writeVarC v x = writeVar (head $ fromCustomVar v) x

instance CustomValue Proc_Text where
 newVarC = liftM fromVar . newVar
 readVarC = readVar . head . fromCustomVar
 writeVarC v x = writeVar (head $ fromCustomVar v) x

instance CustomValue Proc_Image where
 newVarC = liftM fromVar . newVar
 readVarC = readVar . head . fromCustomVar
 writeVarC v x = writeVar (head $ fromCustomVar v) x

instance CustomValue Proc_Char where
 newVarC = liftM fromVar . newVar
 readVarC = readVar . head . fromCustomVar
 writeVarC v x = writeVar (head $ fromCustomVar v) x

-- | Typeclass of values that can be stored in several
--   native variables ('Var').
class VarLength a where
 -- | Calculate how many native variables are needed
 --   to store a value.
 varLength :: a -> Int
 default varLength :: (Generic a, GVarLength (Rep a)) => a -> Int
 varLength = gvarLength . from

instance VarLength Proc_Bool where
 varLength _ = 1
instance VarLength Proc_Int where
 varLength _ = 1
instance VarLength Proc_Float where
 varLength _ = 1
instance VarLength Proc_Text where
 varLength _ = 1
instance VarLength Proc_Image where
 varLength _ = 1
instance VarLength Proc_Char where
 varLength _ = 1

-- GENERICS

class GVarLength f where
 gvarLength :: f a -> Int

instance GVarLength U1 where
 gvarLength _ = 1

instance (GVarLength a, GVarLength b) => GVarLength (a :*: b) where
 gvarLength (a :*: b) = gvarLength a + gvarLength b

instance GVarLength (a :+: b) where
 gvarLength _ = error "gvarLength: Custom variables cannot be sum types."

instance GVarLength a => GVarLength (M1 i c a) where
 gvarLength (M1 x) = gvarLength x

instance VarLength a => GVarLength (K1 i a) where
 gvarLength (K1 x) = varLength x

varDrop :: Int -> CustomVar a -> CustomVar a
varDrop n (CustomVar xs) = CustomVar $ drop n xs

castCVar :: CustomVar a -> CustomVar b
castCVar (CustomVar xs) = CustomVar xs

class GCustomValue f where
 gnewVarC :: (Monad (m Preamble), ProcMonad m) => f a -> m Preamble (CustomVar (f a))
 greadVarC :: (Monad (m c), ProcMonad m) => CustomVar (f a) -> m c (f a)
 gwriteVarC :: (Monad (m c), ProcMonad m) => CustomVar (f a) -> f a -> m c ()

instance (GVarLength a, GCustomValue a, GCustomValue b) => GCustomValue (a :*: b) where
 gnewVarC (a :*: b) = do
   CustomVar xs <- gnewVarC a
   CustomVar ys <- gnewVarC b
   return $ CustomVar $ xs ++ ys
 greadVarC v = do
   a <- greadVarC $ castCVar v
   let n = gvarLength a
   b <- greadVarC $ varDrop n $ castCVar v
   return $ a :*: b
 gwriteVarC (CustomVar v) (a :*: b) = do
   let (xs,ys) = splitAt (gvarLength a) v
   gwriteVarC (CustomVar xs) a
   gwriteVarC (CustomVar ys) b

instance GCustomValue (a :+: b) where
 gnewVarC   = error   "gnewVarC: Custom variables cannot be sum types."
 greadVarC  = error  "greadVarC: Custom variables cannot be sum types."
 gwriteVarC = error "gwriteVarC: Custom variables cannot be sum types."

instance GCustomValue a => GCustomValue (M1 i c a) where
 gnewVarC (M1 x) = liftM castCVar $ gnewVarC x
 greadVarC v = liftM M1 $ greadVarC $ castCVar v
 gwriteVarC v (M1 x) = gwriteVarC (castCVar v) x

instance CustomValue a => GCustomValue (K1 i a) where
 gnewVarC (K1 x) = liftM castCVar $ newVarC x
 greadVarC v = liftM K1 $ readVarC $ castCVar v
 gwriteVarC v (K1 x) = writeVarC (castCVar v) x

instance (VarLength a, VarLength b) => VarLength (a,b)
instance (CustomValue a, CustomValue b) => CustomValue (a,b)

instance (VarLength a, VarLength b, VarLength c) => VarLength (a,b,c)
instance (CustomValue a, CustomValue b, CustomValue c) => CustomValue (a,b,c)
