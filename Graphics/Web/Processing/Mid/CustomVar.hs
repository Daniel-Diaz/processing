
{-# LANGUAGE DeriveGeneric, TypeOperators, DefaultSignatures, FlexibleContexts,
             TemplateHaskell
  #-}

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
--   There are also arrays which may contain custom values.
--   See 'CustomArrayVar'.
--
--   The dependency of this module in several language extensions was
--   the reason to make it separate from the rest of the /mid/ interface
--   where it belongs to. Somehow, it forces the user to use @DeriveGeneric@
--   and import "GHC.Generics" to do something useful with it (more than use
--   custom variables for tuples).
module Graphics.Web.Processing.Mid.CustomVar (
    CustomVar
  , CustomArrayVar
  , customArraySize
  , VarLength (..)
  , CustomValue (..)
  , readArrayVarC
  , writeArrayVarC
    ) where

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Text.PrettyPrint.Mainland (ppr, prettyLazyText)
import Data.Monoid ((<>))
import Graphics.Web.Processing.Mid
import Graphics.Web.Processing.Core.Primal (varFromText,Proc_Int (..))
import Control.Monad (liftM)
-- generics
import GHC.Generics
import Graphics.Web.Processing.Core.TH

{-

This module is somehow magic. It allows you handle set of variables
and arrays like if they were a single variable. It is very convenient
to handle other values rather than the primitive Proc_* types.

The interesting part is that you can make, with some restrictions,
your own type be stored in one of these variables.

-}

-- | Variable with custom values.
data CustomVar a = CustomVar [Text] deriving Generic

-- | Modify all the variable names inside a custom variable.
mapCustomVar :: (Text -> Text) -> CustomVar a -> CustomVar a
mapCustomVar f (CustomVar xs) = CustomVar (fmap f xs)

-- | Typeclass of custom values, which can be stored in custom variables ('CustomVar').
class VarLength a => CustomValue a where
 -- | Version of 'newVar' for custom variables.
 newVarC :: (Monad (m Preamble), ProcMonad m) => a -> m Preamble (CustomVar a)
 default newVarC :: (Monad (m Preamble), ProcMonad m, Generic a, GCustomValue (Rep a))
                 => a -> m Preamble (CustomVar a)
 newVarC = liftM castCVar . gnewVarC . from
 -- | Version of 'newArrayVar' for custom variables.
 newArrayVarC :: (Monad (m Preamble), ProcMonad m) => [a] -> m Preamble (CustomArrayVar a)
 default newArrayVarC :: (Monad (m Preamble), ProcMonad m, Generic a, GCustomValue (Rep a))
                      => [a] -> m Preamble (CustomArrayVar a)
 newArrayVarC = liftM castCAVar . gnewArrayVarC . fmap from
 -- | Version of 'readVar' for custom variables.
 readVarC :: (Monad (m c), ProcMonad m) => CustomVar a -> m c a
 default readVarC :: (Monad (m c), ProcMonad m, Generic a, GCustomValue (Rep a))
                  => CustomVar a -> m c a
 readVarC v = liftM to $ greadVarC (castCVar v)
 -- | Version of 'writeVar' for custom variables.
 writeVarC :: (Monad (m c), ProcMonad m) => CustomVar a -> a -> m c ()
 default writeVarC :: (Monad (m c), ProcMonad m, Generic a, GCustomValue (Rep a)) => CustomVar a -> a -> m c ()
 writeVarC v x = gwriteVarC (castCVar v) (from x)
 -- | Version of 'if_' for custom values.
 ifC :: Proc_Bool -> a -> a -> a
 default ifC :: (Generic a, GCustomValue (Rep a)) => Proc_Bool -> a -> a -> a
 ifC b x y = to $ gifC b (from x) (from y)

-- Maybe this function can be written in terms of arrayVarToVar?
arrayVarToVarC :: CustomArrayVar a -> Proc_Int -> CustomVar a
arrayVarToVarC v n = mapCustomVar f $ customInnerVar v
  where
   f t = t <> "[" <> (toStrict $ prettyLazyText 80 $ ppr n) <> "]"

-- | Read a component of a custom array variable.
readArrayVarC :: (ProcMonad m, Monad (m c), CustomValue a)
              => CustomArrayVar a -> Proc_Int -> m c a
readArrayVarC v n =
  case n of
    Proc_Int i -> let s = customArraySize v
                  in  if (i < 0) || (i >= s)
                         then fail $ "readArrayVarC: index out of bounds.\nArray size: "
                                  ++ show s
                                  ++ ".\nIndex given: "
                                  ++ show i
                                  ++ ".\nRemember that indices start from 0."
                         else readVarC $ arrayVarToVarC v n
    _ -> readVarC $ arrayVarToVarC v n

-- | Write a component of a custom array variable.
writeArrayVarC :: (ProcMonad m, Monad (m c), CustomValue a)
               => CustomArrayVar a -> Proc_Int -> a -> m c ()
writeArrayVarC v n x = writeVarC (arrayVarToVarC v n) x

fromVar :: Var a -> CustomVar a
fromVar = CustomVar . (:[]) . varName

fromCustomVar :: CustomVar a -> [Var a]
fromCustomVar (CustomVar xs) = fmap varFromText xs

-- Custom arrays

-- | Array variable of custom values.
data CustomArrayVar a =
  CustomArrayVar { -- | Size of the custom array.
                   customArraySize :: Int
                 , customInnerVar :: CustomVar a
                   }

fromArrayVar :: ArrayVar a -> CustomArrayVar a
fromArrayVar v =
  CustomArrayVar (arraySize v) $ fromVar $ varFromText $ arrayVarName v

-- | Typeclass of values that can be stored in several
--   native variables ('Var').
class VarLength a where
 -- | Calculate how many native variables are needed
 --   to store a value.
 varLength :: a -> Int
 default varLength :: (Generic a, GVarLength (Rep a)) => a -> Int
 varLength = gvarLength . from

-- GENERICS

class GVarLength f where
 gvarLength :: f a -> Int

instance GVarLength U1 where
 gvarLength _ = 1

instance (GVarLength a, GVarLength b) => GVarLength (a :*: b) where
 gvarLength (a :*: b) = gvarLength a  + gvarLength b

instance GVarLength (a :+: b) where
 gvarLength _ = error "gvarLength: Custom variables cannot contain sum types."

instance GVarLength a => GVarLength (M1 i c a) where
 gvarLength (M1 x) = gvarLength x

instance VarLength a => GVarLength (K1 i a) where
 gvarLength (K1 x) = varLength x

varDrop :: Int -> CustomVar a -> CustomVar a
varDrop n (CustomVar xs) = CustomVar $ drop n xs

castCVar :: CustomVar a -> CustomVar b
castCVar (CustomVar xs) = CustomVar xs

castCAVar :: CustomArrayVar a -> CustomArrayVar b
castCAVar (CustomArrayVar n v) = CustomArrayVar n $ castCVar v

class GCustomValue f where
 gnewVarC :: (Monad (m Preamble), ProcMonad m) => f a -> m Preamble (CustomVar (f a))
 gnewArrayVarC :: (Monad (m Preamble), ProcMonad m) => [f a] -> m Preamble (CustomArrayVar (f a))
 greadVarC :: (Monad (m c), ProcMonad m) => CustomVar (f a) -> m c (f a)
 gwriteVarC :: (Monad (m c), ProcMonad m) => CustomVar (f a) -> f a -> m c ()
 gifC :: Proc_Bool -> f a -> f a -> f a

leftP :: (a :*: b) c -> a c
leftP (a :*: _) = a

rightP :: (a :*: b) c -> b c
rightP (_ :*: b) = b

instance (GVarLength a, GCustomValue a, GCustomValue b) => GCustomValue (a :*: b) where
 gnewVarC (a :*: b) = do
   CustomVar xs <- gnewVarC a
   CustomVar ys <- gnewVarC b
   return $ CustomVar $ xs ++ ys
 gnewArrayVarC l = do
   let as = fmap leftP  l
       bs = fmap rightP l
   CustomArrayVar n (CustomVar xs) <- gnewArrayVarC as
   CustomArrayVar _ (CustomVar ys) <- gnewArrayVarC bs
   return $ CustomArrayVar n $ CustomVar $ xs ++ ys
 greadVarC v = do
   a <- greadVarC $ castCVar v
   let n = gvarLength a
   b <- greadVarC $ varDrop n $ castCVar v
   return $ a :*: b
 gwriteVarC (CustomVar v) (a :*: b) = do
   let (xs,ys) = splitAt (gvarLength a) v
   gwriteVarC (CustomVar xs) a
   gwriteVarC (CustomVar ys) b
 gifC c (a :*: b) (x :*: y) = gifC c a x :*: gifC c b y

instance GCustomValue (a :+: b) where
 gnewVarC      = error      "gnewVarC: Custom variables cannot contain sum types."
 gnewArrayVarC = error "gnewArrayVarC: Custom variables cannot contain sum types."
 greadVarC     = error     "greadVarC: Custom variables cannot contain sum types."
 gwriteVarC    = error    "gwriteVarC: Custom variables cannot contain sum types."
 gifC          = error          "gifC: Custom values are not sum types."

instance GCustomValue a => GCustomValue (M1 i c a) where
 gnewVarC (M1 x) = liftM castCVar $ gnewVarC x
 gnewArrayVarC = liftM castCAVar . gnewArrayVarC . fmap unM1
 greadVarC v = liftM M1 $ greadVarC $ castCVar v
 gwriteVarC v (M1 x) = gwriteVarC (castCVar v) x
 gifC b (M1 x) (M1 y) = M1 $ gifC b x y

instance CustomValue a => GCustomValue (K1 i a) where
 gnewVarC (K1 x) = liftM castCVar $ newVarC x
 gnewArrayVarC = liftM castCAVar . newArrayVarC . fmap unK1
 greadVarC v = liftM K1 $ readVarC $ castCVar v
 gwriteVarC v (K1 x) = writeVarC (castCVar v) x
 gifC b (K1 x) (K1 y) = K1 $ ifC b x y

{- Proc_* types as custom values

Any Proc_* type can be seen as a custom value,
making a trivial instance to the CustomValue class,
using custom variables as usual variables.

For any Proc_* type:

instance VarLength Proc_* where
  varLength _ = 1

instance CustomValue Proc_* where
  newVarC = liftM fromVar . newVar
  newArrayVarC = liftM fromArrayVar . newArrayVar
  readVarC = readVar . head . fromCustomVar
  writeVarC v x = writeVar (head $ fromCustomVar v) x
  ifC = if_

-}

$(deriveCustomValues)

-- Instances for other types.

instance (VarLength a, VarLength b) => VarLength (a,b)
instance (CustomValue a, CustomValue b) => CustomValue (a,b)

instance (VarLength a, VarLength b, VarLength c) => VarLength (a,b,c)
instance (CustomValue a, CustomValue b, CustomValue c) => CustomValue (a,b,c)
