
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- | Internal module. Mostly for type definitions
--   and class instances.
module Graphics.Web.Processing.Core.Primal (
  -- * Types
  -- ** Singleton types
  -- | A list of singleton types, used to restrict the
  --   use of certain commands to specific contexts.
    Preamble (..), Setup (..), Draw (..)
  , MouseClicked (..)
  -- ** Processing types
  -- *** Boolean
  , Proc_Bool, fromBool
  , true, false
  , pnot, (#||), (#&&)
  -- *** Int
  , Proc_Int, fromInt, evalInt
  , pfloor
  -- *** Float
  , Proc_Float (..), fromFloat
  , intToFloat
  , noisef
  -- *** Image
  , Proc_Image
  -- *** Char
  , Proc_Char , fromChar
  -- *** Text
  , Proc_Text, fromStText
  -- ** Type class of proc types
  , ProcType (..)
  -- ** Conditionals
  , Proc_Eq (..)
  , Proc_Ord (..)
  -- ** Reductions
  , Reducible (..)
  -- ** Variables
  , Var, varName, varFromText
  -- ** Script
  , ProcCode, ProcArg, ProcAsign
  , emptyCode
  , command , assignment, createVar, comment
  , conditional
  , (>>.)
  , ProcScript (..)
  , emptyScript
  ) where

{- Notes on this module

This module needs some work to be refactored, probably
in different submodules. Currently, it's getting too long
and is potentially going to grow even more. Also, it has
code for unrelated tasks: types, code reduction, variables,
events, etc. I think 'Reducible' deserves a module by itself.

-}

import Prelude hiding (foldr)
import Data.Text (Text,lines)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Foldable (foldMap,foldr)
import Control.Applicative (liftA2)
-- Pretty
import Text.PrettyPrint.Mainland
-- Hashable & Generics
import Data.Hashable
import GHC.Generics (Generic)

-- | The /preamble/ is the code that is executed
--   at the beginning of the script.
data Preamble = Preamble

-- | In the /setup/ part, settings like /size/ or
--   /frame rate/ are supplied.
data Setup = Setup

-- | The drawing loop.
data Draw = Draw

-- | Code that is executed when the mouse is clicked.
data MouseClicked = MouseClicked

-- PRETTY-HELPERS

pfunction :: Text -> [Doc] -> Doc
pfunction n as = fromText n <> parens (commasep as)

-- TYPES

-- | Boolean values.
data Proc_Bool =
   Proc_True
 | Proc_False
   -- Operations
 | Proc_Neg Proc_Bool
 | Proc_Or Proc_Bool Proc_Bool
 | Proc_And Proc_Bool Proc_Bool
   -- Variables
 | Bool_Var Text
   -- Comparisons
     -- Bool
   | Bool_Eq Proc_Bool Proc_Bool
   | Bool_NEq Proc_Bool Proc_Bool
     -- Int (with order)
   | Int_Eq Proc_Int Proc_Int
   | Int_NEq Proc_Int Proc_Int
   | Int_LE Proc_Int Proc_Int
   | Int_L Proc_Int Proc_Int
   | Int_GE Proc_Int Proc_Int
   | Int_G Proc_Int Proc_Int
     -- Float (with order)
   | Float_Eq Proc_Float Proc_Float
   | Float_NEq Proc_Float Proc_Float
   | Float_LE Proc_Float Proc_Float
   | Float_L Proc_Float Proc_Float
   | Float_GE Proc_Float Proc_Float
   | Float_G Proc_Float Proc_Float
     -- Char
   | Char_Eq Proc_Char Proc_Char
   | Char_NEq Proc_Char Proc_Char
     -- Text
   | Text_Eq Proc_Text Proc_Text

-- Doc helpers

docEq :: Doc
docEq = fromText "=="

docNEq :: Doc
docNEq = fromText "!="

docLE :: Doc
docLE = fromText "<="

docL :: Doc
docL = fromText "<"

docGE :: Doc
docGE = fromText ">="

docG :: Doc
docG = fromText ">"

indentLevel :: Int
indentLevel = 3

instance Pretty Proc_Bool where
 ppr Proc_True = fromText "true"
 ppr Proc_False = fromText "false"
 ppr (Proc_Neg b) = fromText "!" <> ppr b
 ppr (Proc_Or b1 b2)  = parens $ ppr b1 <+> fromText "||" <+> ppr b2
 ppr (Proc_And b1 b2) = parens $ ppr b1 <+> fromText "&&" <+> ppr b2
 ppr (Bool_Var t) = fromText t
 -- Comparisons
 ppr (Bool_Eq x y) = parens $ ppr x <+> docEq <+> ppr y
 ppr (Bool_NEq x y) = parens $ ppr x <+> docNEq <+> ppr y
 ppr (Int_Eq x y) = parens $ ppr x <+> docEq <+> ppr y
 ppr (Int_NEq x y) = parens $ ppr x <+> docNEq <+> ppr y
 ppr (Int_LE x y) = parens $ ppr x <+> docLE <+> ppr y
 ppr (Int_L x y) = parens $ ppr x <+> docL <+> ppr y
 ppr (Int_GE x y) = parens $ ppr x <+> docGE <+> ppr y
 ppr (Int_G x y) = parens $ ppr x <+> docG <+> ppr y
 ppr (Float_Eq x y) = parens $ ppr x <+> docEq <+> ppr y
 ppr (Float_NEq x y) = parens $ ppr x <+> docNEq <+> ppr y
 ppr (Float_LE x y) = parens $ ppr x <+> docLE <+> ppr y
 ppr (Float_L x y) = parens $ ppr x <+> docL <+> ppr y
 ppr (Float_GE x y) = parens $ ppr x <+> docGE <+> ppr y
 ppr (Float_G x y) = parens $ ppr x <+> docG <+> ppr y
 ppr (Char_Eq x y) = parens $ ppr x <+> docEq <+> ppr y
 ppr (Char_NEq x y) = parens $ ppr x <+> docNEq <+> ppr y
 ppr (Text_Eq x y) = ppr x <> fromText "." <> pfunction "equals" [ppr y]

-- | Value of 'True'.
true :: Proc_Bool
true = Proc_True

-- | Value of 'False'.
false :: Proc_Bool
false = Proc_False

-- | Negation.
pnot :: Proc_Bool -> Proc_Bool
pnot = Proc_Neg

infixr 2 #||

-- | Disjunction.
(#||) :: Proc_Bool -> Proc_Bool -> Proc_Bool
(#||) = Proc_Or

infixr 3 #&&

-- | Conjunction.
(#&&) :: Proc_Bool -> Proc_Bool -> Proc_Bool
(#&&) = Proc_And

-- | Cast a 'Bool' value.
fromBool :: Bool -> Proc_Bool
fromBool True  = Proc_True
fromBool False = Proc_False

-- | Integer numbers.
data Proc_Int =
   Proc_Int Int
   -- Operations
 | Int_Sum Proc_Int Proc_Int
 | Int_Substract Proc_Int Proc_Int
 | Int_Divide Proc_Int Proc_Int
 | Int_Mult Proc_Int Proc_Int
 | Int_Mod Proc_Int Proc_Int
   -- Variables
 | Int_Var Text
   -- Functions
 | Int_Abs Proc_Int
 | Int_Floor Proc_Float
   deriving Eq

instance Pretty Proc_Int where
 ppr (Proc_Int i) = ppr i
 ppr (Int_Sum n m) = parens $ ppr n <> fromText "+" <> ppr m
 ppr (Int_Substract n m) = parens $ ppr n <> fromText "-" <> ppr m
 ppr (Int_Divide n m) = parens $ ppr n <> fromText "/" <> ppr m
 ppr (Int_Mult n m) = parens $ ppr n <> fromText "*" <> ppr m
 ppr (Int_Mod n m) = parens $ ppr n <> fromText "%" <> ppr m
 ppr (Int_Var t) = fromText t
 ppr (Int_Abs n) = pfunction "abs" [ppr n]
 ppr (Int_Floor x) = pfunction "floor" [ppr x]

-- | Cast an 'Int' value.
fromInt :: Int -> Proc_Int
fromInt = Proc_Int

-- | Try to evaluate an integer.
evalInt :: Proc_Int -> Maybe Int
evalInt (Proc_Int i) = Just i
evalInt (Int_Sum n m) = liftA2 (+) (evalInt n) (evalInt m)
evalInt (Int_Substract n m) = liftA2 (-) (evalInt n) (evalInt m)
evalInt (Int_Divide n m) = liftA2 div (evalInt n) (evalInt m)
evalInt (Int_Mult n m) = liftA2 (*) (evalInt n) (evalInt m)
evalInt (Int_Mod n m) = liftA2 mod (evalInt n) (evalInt m)
evalInt (Int_Abs n) = fmap abs $ evalInt n
-- Variables cannot be evaluated.
evalInt _ = Nothing

-- | Calculates the 'floor' of a 'Proc_Float'.
pfloor :: Proc_Float -> Proc_Int
pfloor = Int_Floor

instance Ord Proc_Int where
 n <= m = case liftA2 (<=) (evalInt n) (evalInt m) of
   Nothing -> error "Proc_Int: (<=) applied to a variable."
   Just b -> b

instance Enum Proc_Int where
 toEnum = fromInt
 fromEnum n = case evalInt n of
  Nothing -> error "Proc_Int: fromEnum applied to a variable."
  Just i -> i
 succ n = n + 1
 pred n = n - 1
 
-- | WARNING: 'signum' method is undefined.
instance Num Proc_Int where
 fromInteger = fromInt . fromInteger
 (+) = Int_Sum
 (-) = Int_Substract
 (*) = Int_Mult
 abs = Int_Abs
 signum = error "Proc_Int: signum method is undefined."

instance Real Proc_Int where
 toRational n = case evalInt n of
   Nothing -> error "Proc_Int: toRational applied to a variable."
   Just i -> toRational i

instance Integral Proc_Int where
 div = Int_Divide
 mod = Int_Mod
 divMod n d = (div n d, mod n d)
 toInteger n = case evalInt n of
   Nothing -> error "Proc_Int: toInteger applied to a variable."
   Just i  -> toInteger i

-- | Floating point numbers.
--   The provided 'Eq' instance checks the equality of the
--   internal expression, not the value.
data Proc_Float =
   Proc_Float Float
   -- Operations
 | Float_Sum Proc_Float Proc_Float
 | Float_Substract Proc_Float Proc_Float
 | Float_Divide Proc_Float Proc_Float
 | Float_Mult Proc_Float Proc_Float
 | Float_Mod Proc_Float Proc_Float
   -- Variables
 | Float_Var Text
   -- Constants
 | Float_Pi
   -- Functions
 | Float_Abs Proc_Float
 | Float_Exp Proc_Float
 | Float_Sqrt Proc_Float
 | Float_Log Proc_Float
 | Float_Sine Proc_Float
 | Float_Cosine Proc_Float
 | Float_Arcsine Proc_Float
 | Float_Arccosine Proc_Float
 | Float_Arctangent Proc_Float
 | Float_Floor Proc_Float -- Applies floor but it treats the result
                          -- as a float. Only internal.
 | Float_Noise Proc_Float Proc_Float
   deriving (Eq,Generic)

instance Hashable Proc_Float

instance Pretty Proc_Float where 
 ppr (Proc_Float f) = ppr f
 ppr (Float_Sum x y) = parens $ ppr x <> fromText "+" <> ppr y
 ppr (Float_Substract x y) = parens $ ppr x <> fromText "-" <> ppr y
 ppr (Float_Divide x y) = parens $ ppr x <> fromText "/" <> ppr y
 ppr (Float_Mult x y) = parens $ ppr x <> fromText "*" <> ppr y
 ppr (Float_Mod x y) = parens $ ppr x <> fromText "%" <> ppr y
 ppr (Float_Var t) = fromText t
 ppr Float_Pi = fromText "PI"
 ppr (Float_Abs x) = pfunction "abs" [ppr x]
 ppr (Float_Exp x) = pfunction "exp" [ppr x]
 ppr (Float_Sqrt x) = pfunction "sqrt" [ppr x]
 ppr (Float_Log x) = pfunction "log" [ppr x]
 ppr (Float_Sine x) = pfunction "sin" [ppr x]
 ppr (Float_Cosine x) = pfunction "cos" [ppr x]
 ppr (Float_Arcsine x) = pfunction "asin" [ppr x]
 ppr (Float_Arccosine x) = pfunction "acos" [ppr x]
 ppr (Float_Arctangent x) = pfunction "atan" [ppr x]
 ppr (Float_Floor x) = pfunction "floor" [ppr x]
 ppr (Float_Noise x y) = pfunction "noise" [ppr x,ppr y]

-- | Cast a 'Float' value.
fromFloat :: Float -> Proc_Float
fromFloat = Proc_Float

-- | Specialized form of 'floor'.
floorI :: Float -> Int
floorI = floor

-- | Noise random function.
noisef :: Proc_Float -> Proc_Float -> Proc_Float
noisef = Float_Noise

evalFloat :: Proc_Float -> Maybe Float
evalFloat (Proc_Float f) = Just f
evalFloat (Float_Sum x y) = liftA2 (+) (evalFloat x) (evalFloat y)
evalFloat (Float_Substract x y) = liftA2 (-) (evalFloat x) (evalFloat y)
evalFloat (Float_Divide x y) = liftA2 (/) (evalFloat x) (evalFloat y)
evalFloat (Float_Mult x y) = liftA2 (*) (evalFloat x) (evalFloat y)
evalFloat Float_Pi = Just pi
evalFloat (Float_Abs x) = fmap abs $ evalFloat x
evalFloat (Float_Exp x) = fmap exp $ evalFloat x
evalFloat (Float_Sqrt x) = fmap sqrt $ evalFloat x
evalFloat (Float_Log x) = fmap log $ evalFloat x
evalFloat (Float_Sine x) = fmap sin $ evalFloat x
evalFloat (Float_Cosine x) = fmap cos $ evalFloat x
evalFloat (Float_Arcsine x) = fmap asin $ evalFloat x
evalFloat (Float_Arccosine x) = fmap acos $ evalFloat x
evalFloat (Float_Arctangent x) = fmap atan $ evalFloat x
evalFloat (Float_Floor x) = fmap (fromIntegral . floorI) $ evalFloat x
--
evalFloat _ = Nothing

-- | Cast a 'Proc_Int' to a 'Proc_Float'.
intToFloat :: Proc_Int -> Proc_Float
intToFloat (Proc_Int i) = Proc_Float $ fromIntegral i
intToFloat (Int_Sum n m) = Float_Sum (intToFloat n) (intToFloat m)
intToFloat (Int_Substract n m) = Float_Substract (intToFloat n) (intToFloat m)
intToFloat (Int_Divide n m) = Float_Divide (intToFloat n) (intToFloat m)
intToFloat (Int_Mult n m) = Float_Mult (intToFloat n) (intToFloat m)
intToFloat (Int_Mod n m) = Float_Mod (intToFloat n) (intToFloat m)
intToFloat (Int_Var t) = Float_Var t
intToFloat (Int_Abs n) = Float_Abs $ intToFloat n
intToFloat (Int_Floor x) = Float_Floor x

-- | WARNING: 'signum' method is undefined.
instance Num Proc_Float where
 fromInteger = fromFloat . fromInteger
 (+) = Float_Sum
 (-) = Float_Substract
 (*) = Float_Mult
 abs = Float_Abs
 signum = error "Proc_Float: signum method is undefined."

instance Fractional Proc_Float where
 (/) = Float_Divide
 fromRational = fromFloat . fromRational

-- | WARNING: 'sinh', 'cosh', 'asinh', 'acosh' and 'atanh'
--   methods are undefined.
instance Floating Proc_Float where
 pi = Float_Pi
 exp = Float_Exp
 sqrt = Float_Sqrt
 log = Float_Log
 sin = Float_Sine
 cos = Float_Cosine
 asin = Float_Arcsine
 acos = Float_Arccosine
 atan = Float_Arctangent
 -- UNDEFINED
 sinh = error "Proc_Float: sinh method is undefined."
 cosh = error "Proc_Float: cosh method is undefined."
 asinh = error "Proc_Float: asinh method is undefined."
 acosh = error "Proc_Float: acosh method is undefined."
 atanh = error "Proc_Float: atanh method is undefined."

-- | Type of images.
data Proc_Image = Image_Var Text

instance Pretty Proc_Image where
 ppr (Image_Var t) = fromText t

-- | Type of characters.
data Proc_Char =
   Proc_Char Char
 | Char_Var Text

-- | Cast a 'Char' value.
fromChar :: Char -> Proc_Char
fromChar = Proc_Char

instance Pretty Proc_Char where
 ppr (Proc_Char c) = enclose squote squote (char c)
 ppr (Char_Var n) = fromText n

-- | Type of textual values.
data Proc_Text =
   Proc_Text Text
 | Text_Var Text

instance Pretty Proc_Text where
 ppr (Proc_Text t) = enclose dquote dquote (fromText t)
 ppr (Text_Var n) = fromText n

-- | Cast a strict 'Text' value.
fromStText :: Text -> Proc_Text
fromStText = Proc_Text

-- CODE

-- | A piece of Processing code.
--   The type parameter indicates what the
--   context of the code is.
--   This context will allow or disallow
--   the use of certain commands.
data ProcCode c = 
   Command Text [ProcArg] 
 | CreateVar ProcAsign
 | Assignment ProcAsign
 | Conditional Proc_Bool   -- IF
              (ProcCode c) -- THEN
              (ProcCode c) -- ELSE
 | Comment Text
 | Sequence (Seq.Seq (ProcCode c))

instance Pretty (ProcCode c) where
 ppr (Command n as) = pfunction n (fmap ppr as) <+> fromText ";"
 ppr (CreateVar a) = ptype a <+> ppr a <+> fromText ";"
 ppr (Assignment a) = ppr a <+> fromText ";"
 ppr (Conditional b e1 e2) =
   let c1 = indent indentLevel $ ppr e1
       c2 = indent indentLevel $ ppr e2
   in  pfunction "if" [ppr b]
   <+> enclose lbrace rbrace (line <> c1)
   <+> fromText "else" <+> enclose lbrace rbrace (line <> c2)
 ppr (Comment t) = stack $ fmap (fromText . ("// " <>)) $ Data.Text.lines t
 ppr (Sequence sq) =
   if Seq.null sq then Text.PrettyPrint.Mainland.empty
                  else foldMap ((<> line) . ppr) sq

-- | Code for commands.
command :: Text -- ^ Name of the command.
        -> [ProcArg] -- ^ Arguments.
        -> ProcCode c -- ^ Code result.
command = Command

-- | Code for assignments.
assignment :: ProcAsign -- ^ Assignment.
           -> ProcCode c -- ^ Code result.
assignment = Assignment

-- | Code for variable creation.
createVar :: ProcAsign -- ^ Initial assignment.
          -> ProcCode c -- ^ Code result.
createVar = CreateVar

-- | Code for comments.
comment :: Text -> ProcCode c
comment = Comment

-- | Code for conditionals.
conditional :: Proc_Bool -> ProcCode c -> ProcCode c -> ProcCode c
conditional = Conditional

-- | Sequence to pieces of code with the same
--   context type. This way, code that belongs
--   to different parts of the program will
--   never get mixed.
(>>.) :: ProcCode c -> ProcCode c -> ProcCode c
(Sequence xs) >>. (Sequence ys) = Sequence $ xs Seq.>< ys
(Sequence xs) >>. p = Sequence $ xs Seq.|> p
p >>. (Sequence xs) = Sequence $ p Seq.<| xs
p >>. q = Sequence $ Seq.fromList [p,q]

-- | An empty piece of code.
emptyCode :: ProcCode a
emptyCode = Sequence $ Seq.empty

instance Monoid (ProcCode a) where
 mempty = emptyCode
 mappend = (>>.)

-- | A command argument.
data ProcArg =
   BoolArg  Proc_Bool
 | IntArg   Proc_Int
 | FloatArg Proc_Float
 | ImageArg Proc_Image
 | TextArg  Proc_Text
 | CharArg  Proc_Char

instance Pretty ProcArg where
 ppr (BoolArg  b) = ppr b
 ppr (IntArg   i) = ppr i
 ppr (FloatArg f) = ppr f
 ppr (ImageArg i) = ppr i
 ppr (TextArg  t) = ppr t
 ppr (CharArg  c) = ppr c

-- | Assigments.
data ProcAsign =
   BoolAsign  Text Proc_Bool
 | IntAsign   Text Proc_Int
 | FloatAsign Text Proc_Float
 | ImageAsign Text Proc_Image
 | TextAsign  Text Proc_Text
 | CharAsign  Text Proc_Char

instance Pretty ProcAsign where
 ppr (BoolAsign  n b) = fromText n <+> fromText "=" <+> ppr b
 ppr (IntAsign   n i) = fromText n <+> fromText "=" <+> ppr i
 ppr (FloatAsign n f) = fromText n <+> fromText "=" <+> ppr f
 ppr (ImageAsign n i) = fromText n <+> fromText "=" <+> ppr i
 ppr (TextAsign  n t) = fromText n <+> fromText "=" <+> ppr t
 ppr (CharAsign  n c) = fromText n <+> fromText "=" <+> ppr c

-- | Returns the name of the type (processing version) in an assignment.
ptype :: ProcAsign -> Doc
ptype (BoolAsign  _ _) = fromText "boolean"
ptype (IntAsign   _ _) = fromText "int"
ptype (FloatAsign _ _) = fromText "float"
ptype (ImageAsign _ _) = fromText "PImage"
ptype (TextAsign  _ _) = fromText "String"
ptype (CharAsign  _ _) = fromText "char"

---- ProcType class and instances

-- | Type of variables.
data Var a = Var { -- | Get the name of a variable.
                   varName :: Text }

-- | Internal function to create variables.
varFromText :: Text -> Var a
varFromText = Var

----- CLASSES

-- | Class of Processing value types.
class ProcType a where
 -- | Create a variable assignment, provided
 --   the name of the variable and the value to asign.
 proc_asign :: Text -> a -> ProcAsign
 -- | Create an argument for a command.
 proc_arg :: a -> ProcArg
 -- | Variable reading.
 proc_read :: Var a -> a

instance ProcType Proc_Bool where
 proc_asign = BoolAsign
 proc_arg = BoolArg
 proc_read (Var v) = Bool_Var v

instance ProcType Proc_Int where
 proc_asign = IntAsign
 proc_arg = IntArg
 proc_read (Var v) = Int_Var v

instance ProcType Proc_Float where
 proc_asign = FloatAsign
 proc_arg = FloatArg
 proc_read (Var v) = Float_Var v

instance ProcType Proc_Image where
 proc_asign = ImageAsign
 proc_arg = ImageArg
 proc_read (Var v) = Image_Var v

instance ProcType Proc_Char where
 proc_asign = CharAsign
 proc_arg = CharArg
 proc_read (Var v) = Char_Var v

instance ProcType Proc_Text where
 proc_asign =  TextAsign
 proc_arg = TextArg
 proc_read (Var v) = Text_Var v

infix 4 #==, #/=

-- | 'Eq' class for @Proc_*@ values.
class Proc_Eq a where
 (#==) :: a -> a -> Proc_Bool
 (#/=) :: a -> a -> Proc_Bool
 -- Minimal default instance: (#==) or (#/=).
 x #== y = pnot $ x #/= y
 x #/= y = pnot $ x #== y

instance Proc_Eq Proc_Bool where
 (#==) = Bool_Eq
 (#/=) = Bool_NEq

instance Proc_Eq Proc_Int where
 (#==) = Int_Eq
 (#/=) = Int_NEq

instance Proc_Eq Proc_Float where
 (#==) = Float_Eq
 (#/=) = Float_NEq

instance Proc_Eq Proc_Char where
 (#==) = Char_Eq
 (#/=) = Char_NEq

instance Proc_Eq Proc_Text where
 (#==) = Text_Eq

infix 4 #<=, #<, #>=, #>

-- | 'Ord' class for @Proc_*@ values.
class Proc_Ord a where
 (#<=) :: a -> a -> Proc_Bool
 (#<)  :: a -> a -> Proc_Bool
 (#>=) :: a -> a -> Proc_Bool
 (#>)  :: a -> a -> Proc_Bool

instance Proc_Ord Proc_Int where
 (#<=) = Int_LE
 (#<)  = Int_L
 (#>=) = Int_GE
 (#>)  = Int_G

instance Proc_Ord Proc_Float where
 (#<=) = Float_LE
 (#<)  = Float_L
 (#>=) = Float_GE
 (#>)  = Float_G

-- | Class of reducible types. Values of these
--   types contain expressions that can be
--   reducible.
class Reducible a where
 reduce :: a -> a

{-

FLOAT REDUCTION

Float is probably the most common argument type.
Below a case-by-case analysis try to reduce a
float expression to its minimal extension.
The more we reduce, the more effective
will be the Processing output code, since we save
operations.

Using the function 'evalFloat', we detect if
a expression can be reduced to a single 'Float'
value. If it is not possible, that means that it
contains either a variable or a function which
output we can't predict from the input (like 'noise').

Any work here is always valuable. I am even thinking
about creating a module only for this kind of stuff.
This module is getting too big.

The current reduction algorithm is really naïve and
without any guarantees. However, it is already saving
lot of operations.

-}

instance Reducible Proc_Float where
 reduce (Float_Sum x y) =
   if x == y
      then Float_Mult 2 x
      else case (evalFloat x, evalFloat y) of
            (Just a ,Just b ) -> Proc_Float $ a + b
            (Nothing,Just b ) -> Float_Sum (reduce x) (Proc_Float b)
            (Just a ,Nothing) -> Float_Sum (Proc_Float a) (reduce y)
            (Nothing,Nothing) -> Float_Sum (reduce x) (reduce y)
 reduce (Float_Substract x y) =
   if x == y
      then 0
      else case (evalFloat x, evalFloat y) of
            (Just a ,Just b ) -> Proc_Float $ a - b
            (Nothing,Just b ) -> Float_Substract (reduce x) (Proc_Float b)
            (Just a ,Nothing) -> Float_Substract (Proc_Float a) (reduce y)
            (Nothing,Nothing) -> Float_Substract (reduce x) (reduce y)
 reduce (Float_Mult x y) =
   if x == y
      -- x*x = x^2
      then x**2
      else case (evalFloat x, evalFloat y) of
            (Just a ,Just b ) -> Proc_Float $ a * b
            (Nothing,Just b ) -> Float_Mult (reduce x) (Proc_Float b)
            (Just a ,Nothing) -> Float_Mult (Proc_Float a) (reduce y)
            (Nothing,Nothing) -> Float_Mult (reduce x) (reduce y)
 reduce (Float_Divide (Float_Mult x y) z) =
   case evalFloat (Float_Divide x z) of
    -- (x*y)/z = y*(x/z)
    Just a -> reduce $ Float_Mult y $ Proc_Float a
    Nothing ->
      case evalFloat (Float_Divide y z) of
       -- (x*y)/z = x*(y/z)
       Just a -> reduce $ Float_Mult x $ Proc_Float a
       -- When both (x/z) and (y/z) contain a variable.
       Nothing -> Float_Divide (reduce $ Float_Mult x y) $ reduce z
 reduce (Float_Divide x y) =
   case (evalFloat x, evalFloat y) of
    (Just a ,Just b ) -> Proc_Float $ a / b
    (Nothing,Just b ) -> Float_Divide (reduce x) (Proc_Float b)
    (Just a ,Nothing) -> Float_Divide (Proc_Float a) (reduce y)
    (Nothing,Nothing) -> Float_Divide (reduce x) (reduce y)
 reduce (Float_Exp x) =
   case evalFloat x of
    Just a  -> Proc_Float $ exp a
    Nothing -> Float_Exp $ reduce x
 reduce (Float_Abs x) =
    case evalFloat x of
    Just a  -> Proc_Float $ abs a
    Nothing -> Float_Abs $ reduce x
 reduce (Float_Sqrt x) =
    case evalFloat x of
    Just a  -> Proc_Float $ sqrt a
    Nothing -> Float_Sqrt $ reduce x
 reduce (Float_Log x) =
    case evalFloat x of
    Just a  -> Proc_Float $ log a
    Nothing -> Float_Log $ reduce x
 reduce (Float_Sine x) =
    case evalFloat x of
    Just a  -> Proc_Float $ sin a
    Nothing -> Float_Sine $ reduce x
 reduce (Float_Cosine x) =
    case evalFloat x of
    Just a  -> Proc_Float $ cos a
    Nothing -> Float_Cosine $ reduce x
 reduce (Float_Arcsine x) =
    case evalFloat x of
    Just a  -> Proc_Float $ asin a
    Nothing -> Float_Arcsine $ reduce x
 reduce (Float_Arccosine x) =
    case evalFloat x of
    Just a  -> Proc_Float $ acos a
    Nothing -> Float_Arccosine $ reduce x
 reduce (Float_Arctangent x) =
    case evalFloat x of
    Just a  -> Proc_Float $ atan a
    Nothing -> Float_Arctangent $ reduce x
 reduce (Float_Floor x) =
    case evalFloat x of
    Just a  -> Proc_Float $ (fromIntegral . floorI) a
    Nothing -> Float_Floor $ reduce x
 reduce (Float_Noise x y) = Float_Noise (reduce x) (reduce y)
 reduce x = x

instance Reducible ProcArg where
 reduce (FloatArg x) = FloatArg $ reduce x
 reduce x = x

instance Reducible (ProcCode c) where
 reduce (Sequence sq) = Sequence $ fmap reduce $ foldr (
   \x xs -> 
     case Seq.viewl xs of
      y Seq.:< ys -> case reduceProcPair x y of
                       Nothing -> x Seq.<| xs
                       Just z  -> z Seq.<| ys
      Seq.EmptyL -> Seq.singleton x
        ) Seq.empty sq
 reduce (Command n xs) = Command n $ fmap reduce xs
 reduce x = x

reduceProcPair :: ProcCode c -> ProcCode c -> Maybe (ProcCode c)
reduceProcPair (Command "translate" [FloatArg x,FloatArg y])
               (Command "translate" [FloatArg x',FloatArg y'])
                 = Just $ Command "translate" [ FloatArg $ x+x'
                                              , FloatArg $ y+y']
reduceProcPair (Command "rotate" [FloatArg x])
               (Command "rotate" [FloatArg x'])
                 = Just $ Command "rotate" [FloatArg $ x+x']
reduceProcPair _ _ = Nothing

----

-- | A complete Processing script.
--
-- It consists in several parts, most of them optional.
--
-- * 'Preamble': Usually the place where variables are
--   initialized.
--
-- * 'Setup': After running the code in the preamble,
--   the code in this part is executed once.
--
-- * 'Draw': After the setup, this part of the code is
--   executed in loops over and over again.
--
-- * 'MouseClicked': Each time the user clicks, the code
--   here is executed once.
--
-- To generate each part of the code, use the 'ProcM' monad
-- and the functions from the "Graphics.Web.Processing.Interface"
-- module. Then, run 'runProcM' or 'execProcM' to get the
-- code result.
data ProcScript = ProcScript
 { proc_preamble :: ProcCode Preamble
 , proc_setup :: ProcCode Setup
 , proc_draw  :: Maybe (ProcCode Draw)
 , proc_mouseClicked :: Maybe (ProcCode MouseClicked)
   }

-- | Empty script.
emptyScript :: ProcScript
emptyScript = ProcScript {
   proc_preamble = emptyCode
 , proc_setup = emptyCode
 , proc_draw = Nothing
 , proc_mouseClicked = Nothing
   }

pvoid :: Pretty a => Text -> Maybe a -> Doc
pvoid _ Nothing = Text.PrettyPrint.Mainland.empty
pvoid n (Just c) = fromText "void" <+> fromText n <> fromText "()" <+> enclose lbrace rbrace inside
 where
  inside = line <> indent indentLevel (ppr c) <> line

instance Pretty ProcScript where
 ppr ps = stack [ 
     ppr $ proc_preamble ps
   , pvoid "setup" $ Just $ proc_setup ps
   , pvoid "draw" $ proc_draw ps
   , pvoid "mouseClicked" $ proc_mouseClicked ps
     ]
