
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies,
             DeriveGeneric, TypeOperators, DefaultSignatures, FlexibleContexts,
             TemplateHaskell
  #-}

{- | Internal core module.
The purpose of this module is to define the most basic types
and write the necessary instances for them.
-}
module Graphics.Web.Processing.Core.Primal (
  -- * Types
  -- ** Singleton types
  -- | A list of singleton types, used to restrict the
  --   use of certain commands to specific contexts.
    Preamble (..), Setup (..), Draw (..)
  , MouseClicked (..), MouseReleased (..)
  , KeyPressed (..)
  -- ** Recursive types
  , Recursive (..)
  -- ** @Proc_*@ types
  -- *** Boolean
  , Proc_Bool (..), fromBool
  , true, false
  , pnot, (#||), (#&&)
  -- *** Int
  , Proc_Int (..), fromInt
  , pfloor, pround
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
  -- *** Keys
  , Proc_Key (..)
  , Proc_KeyCode (..)
  -- ** Type class of proc types
  , ProcType (..)
  -- ** Conditionals
  , Proc_Eq (..)
  , Proc_Ord (..)
  -- ** Reductions
  , Reducible (..)
  -- ** Variables
  , Var, varName, varFromText
  , ArrayVar, arrayVarName, arrayVarFromText
  , arraySize
  , arrayVarToVar
  -- ** Script
  , ProcCode (..), ProcArg (..), ProcAssign (..)
  , ProcList (..)
  , emptyCode
  , (>>.)
  , ProcScript (..)
  , emptyScript
  ) where

import Prelude hiding (foldr)
import Data.Text (Text,lines,pack)
import Data.Text.Lazy (toStrict)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.String
import Data.Foldable (foldMap,foldr)
import Control.Applicative
-- Pretty
import Text.PrettyPrint.Mainland
-- QuickCheck
import Test.QuickCheck (Arbitrary (..), Gen, oneof, sized, resize, vectorOf)
import Test.QuickCheck.Instances()
-- Meta-programming
import GHC.Generics
import Graphics.Web.Processing.Core.TH

------------------------------------------------
-- QUICK CHECK DERIVING

{-

Some of the types defined in this module have a big
amount of data constructors. Creating Arbitrary instances
for each of them manually is a tedious and unnecessary work.

In order to be able to derive automatically instance for
the arbitrary typeclass, we create a new class, PArbitrary
(from Processing Arbitrary). Having access to the class
definition, we can provide a default instance based in our
generic deriving. Once an instance to PArbitrary is done,
the Arbitrary instance is trivial:

instance Arbitrary a where
 arbitrary = parbitrary

Given the number of data constructors, and being most of them
recursive, if we create totally random values, the chances of
creating (insanely) huge values is very high. To avoid it, we
set a maximum number of random steps (see 'sizeLimit'). When
this number is reached, we "travel" to the left-most constructor,
which by definition will be finite (something like Proc_Float Float).

-}

class PArbitrary a where
 parbitrary :: Gen a
 default parbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
 parbitrary = to <$> garbitrary

class GArbitrary f where
 garbitrary :: Gen (f a)

instance GArbitrary U1 where
 garbitrary = pure U1

instance (GArbitrary a, GArbitrary b) => GArbitrary (a :*: b) where
 garbitrary = sized $
   \n -> resize (n+1) $ (:*:) <$> garbitrary <*> garbitrary

sizeLimit :: Int
sizeLimit = 15

instance (GArbitrary a, GArbitrary b) => GArbitrary (a :+: b) where
 garbitrary = sized $
     \n -> if n > sizeLimit
              then L1 <$> garbitrary
              else oneof [L1 <$> garbitrary, R1 <$> garbitrary]

instance GArbitrary a => GArbitrary (M1 i c a) where
 garbitrary = M1 <$> garbitrary

instance PArbitrary a => GArbitrary (K1 i a) where
 garbitrary = K1 <$> parbitrary

------------------------------------------------
-- DEFAULT INSTANCES

instance Arbitrary a => PArbitrary (Maybe a) where
 parbitrary = arbitrary

instance Arbitrary a => PArbitrary (Seq.Seq a) where
 parbitrary = arbitrary

instance PArbitrary Int where
 parbitrary = arbitrary

instance PArbitrary Float where
 parbitrary = arbitrary

instance PArbitrary Text where
 parbitrary = pack <$> vectorOf 4 parbitrary

instance PArbitrary Char where
 parbitrary = oneof $ fmap pure [ 'a' .. 'z' ]

instance Arbitrary a => PArbitrary [a] where
 parbitrary = arbitrary

------------------------------------------------

{-
Processing.js code is divided in different sections.
Each section handles a different event. Naturally,
there are commands that may be called inside of a
particular context, but not within another. Most of
these commands are runnable in different kind of
events. Writing variables should be possible from
any event. To handle this situation, we annotate
the AST with a /context/. This context indicates
which event that portion of code belongs to. For
example, @ProcCode Draw@ indicates that the code
belongs to the draw loop. Now we can restrict
functions to work only under certain contexts.
For example, variables should be created only once.
Since events may be called several times, we restrict
the type of any function that creates variables,
annotating the type with 'Preamble'.
-}

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

-- | Code that is executed when the mouse is released.
data MouseReleased = MouseReleased

-- | Code executed when a key is pressed.
data KeyPressed = KeyPressed

-- PRETTY-HELPERS

pfunction :: Text -> [Doc] -> Doc
pfunction n as = fromText n <> parens (commasep as)

-- TYPES

{-

Some Proc_* types can be seen as extensions
of some Haskell types. For example Proc_Float
may contain a Float under the Proc_Float data
constructor.  However, it has more data constructors
and, therefore, it may contain other different
values. The Extended class is created for
these types. It provides two methods that, once
defined, permit to extend functions and operators
from the type that has been extended to the extension.
For example, we can extend the sin function, which
is defined for Float values, to value of the type
Proc_Float.

The two methods required are extend and patmatch
(pattern match). The method extend should inject
a value from the extended type to the extension.
The method patmatch would do the opposite. However,
not every element in the extension belongs to the
extended type. We return Nothing in those cases.

Extended functions and operators behave the same
way as the originals for values in the extended
type. A supplied default function/operator
indicates what to do in the rest of cases.

-}

class Extended from to | to -> from where
 extend :: from -> to
 patmatch :: to -> Maybe from

-- | Function extension.
extendf :: (Extended from to, Extended from' to')
        => (from -> from') -> (to -> to') -> (to -> to')
extendf f g x =
  case patmatch x of
    Nothing -> g x
    Just a  -> extend $ f a

-- | Operator extension.
extendop :: (Extended from   to
            ,Extended from'  to'
            ,Extended from'' to'')
         => (from -> from' -> from'')
         -> (to   -> to'   -> to'')
         -> (to   -> to'   -> to'')
extendop f g x y =
  case (patmatch x, patmatch y) of
    (Just a, Just b) -> extend $ f a b
    _ -> g x y

{- | Class of recursive types.

The 'recursor' function applies the
given function to every subexpression
of the same type. For example, this would
be the recursor over lists:

recursor f [] = []
recursor f (x:xs) = x : f xs

Instances of Recursive can be derived
using $(deriveRecursive ''Type).
-}
class Recursive a where
 recursor :: (a -> a) -> a -> a

{- Proc_* types

Proc_* types are AST's for different kind
of expressions. For example, a value of type
Proc_Bool store an AST of a boolean expression.
The "Proc_" prefix indicates that the type
of the expression matches a type in Processing.

Proc_* types have a specialized version of
'extend'. This way, the Extended class can be
kept hidden to the user. If this is or not a
good idea is something to be discussed.
Note that the class use Functional Dependencies.

-}

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
     -- Key
   | Key_Eq Proc_Key Proc_Key
   | KeyCode_Eq Proc_KeyCode Proc_KeyCode
   -- Conditional
 | Bool_Cond Proc_Bool Proc_Bool Proc_Bool
     deriving (Eq,Ord,Generic)

instance PArbitrary Proc_Bool

instance Arbitrary Proc_Bool where
 arbitrary = parbitrary

instance Extended Bool Proc_Bool where
 extend True  = Proc_True 
 extend False = Proc_False
 patmatch Proc_True = Just True
 patmatch Proc_False = Just False
 patmatch _ = Nothing

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

-- | Processing.js syntax for conditionals.
--
-- > <bool> ? <a> : <a>
--
docCond :: Doc -> Doc -> Doc -> Doc
docCond _if _then _else = _if <+> fromText "?" <+> _then <+> fromText ":" <+> _else

-- | This constant indicates how many spaces
--   are added in each indentation. Events
--   and conditionals add indentation.
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
 ppr (Key_Eq x y) = parens $ ppr x <+> docEq <+> ppr y
 ppr (KeyCode_Eq x y) = parens $ ppr x <+> docEq <+> ppr y
 -- Conditional
 ppr (Bool_Cond b x y) = parens $ docCond (ppr b) (ppr x) (ppr y)

-- | Value of 'True'.
true :: Proc_Bool
true = Proc_True

-- | Value of 'False'.
false :: Proc_Bool
false = Proc_False

-- | Negation.
pnot :: Proc_Bool -> Proc_Bool
pnot = extendf not Proc_Neg

infixr 2 #||

-- | Disjunction.
(#||) :: Proc_Bool -> Proc_Bool -> Proc_Bool
(#||) = extendop (||) Proc_Or

infixr 3 #&&

-- | Conjunction.
(#&&) :: Proc_Bool -> Proc_Bool -> Proc_Bool
(#&&) = extendop (&&) Proc_And

-- | Cast a 'Bool' value.
fromBool :: Bool -> Proc_Bool
fromBool = extend

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
 | Int_Round Proc_Float
   -- Conditional
 | Int_Cond Proc_Bool Proc_Int Proc_Int
   deriving (Eq,Ord,Generic)

instance PArbitrary Proc_Int

instance Arbitrary Proc_Int where
 arbitrary = parbitrary

instance Extended Int Proc_Int where
 extend = Proc_Int
 patmatch (Proc_Int a) = Just a
 patmatch _ = Nothing

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
 ppr (Int_Round x) = pfunction "round" [ppr x]
 ppr (Int_Cond b x y) = parens $ docCond (ppr b) (ppr x) (ppr y)

-- | Cast an 'Int' value.
fromInt :: Int -> Proc_Int
fromInt = extend

-- | Calculate the 'floor' of a 'Proc_Float'.
pfloor :: Proc_Float -> Proc_Int
pfloor = extendf floor Int_Floor

-- | Round a number to the closest integer.
pround :: Proc_Float -> Proc_Int
pround = extendf round Int_Round

instance Enum Proc_Int where
 toEnum = fromInt
 fromEnum n = case patmatch n of
  Nothing -> error "Proc_Int: fromEnum applied to a variable."
  Just i -> i
 succ n = n + 1
 pred n = n - 1

-- | WARNING: 'signum' method is undefined.
instance Num Proc_Int where
 fromInteger = fromInt . fromInteger
 (+) = extendop (+) Int_Sum
 (-) = extendop (-) Int_Substract
 (*) = extendop (*) Int_Mult
 abs = extendf abs Int_Abs
 signum = error "Proc_Int: signum method is undefined."

instance Real Proc_Int where
 toRational n = case patmatch n of
   Nothing -> error "Proc_Int: toRational applied to a variable."
   Just i -> toRational i

instance Integral Proc_Int where
 div = extendop div Int_Divide
 mod = extendop mod Int_Mod
 quotRem n d = (div n d, mod n d)
 divMod = quotRem
 toInteger n = case patmatch n of
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
 | Float_Neg Proc_Float
   -- Variables
 | Float_Var Text
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
 | Float_Round Proc_Float -- Same observation for Float_Floor.
 | Float_Noise Proc_Float Proc_Float
 | Float_Random Proc_Float Proc_Float
   -- Conditional
 | Float_Cond Proc_Bool Proc_Float Proc_Float
   deriving (Eq,Ord,Generic)

instance PArbitrary Proc_Float

instance Arbitrary Proc_Float where
 arbitrary = parbitrary

instance Extended Float Proc_Float where
 extend = Proc_Float
 patmatch (Proc_Float x) = Just x
 patmatch _ = Nothing

instance Pretty Proc_Float where 
 ppr (Proc_Float f) = ppr f
 ppr (Float_Sum x y) = parens $ ppr x <> fromText "+" <> ppr y
 ppr (Float_Substract x y) = parens $ ppr x <> fromText "-" <> ppr y
 ppr (Float_Divide x y) = parens $ ppr x <> fromText "/" <> ppr y
 ppr (Float_Mult x y) = parens $ ppr x <> fromText "*" <> ppr y
 ppr (Float_Neg x) = fromText "-" <> ppr x
 ppr (Float_Mod x y) = parens $ ppr x <> fromText "%" <> ppr y
 ppr (Float_Var t) = fromText t
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
 ppr (Float_Round x) = pfunction "round" [ppr x]
 ppr (Float_Noise x y) = pfunction "noise" [ppr x,ppr y]
 ppr (Float_Cond b x y) = parens $ docCond (ppr b) (ppr x) (ppr y)
 ppr (Float_Random x y) = pfunction "random" [ppr x,ppr y]

-- | Cast a 'Float' value.
fromFloat :: Float -> Proc_Float
fromFloat = extend

-- | Noise random function.
noisef :: Proc_Float -> Proc_Float -> Proc_Float
noisef = Float_Noise

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
intToFloat (Int_Round x) = Float_Round x
intToFloat (Int_Cond b x y) = Float_Cond b (intToFloat x) (intToFloat y)

-- | WARNING: 'signum' method is undefined.
instance Num Proc_Float where
 fromInteger = fromFloat . fromInteger
 (+) = extendop (+) Float_Sum
 (-) = extendop (-) Float_Substract
 (*) = extendop (*) Float_Mult
 abs = extendf abs Float_Abs
 negate = extendf negate Float_Neg
 signum = error "Proc_Float: signum method is undefined."

instance Fractional Proc_Float where
 (/) = extendop (/) Float_Divide
 fromRational = fromFloat . fromRational

-- | WARNING: 'sinh', 'cosh', 'asinh', 'acosh' and 'atanh'
--   methods are undefined. They are not present in
--   processing.js.
instance Floating Proc_Float where
 pi = extend pi
 exp = extendf exp Float_Exp
 sqrt = extendf sqrt Float_Sqrt
 log = extendf log Float_Log
 sin = extendf sin Float_Sine
 cos = extendf cos Float_Cosine
 asin = extendf asin Float_Arcsine
 acos = extendf acos Float_Arccosine
 atan = extendf atan Float_Arctangent
 -- UNDEFINED
 sinh = error "Proc_Float: sinh method is undefined."
 cosh = error "Proc_Float: cosh method is undefined."
 asinh = error "Proc_Float: asinh method is undefined."
 acosh = error "Proc_Float: acosh method is undefined."
 atanh = error "Proc_Float: atanh method is undefined."

-- | Type of images.
data Proc_Image =
   Image_Var Text
 | Image_Cond Proc_Bool Proc_Image Proc_Image
   deriving (Eq,Generic)

instance PArbitrary Proc_Image

instance Arbitrary Proc_Image where
 arbitrary = parbitrary

instance Pretty Proc_Image where
 ppr (Image_Var t) = fromText t
 ppr (Image_Cond b x y) = parens $ docCond (ppr b) (ppr x) (ppr y)

-- | Type of characters.
data Proc_Char =
   Proc_Char Char
 | Char_Var Text
 | Char_Cond Proc_Bool Proc_Char Proc_Char
   deriving (Eq,Ord,Generic)

instance PArbitrary Proc_Char

instance Arbitrary Proc_Char where
 arbitrary = parbitrary

instance Extended Char Proc_Char where
 extend = Proc_Char
 patmatch (Proc_Char c) = Just c
 patmatch _ = Nothing

-- | Cast a 'Char' value.
fromChar :: Char -> Proc_Char
fromChar = extend

instance Pretty Proc_Char where
 ppr (Proc_Char c) = enclose squote squote (char c)
 ppr (Char_Var n) = fromText n
 ppr (Char_Cond b x y) = parens $ docCond (ppr b) (ppr x) (ppr y)

-- | Type of textual values.
--
--   It is recommended to enable the @OverloadedStrings@ extension.
--   Note that 'Proc_Text' is an instance of the 'IsString' class.
data Proc_Text =
   Proc_Text Text
 | Text_Var Text
   -- Conditional
 | Text_Cond Proc_Bool Proc_Text Proc_Text
   deriving (Eq,Ord,Generic)

instance PArbitrary Proc_Text

instance Arbitrary Proc_Text

instance Extended Text Proc_Text where
 extend = Proc_Text
 patmatch (Proc_Text t) = Just t
 patmatch _ = Nothing

instance Pretty Proc_Text where
 -- Wrong pretty-printer for text values. Fix it to
 -- escape characters.
 ppr (Proc_Text t) = enclose dquote dquote (fromText t)
 ppr (Text_Var n) = fromText n
 ppr (Text_Cond b x y) = parens $ docCond (ppr b) (ppr x) (ppr y)

-- | Cast a strict 'Text' value.
fromStText :: Text -> Proc_Text
fromStText = extend

instance IsString Proc_Text where
 fromString = fromStText . fromString

-- | Type of keyboard keys.
data Proc_Key =
   Key_Var
 | Key_CODED
 | Key_Char Char
   deriving (Eq,Ord,Generic)

instance PArbitrary Proc_Key

instance Pretty Proc_Key where
 ppr Key_Var = fromText "key"
 ppr Key_CODED = fromText "CODED"
 ppr (Key_Char c) = ppr $ fromChar c

-- | Type of keyboard key codes.
data Proc_KeyCode =
   KeyCode_Var
 | KeyCode_UP
 | KeyCode_DOWN
 | KeyCode_LEFT
 | KeyCode_RIGHT
 | KeyCode_ALT
 | KeyCode_CONTROL
 | KeyCode_SHIFT
 | KeyCode_BACKSPACE
 | KeyCode_TAB
 | KeyCode_ENTER
 | KeyCode_RETURN
 | KeyCode_ESC
 | KeyCode_DELETE
   deriving (Eq,Ord,Generic)

instance PArbitrary Proc_KeyCode

instance Pretty Proc_KeyCode where
 ppr KeyCode_Var = fromText "keyCode"
 ppr KeyCode_UP = fromText "UP"
 ppr KeyCode_DOWN = fromText "DOWN"
 ppr KeyCode_LEFT = fromText "LEFT"
 ppr KeyCode_RIGHT = fromText "RIGHT"
 ppr KeyCode_ALT = fromText "ALT"
 ppr KeyCode_CONTROL = fromText "CONTROL"
 ppr KeyCode_SHIFT = fromText "SHIFT"
 ppr KeyCode_BACKSPACE = fromText "BACKSPACE"
 ppr KeyCode_TAB = fromText "TAB"
 ppr KeyCode_ENTER = fromText "ENTER"
 ppr KeyCode_RETURN = fromText "RETURN"
 ppr KeyCode_ESC = fromText "ESC"
 ppr KeyCode_DELETE = fromText "DELETE"


-- END OF PROC_* TYPES
----------------------------------------------
----------------------------------------------

{- Proc_* types mechanics

Two types are automatically generated for the
Proc_* types. These are ProcArg and ProcAssign.
A processing command may receive several arguments
of the same or different Proc_* types.
We encode arguments under the ProcArg type. The
ProcArg type is the disjoint union of the different
Proc_* types. For a complete list, see the
Graphics.Web.Processing.Core.TH module.
The list is called procTypeNames.

In the other hand, variable assignments are also
encoded in a particular type, named ProcAssign.
The ProcAssign type is the disjoint union of the
product of each Proc_* type with Text. This means
that a value of this type contains a value of any
of the different Proc_* types together with a value
of type Text. This text represents the name of the
variable in the assignment.

Both ProcArg and ProcAssign are generated
automatically, together with instances of the
Pretty class, by procTypeMechs.
Compile with "-f info" to see the generated
code.

-}

$(procTypeMechs)

instance PArbitrary ProcArg

instance Arbitrary ProcArg where
 arbitrary = parbitrary

instance PArbitrary ProcAssign

instance PArbitrary ProcList

-- CODE

-- | A piece of Processing code.
--   The type parameter indicates what the
--   context of the code is.
--   This context will allow or disallow
--   the use of certain commands along
--   different events.
data ProcCode c = 
   Command Text [ProcArg] 
 | CreateVar ProcAssign
 | CreateArrayVar Text ProcList
 | Assignment ProcAssign
 | Conditional Proc_Bool   -- IF
              (ProcCode c) -- THEN
              (ProcCode c) -- ELSE
 | Comment Text
 | Sequence (Seq.Seq (ProcCode c))
   deriving (Generic,Eq)

instance PArbitrary (ProcCode c)

instance Arbitrary (ProcCode c) where
 arbitrary = parbitrary

instance Pretty (ProcCode c) where
 ppr (Command n as) = pfunction n (fmap ppr as) <+> fromText ";"
 -- ptype is defined by $(procTypeMechs).
 ppr (CreateVar a) = ptype a <+> ppr a <+> fromText ";"
 -- ltype is defined by $(procTypeMechs).
 ppr (CreateArrayVar n xs) = ltype xs <+> fromText n <+> fromText "="
                         <+> ppr xs <+> fromText ";"
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

-- | Sequence to pieces of code with the same
--   context type. This way, code that belongs
--   to different parts of the program will
--   never get mixed.
(>>.) :: ProcCode c -> ProcCode c -> ProcCode c
(Sequence xs) >>. (Sequence ys) = Sequence $ xs Seq.>< ys
(Sequence xs) >>. p = if Seq.null xs 
                         then p
                         else Sequence $ xs Seq.|> p
p >>. (Sequence xs) = if Seq.null xs
                         then p
                         else Sequence $ p Seq.<| xs
p >>. q = Sequence $ Seq.fromList [p,q]

-- | An empty piece of code.
emptyCode :: ProcCode a
emptyCode = Sequence $ Seq.empty

instance Monoid (ProcCode a) where
 mempty = emptyCode
 mappend = (>>.)

---- ProcType class and instances

-- | Type of variables.
data Var a = Var { -- | Get the name of a variable.
                   varName :: Text }

-- | Internal function to create variables.
varFromText :: Text -> Var a
varFromText = Var

-- | Type of variables storing arrays.
data ArrayVar a =
  ArrayVar { -- | Size of the array.
             arraySize :: Int
           , innerVar :: Var a }

-- | Get the name of a variable storing an array.
arrayVarName :: ArrayVar a -> Text
arrayVarName = varName . innerVar

-- | Internal function to create array variables.
arrayVarFromText :: Int -> Text -> ArrayVar a
arrayVarFromText n t = ArrayVar n (Var t)

-- | Translate an Array variable to the correspondent
--   component variable.
arrayVarToVar :: ArrayVar a -> Proc_Int -> Var a
arrayVarToVar v n = varFromText $ arrayVarName v <> "[" <> f n <> "]"
  where
   f = toStrict . prettyLazyText 80 . ppr

----- CLASSES

-- | Class of Processing value types (@Proc_*@ types).
--
--   @Proc_*@ types are types from the world of Processing.
--   Some of them are similar to Haskell types, like 'Proc_Bool'
--   and 'Bool'. However, they are not equal. @Proc_*@ types
--   are instance of 'Eq'. However, you should instead use methods from
--   the analog 'Proc_Eq' class. @Proc_*@ types contain expressions instead
--   of values. Think of @2+2@ instead of @4@. Under this situation,
--   @2+2 /= 3+1@, since they are different expressions, even if they
--   evaluate to the same value. Actually, you will get 'True'
--   from the evaluation of @2+2 == 3+1@, since the library is smart
--   enough to figure out they have the same value. But, please, don't
--   rely on this. Use the 'Proc_Eq' and 'Proc_Ord' classes instead.
--   They return Processing boolean expressions instead of 'Bool' values.
--   Anyway, the types of the library will try to force you to use @Proc_*@
--   types everywhere.
--
--   The reason this library stores expressions instead of values is that
--   it needs to handle things like @2+x@, where @x@ is an unknown value.
--   However, an effort is done to ensure that each expression is reduced
--   to its minimum extension.
class ProcType a where
 -- | Create a variable assignment, provided
 --   the name of the variable and the value to asign.
 proc_assign :: Text -> a -> ProcAssign
 -- | Create a list.
 proc_list :: [a] -> ProcList
 -- | Create an argument for a command.
 proc_arg :: a -> ProcArg
 -- | Variable reading.
 proc_read :: Var a -> a
 -- | Conditional value.
 proc_cond :: Proc_Bool -> a -> a -> a

{- Template Haskell and Proc_* types.

Template Haskell is used in order to derive instances
of the ProcType class. These instances consist merely
in select the appropiate data constructor of the
appropiate datatype. Use "-f info" when compiling
with cabal to see the generated instances.

-}

$(deriveProcTypeInsts)

{- Eq and Ord classes for Proc_* types

Since Proc_* types represent expressions,
they cannot be compared in the usual way.
We cannot decide if two expressions will reduce
to the same value. In fact, sometimes they will,
and sometimes they will not. What we can do is
to, given two expressions, return a boolean
expression, which will evaluate to the correct
value in the appropiate context.

Therefore, we define the Proc_Eq and Proc_Ord
classes similarly to Eq and Ord, but returning
a Proc_Bool value instead of a Bool value.

Operators have the same name than their
analagous, but preceded with #.

-}

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

instance Proc_Eq Proc_Key where
 (#==) = Key_Eq

instance Proc_Eq Proc_KeyCode where
 (#==) = KeyCode_Eq

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

{- Proc_* types and recursion

Since some of the Proc_* types are recursive,
we derive the correspondent instances using
Template Haskell. Otherwise, they would be
long and tedious.

-}

$(deriveRecursive ''Proc_Bool)
$(deriveRecursive ''Proc_Int)
$(deriveRecursive ''Proc_Float)

-- | Class of reducible types. Values of these
--   types contain expressions that can be
--   reducible.
class Eq a => Reducible a where
 reduce :: a -> a

-- | Find a fix point of the 'reduce' function from
--   any value. If 'reduce' is well defined, this function
--   must end.
iteratedReduce :: Reducible a => a -> a
iteratedReduce = fst . firstWith (uncurry (==)) . pairing . iterate reduce
 where
  pairing (x:y:xs) = (x,y) : pairing (y:xs)
  pairing _ = []
  firstWith f (x:xs) = if f x then x else firstWith f xs
  firstWith _ _ = error "Error in iterated reduction. Report this as a bug."

{-

FLOAT REDUCTION

Float is probably the most common argument type.
Below a case-by-case analysis tries to reduce a
float expression to its minimal extension.
The more we reduce, the more effective
will be the Processing output code, since we save
operations.

-}

instance Reducible Proc_Float where
 reduce (Float_Sum x y) =
   if x == y
      -- x+x = 2*x
      then 2 * reduce x
      else reduce x + reduce y
 reduce (Float_Substract x y) =
   if x == y
      -- x-x = 0
      then 0
      else reduce x - reduce y
 reduce (Float_Mult x y) =
   if x == y
      -- x*x = x^2
      then reduce x ** 2
      else reduce x * reduce y
 -- (x*y)/z = y*(x/z)
 reduce (Float_Divide (Float_Mult (Proc_Float x) y) (Proc_Float z)) =
   reduce $ (y*) $ Proc_Float $ x / z
 -- (x*y)/z = x*(y/z)
 reduce (Float_Divide (Float_Mult x (Proc_Float y)) (Proc_Float z)) =
   reduce $ (x*) $ Proc_Float $ y / z
 reduce x = recursor reduce x

instance Reducible ProcArg where
 reduce (FloatArg x) = FloatArg $ iteratedReduce x
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
-- Translations
reduceProcPair (Command "translate" [FloatArg 0,FloatArg 0]) x = Just x
reduceProcPair x (Command "translate" [FloatArg 0,FloatArg 0]) = Just x
reduceProcPair (Command "translate" [FloatArg x,FloatArg y])
               (Command "translate" [FloatArg x',FloatArg y'])
                 = Just $ Command "translate" [ FloatArg $ x+x'
                                              , FloatArg $ y+y']
-- Rotations
reduceProcPair (Command "rotate" [FloatArg 0]) x = Just x
reduceProcPair x (Command "rotate" [FloatArg 0]) = Just x
reduceProcPair (Command "rotate" [FloatArg x])
               (Command "rotate" [FloatArg x'])
                 = Just $ Command "rotate" [FloatArg $ x+x']
reduceProcPair _ _ = Nothing

----

-- | A complete Processing script.
--
-- It consists in several parts, most of them optional.
--
-- To generate each part of the code, use the 'ProcM' monad
-- and the functions from the "Graphics.Web.Processing.Interface"
-- module. Then, run 'runProcM' or 'execProcM' to get the
-- code result.
--
-- More abstract functions generate 'ProcScript' values as well.
-- See modules "Graphics.Web.Processing.Mid" and "Graphics.Web.Processing.Simple"
-- for two alternative ways.
data ProcScript = ProcScript
 { proc_preamble :: ProcCode Preamble
 , proc_setup :: ProcCode Setup
 , proc_draw  :: Maybe (ProcCode Draw)
 , proc_mouseClicked :: Maybe (ProcCode MouseClicked)
 , proc_mouseReleased :: Maybe (ProcCode MouseReleased)
 , proc_keyPressed :: Maybe (ProcCode KeyPressed)
   } deriving (Eq,Generic)

instance PArbitrary ProcScript

instance Arbitrary ProcScript where
 arbitrary = parbitrary

-- | Empty script.
emptyScript :: ProcScript
emptyScript = ProcScript {
   proc_preamble = emptyCode
 , proc_setup = emptyCode
 , proc_draw = Nothing
 , proc_mouseClicked = Nothing
 , proc_mouseReleased = Nothing
 , proc_keyPressed = Nothing
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
   , pvoid "mouseReleased" $ proc_mouseReleased ps
   , pvoid "keyPressed" $ proc_keyPressed ps
     ]
