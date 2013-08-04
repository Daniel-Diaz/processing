
{-# LANGUAGE OverloadedStrings #-}

-- | Variables, commands and functions. The /interface/ to
--   the processing.js API (<http://processingjs.org/reference>),
--   with some additions, deletions and modifications.
module Graphics.Web.Processing.Core.Interface (
   -- * Predefined values
     screenWidth, screenHeight
   -- * Commands
   -- ** General
   , noise
   -- ** Drawing
   , Drawing
   -- *** Colors
   , Color (..)
   , stroke, fill, background
   -- *** Stroke settings
   , strokeWeight
   -- *** Figures
   , Proc_Point
   , ellipse
   , circle
   , arc
   , line
   , point
   , quad
   , rect
   , triangle
   , bezier
   , polygon
   -- *** Text
   , drawtext
   -- ** Setup
   , size
   , setFrameRate
   -- * Transformations
   , translate
   , rotate
   , scale
   , resetMatrix
   -- * Mouse
   , getMousePoint
   -- * Keyboard
   , Key (..)
   , ArrowKey (..)
   , KeyModifier (..)
   , SpecialKey (..)
   -- * Conditionals
   , iff
   -- * Others
   , frameCount
   , getFrameRate
   , writeComment
   -- * Processing monads
   , ProcMonad
   ) where

-- Internal
import Graphics.Web.Processing.Core.Primal
import Graphics.Web.Processing.Core.Monad
import Graphics.Web.Processing.Core.Var (readVar)

---- PREDEFINED VALUES

-- | Width of the canvas.
screenWidth :: Proc_Int
screenWidth = proc_read $ varFromText "screenWidth"

-- | Height of the canvas.
screenHeight :: Proc_Int
screenHeight = proc_read $ varFromText "screenHeight"

---- PREDEFINED VARIABLES

-- | Frames since the beginning of the script execution.
frameCount :: (ProcMonad m, Monad (m c)) => m c Proc_Int
frameCount = readVar $ varFromText "frameCount"

-- | Approximate number of frames per second.
getFrameRate :: (ProcMonad m, Monad (m c)) => m c Proc_Int
getFrameRate = readVar $ varFromText "frameRate"

-- COMMANDS

---- GENERAL

-- | Noise random function.
noise :: (ProcMonad m, Monad (m c)) => Proc_Point -> m c Proc_Float
noise (x,y) = return $ noisef x y

---- DRAWING

-- | Class of contexts where the user can draw pictures
--   in the screen. Instances:
--
-- * 'Setup'
--
-- * 'Draw'
--
-- * 'MouseClicked'
--
-- * 'MouseReleased'
class Drawing a where

instance Drawing Setup where
instance Drawing Draw where
instance Drawing MouseClicked where
instance Drawing MouseReleased where

------ COLORS

-- | RGBA colors. Values must be between
--   0 and 255, including in the alpha channel.
data Color = Color {
    -- | Red channel.
    redc   :: Proc_Int
    -- | Blue channel.
  , bluec  :: Proc_Int
    -- | Green channel.
  , greenc :: Proc_Int
    -- | Alpha channel (opacity).
    -- 0 means transparent, and 255 opaque.
  , alphac :: Proc_Int
    }

colorArgs :: Color -> [ProcArg]
colorArgs (Color r g b a) = fmap proc_arg [r,g,b,a]

-- | Set the drawing color.
stroke :: (ProcMonad m, Drawing c) => Color -> m c ()
stroke = commandM "stroke" . colorArgs

-- | Set the filling color.
fill :: (ProcMonad m, Drawing c) => Color -> m c ()
fill = commandM "fill" . colorArgs

-- | Fill the screen with a given color.
background :: (ProcMonad m, Drawing c) => Color -> m c ()
background = commandM "background" . colorArgs

-- | Set the weight of the lines.
strokeWeight :: (ProcMonad m, Drawing c) => Proc_Int -> m c ()
strokeWeight n = commandM "strokeWeight" [proc_arg n]

------ FIGURES

-- | A point as a pair of floating point numbers.
type Proc_Point = (Proc_Float,Proc_Float)

-- | Draw a ellipse.
ellipse :: (ProcMonad m, Drawing c)
        => Proc_Point -- ^ Center of the ellipse.
        -> Proc_Float -- ^ Width of the ellipse.
        -> Proc_Float -- ^ Height of the ellipse.
        -> m c ()
ellipse (x,y) w h = commandM "ellipse" $ fmap proc_arg [x,y,w,h]

-- | Draw a circle.
circle :: (ProcMonad m, Drawing c)
       => Proc_Point -- ^ Center of the circle.
       -> Proc_Float -- ^ Radius.
       -> m c ()
circle p r = ellipse p (2*r) (2*r)

-- | Draw an arc.
--
--   The arc is drawn following the line of an ellipse
--   between two angles.
arc :: (ProcMonad m, Drawing c)
    => Proc_Point -- ^ Center of the ellipse.
    -> Proc_Float -- ^ Width of the ellipse.
    -> Proc_Float -- ^ Height of the ellipse.
    -> Proc_Float -- ^ Initial angle (in radians).
    -> Proc_Float -- ^ End angle (in radians).
    -> m c ()
arc (x,y) w h a0 a1 = commandM "arc" $ fmap proc_arg [x,y,w,h,a0,a1]

-- | Draw a line.
line :: (ProcMonad m, Drawing c)
     => Proc_Point -- ^ Starting point.
     -> Proc_Point -- ^ End point.
     -> m c ()
line (x0,y0) (x1,y1) = commandM "line" $ fmap proc_arg [x0,y0,x1,y1]

-- | Prints a dot.
point :: (ProcMonad m, Drawing c)
      => Proc_Point -- ^ Location of the point.
      -> m c ()
point (x,y) = commandM "point" $ fmap proc_arg [x,y]

-- | A quad is a quadrilateral, a four sided polygon.
--   The first parameter is the first vertex and the
--   subsequent parameters should proceed clockwise or
--   counter-clockwise around the defined shape.
quad :: (ProcMonad m, Drawing c)
     => Proc_Point -> Proc_Point -> Proc_Point -> Proc_Point
     -> m c ()
quad (x0,y0) (x1,y1) (x2,y2) (x3,y3) =
  commandM "quad" $ fmap proc_arg [x0,y0,x1,y1,x2,y2,x3,y3]

-- | Draws a rectangle to the screen. A rectangle is a
--   four-sided shape with every angle at ninety degrees.
--   The first parameter set the location, the
--   second sets the width, and the third sets the height.
rect :: (ProcMonad m, Drawing c)
     => Proc_Point -- ^ Location of the rectangle.
     -> Proc_Float -- ^ Width of the rectangle.
     -> Proc_Float -- ^ Height of the rectangle.
     -> m c ()
rect (x,y) w h = commandM "rect" $ fmap proc_arg [x,y,w,h]

-- | A triangle is a plane created by connecting three points.
triangle :: (ProcMonad m, Drawing c)
         => Proc_Point -> Proc_Point -> Proc_Point
         -> m c ()
triangle (x0,y0) (x1,y1) (x2,y2) =
  commandM "triangle" $ fmap proc_arg [x0,y0,x1,y1,x2,y2]

-- | Bézier curve.
bezier :: (ProcMonad m, Drawing c)
       => Proc_Point -- ^ Initial point.
       -> Proc_Point -- ^ First control point.
       -> Proc_Point -- ^ Second control point.
       -> Proc_Point -- ^ End point.
       -> m c ()
bezier (x0,y0) (x1,y1) (x2,y2) (x3,y3) =
  commandM "bezier" $ fmap proc_arg [x0,y0,x1,y1,x2,y2,x3,y3]

-- | Begin shape command. Not exported.
beginShape :: (ProcMonad m, Drawing c) => m c ()
beginShape = commandM "beginShape" []

-- | End shape command. Not exported.
endShape :: (ProcMonad m, Drawing c) => m c ()
endShape = commandM "endShape" []

-- | Vertex command. Not exported.
vertex :: (ProcMonad m, Drawing c) => Proc_Point -> m c ()
vertex (x,y) = commandM "vertex" [proc_arg x,proc_arg y]

-- | Polygon drawer.
polygon :: (ProcMonad m, Monad (m c), Drawing c) => [Proc_Point] -> m c ()
polygon ps = beginShape >> mapM_ vertex ps >> endShape

---- TEXT

-- | Display a text in the screen.
--   The color is specified by 'fill'.
drawtext :: (ProcMonad m, Drawing c)
         => Proc_Text  -- ^ Text to draw.
         -> Proc_Point -- ^ Position.
         -> Proc_Float -- ^ Width.
         -> Proc_Float -- ^ Height.
         -> m c ()
drawtext t (x,y) w h =
  commandM "text" $ [ proc_arg t
                    , proc_arg x, proc_arg y
                    , proc_arg w, proc_arg h
                      ]

---- TRANSFORMATIONS

-- | Apply a rotation to the following pictures, centered
--   at the current position.
rotate :: (ProcMonad m, Drawing c) => Proc_Float -> m c ()
rotate a = commandM "rotate" [ proc_arg a ]

-- | Apply a scaling to the following pictures, centered
--   at the current position.
scale :: (ProcMonad m, Drawing c)
      => Proc_Float -- ^ Horizontal scaling.
      -> Proc_Float -- ^ Vertical scaling.
      -> m c ()
scale x y = commandM "scale" [ proc_arg x, proc_arg y ]

-- | Move the current position.
translate :: (ProcMonad m, Drawing c)
          => Proc_Float -- ^ Horizontal displacement.
          -> Proc_Float -- ^ Vertical displacement.
          -> m c ()
translate x y = commandM "translate" [ proc_arg x, proc_arg y ]

-- | Reset the transformation matrix.
resetMatrix :: (ProcMonad m, Drawing c)
            => m c ()
resetMatrix = commandM "resetMatrix" []

---- MOUSE

-- | Get the current position of the mouse pointer.
getMousePoint :: (ProcMonad m, Monad (m c)) => m c Proc_Point
getMousePoint = do
 x <- readVar $ varFromText "mouseX"
 y <- readVar $ varFromText "mouseY"
 return (x,y)

---- KEYBOARD

data Key =
    CharKey Char
  | ArrowKey ArrowKey
  | SpecialKey SpecialKey
  | ModKey KeyModifier Key

data ArrowKey = UP | DOWN | LEFT | RIGHT

data KeyModifier = ALT | CONTROL | SHIFT

data SpecialKey =
    BACKSPACE
  | TAB
  | ENTER
  | RETURN
  | ESC

---- SETUP

-- | Set the size of the canvas.
size :: ProcMonad m => Proc_Int -> Proc_Int -> m c ()
size w h = commandM "size" [proc_arg w, proc_arg h]

-- | Specify the number of frames to be displayed every second.
--   The default rate is 60 frames per second.
setFrameRate :: ProcMonad m => Proc_Int -> m Setup ()
setFrameRate r = commandM "frameRate" [proc_arg r]
