
-- | A 'Monoid' models figures in the plane.
--   Then, figures are displayed or animated using
--   a Processing script.
--
--   For example, this expression represents a circle
--   of radius 10 centered at the origin:
--
-- > Circle (0,0) 10
--
--   The origin will be represented at the center of
--   the screen. As opposed to the other modules,
--   /y/-coordinates increase to the top, while /x/-coordinates
--   still increase to the right.
--
--   This is a red rectangle with top-left corner at the origin,
--   10 points height and 10 points width:
--
-- > FillColor (Color 255 0 0 255) $ Rectangle (0,0) 10 10
--
--   To display several figures together, use the 'Monoid' instance:
--
-- > Circle (0,0) 10 <> Circle (0,20) 10
--
--   If you just want to display this figure in the target canvas,
--   use 'displayFigure'. If you want to animate it, use 'animateFigure'.
--   Animations depend on the number of frames since the beginning of
--   the execution, instead of in the time spent.
--
--   Once you have created a processing script (a value of type
--   'ProcScript'), use 'renderFile' to write it to a file. See
--   also the "Graphics.Web.Processing.Html" module.
--
--   The default filling color and line color are white and black
--   respectively. Use 'FillColor' and 'LineColor' to change these
--   colors. 'Color's are in RGBA format, meaning that they may be
--   transparent (with an alpha value of 0), opaque (with an alpha
--   value of 255) or something in between. Use a fully transparent
--   color to indicate that a Figure should not be filled.
--
--   You can apply transformations like translation, rotation and
--   scaling. If @p@ is a point and @f@ a figure, @Translate p f@
--   will draw @f@ with @p@ as the origin of coordinates. Rotations
--   and scalings are always done in respect to the origin, but note
--   that you can modify where the origin is using 'Translate'.
module Graphics.Web.Processing.Simple (
     -- * Types
     module Graphics.Web.Processing.Core.Types
   , Color (..)
   , Proc_Point
   , Path
     -- * Figure type
   , Figure (..)
     -- * Monoids
     -- | A re-export of the "Data.Monoid" module is provided.
     --   You may be using it to join different 'Figure's into one.
   , module Data.Monoid
     -- * Script
   , displayFigure
   , animateFigure
     -- ** Interactive
   , interactiveFigure
     -- | Module re-export for convenience.
   , module Graphics.Web.Processing.Mid.CustomVar
   ) where

import Data.Monoid
import Graphics.Web.Processing.Core.Types
import Graphics.Web.Processing.Mid
import Graphics.Web.Processing.Mid.CustomVar
-- state
import Control.Applicative ((<$>))
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

-- | A path is just a list of points.
type Path = [Proc_Point]

-- | The monoid of plane figures.
data Figure =
   Line Path
   -- ^ Line joining a list of points.
 | Polygon Path
   -- ^ Polygon given a list of vertex.
 | Ellipse Proc_Point Proc_Float Proc_Float
   -- ^ Ellipse centered at the given point,
   --   with width and height also specified.
 | Circle Proc_Point Proc_Float
   -- ^ Circle centered at the given point and with
   --   the specified radius.
 | Arc Proc_Point Proc_Float Proc_Float
                  Proc_Float Proc_Float
   -- ^ Arc. The arc is drawn following the line of
   --   an ellipse between two angles.
   --   The first argument is the center of the ellipse.
   --   The next two arguments are the width and height of
   --   the ellipse.
   --   The last two arguments are the initial and end
   --   angles of the arc.
 | Rectangle Proc_Point Proc_Float Proc_Float
   -- ^ Rectangle such that the top-left corner is
   --   at the specified point, and its width and
   --   height are specified by the other two arguments.
 | Bezier Proc_Point Proc_Point Proc_Point Proc_Point
   -- ^ Bezier curve. First and last arguments are the initial
   --   and end points of the curve. The other points are
   --   control points.
 | Text Proc_Point Proc_Text
   -- ^ Text.
 | LineColor Color Figure
   -- ^ Set the line color of a figure.
 | FillColor Color Figure
   -- ^ Set the filling color of a figure.
 | Translate Proc_Point Figure
   -- ^ Translate a figure in the direction of a vector.
 | Rotate Proc_Float Figure
   -- ^ Rotate a figure by the given angle in radians.
 | Scale Proc_Float Proc_Float Figure
   -- ^ Scale a figure by the given x and y factors.
 | Figures [Figure]
   -- ^ List of figures.

instance Monoid Figure where
 mempty = Figures []
 mappend (Figures []) x = x
 mappend x (Figures []) = x
 mappend (Figures xs) (Figures ys) = Figures $ xs ++ ys
 mappend (Figures xs) x = Figures $ xs ++ [x]
 mappend x (Figures xs) = Figures $ x : xs
 mappend x y = Figures [x,y]

pairList :: [a] -> [(a,a)]
pairList (x:y:zs) = (x,y) : pairList (y:zs)
pairList _ = []

-- | Adjust a point so x coordinates increase
--   to the right and y coordinates to the top.
adjustPoint :: Proc_Point -> Proc_Point
adjustPoint (x,y) = (x,-y)

-- | SimpleEventM monad: EventM with a state attached.
type SimpleEventM c = StateT Settings (EventM c)

data Settings = Settings {
   currentLineColor :: Color
 , currentFillColor :: Color
   }

defaultSettings :: Settings
defaultSettings = Settings {
   currentLineColor = Color   0   0   0 255 -- black
 , currentFillColor = Color 255 255 255 255 -- white
   }

setLineColor :: Color -> SimpleEventM c ()
setLineColor c = modify $ \s -> s { currentLineColor = c }

getLineColor :: SimpleEventM c Color
getLineColor = currentLineColor <$> get

setFillColor :: Color -> SimpleEventM c ()
setFillColor c = modify $ \s -> s { currentFillColor = c }

getFillColor :: SimpleEventM c Color
getFillColor = currentFillColor <$> get

figureSEvent :: Drawing c => Figure -> SimpleEventM c ()
-- Pictures
figureSEvent (Line ps) = lift $ mapM_ (uncurry line) $ pairList $ fmap adjustPoint ps
figureSEvent (Polygon ps) = lift $ polygon $ fmap adjustPoint ps
figureSEvent (Ellipse p w h) = lift $ ellipse (adjustPoint p) w h
figureSEvent (Circle p r) = lift $ circle (adjustPoint p) r
figureSEvent (Arc p w h start end) = lift $ arc (adjustPoint p) w h start end
figureSEvent (Rectangle p w h) = lift $ rect (adjustPoint p) w h
figureSEvent (Bezier start p1 p2 end) =
  lift $ bezier (adjustPoint start)
                (adjustPoint p1)
                (adjustPoint p2)
                (adjustPoint end)
figureSEvent (Text p t) = lift $ drawtext t (adjustPoint p) 0 0 -- font size doesn't work anyway (?)
-- Settings
figureSEvent (LineColor c f) = do
  c0 <- getLineColor
  setLineColor c
  lift $ stroke c
  figureSEvent f
  setLineColor c0
  lift $ stroke c0
figureSEvent (FillColor c f) = do
  c0 <- getFillColor
  setFillColor c
  lift $ fill c
  figureSEvent f
  setFillColor c0
  lift $ fill c0
-- Transformations
figureSEvent (Translate (x,y) f) = lift (translate x (-y)) >> figureSEvent f >> lift (translate (-x) y)
figureSEvent (Rotate a f) = lift (rotate a) >> figureSEvent f >> lift (rotate (-a))
figureSEvent (Scale x y f) = lift (scale x y) >> figureSEvent f >> lift (scale (recip x) (recip y))
-- Appending
figureSEvent (Figures fs) = mapM_ figureSEvent fs

figureEvent :: Drawing c => Figure -> EventM c ()
figureEvent f = do
  stroke $ currentLineColor defaultSettings
  fill   $ currentFillColor defaultSettings
  evalStateT (figureSEvent f) defaultSettings

-- | Display a figure using a Processing script.
displayFigure ::
     Maybe Int -- ^ Width (if none, takes as much as is available).
  -> Maybe Int -- ^ Height (if none, takes as much as is available).
  -> Color -- ^ Background color.
  -> Figure -- ^ Figure to display.
  -> ProcScript
displayFigure w h bgc f = execScriptM $ on Draw $ do
     size (maybe screenWidth fromInt w) (maybe screenHeight fromInt h)
     background bgc
     translate (intToFloat screenWidth/2) (intToFloat screenHeight/2)
     figureEvent f

-- | Create a Processing animation from a 'Figure'-valued function.
animateFigure ::
     Maybe Int -- ^ Width (if none, takes as much as is available).
  -> Maybe Int -- ^ Height (if none, takes as much as is available).
  -> Int -- ^ Frame rate.
  -> Color -- ^ Background color.
  -> (Proc_Int -> Figure) -- ^ Function to produce the next frame of animation,
                          --   given the current frame number.
  -> ProcScript
animateFigure mw mh fr bgc f = execScriptM $ do
  on Setup $ do
     setFrameRate $ fromInt fr
  on Draw $ do
     let w = maybe screenWidth  fromInt mw
         h = maybe screenHeight fromInt mh
     size w h
     background bgc
     translate (intToFloat w/2) (intToFloat h/2)
     frameCount >>= figureEvent . f

-- | Framework to create interactive scripts.
--
--   Note that is required for the state to be an instance of 'CustomValue'.
--   More info on how to instantiate a type in the 'CustomValue' class in the
--   "Graphics.Web.Processing.Mid.CustomVar" module.
interactiveFigure :: CustomValue w
  => Maybe Int -- ^ Width (if none, takes as much as is available).
  -> Maybe Int -- ^ Height (if none, takes as much as is available).
  -> Int -- ^ Frame rate.
  -> w -- ^ Initial state.
  -> (w -> Figure) -- ^ How to print the state.
  -> (w -> Color) -- ^ Background color, depending on the current state.
  -> (Proc_Int -> w -> w) -- ^ Function to step the world one iteration.
                          --   It is passed the number of frames from the
                          --   beginning.
  -> (Proc_Point -> w -> w) -- ^ Function called each time the mouse is clicked.
  -> ProcScript
interactiveFigure mw mh framerate s0 _print bg step onclick = execScriptM $ do
  let w = maybe screenWidth  fromInt mw
      h = maybe screenHeight fromInt mh
  v <- newVarC s0
  on Setup $ do
     setFrameRate $ fromInt framerate
  on Draw $ do
     size w h
     writeComment "Read state"
     s <- readVarC v
     writeComment "Background color"
     background $ bg s
     writeComment "Draw state"
     figureEvent $ _print s
     writeComment $ "Update state"
     n <- frameCount
     writeVarC v $ step n s
  on MouseClicked $ do
     writeComment "Read state"
     s <- readVarC v
     writeComment "Mouse event"
     p <- getMousePoint
     writeVarC v $ onclick p s
