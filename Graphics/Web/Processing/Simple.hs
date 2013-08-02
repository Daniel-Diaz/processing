
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
--   the screen.
module Graphics.Web.Processing.Simple (
     -- * Types
     module Graphics.Web.Processing.Core.Types
   , Color (..)
   , Proc_Point
   , Path
     -- * Monoid
   , module Data.Monoid
     -- * Figures
   , Figure (..)
   , displayFigure
   , animateFigure
   ) where

import Data.Monoid
import Graphics.Web.Processing.Core.Types
import Graphics.Web.Processing.Mid
-- state
import Control.Applicative ((<$>))
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

-- | A path is just a list of points.
type Path = [Proc_Point]

-- | The monoid of plane figures.
data Figure =
   Line Path
 | Polygon Path
 | Ellipse Proc_Point Proc_Float Proc_Float
 | Circle Proc_Point Proc_Float
 | Arc Proc_Point Proc_Float Proc_Float
                  Proc_Float Proc_Float
 | Rectangle Proc_Point Proc_Float Proc_Float
 | Bezier Proc_Point Proc_Point Proc_Point Proc_Point
 | Text Proc_Point Proc_Text
 | LineColor Color Figure
 | FillColor Color Figure
 | Translate Proc_Point Figure
   -- ^ Translate a figure in the direction of a vector.
 | Rotate Proc_Float Figure
 | Scale Proc_Float Proc_Float Figure
 | Figures [Figure]

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
