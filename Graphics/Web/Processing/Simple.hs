
-- | A 'Monoid' models figures in the plane.
--   Then, figures are displayed or animated using
--   a Processing script.
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

figureEvent :: Drawing c => Figure -> EventM c ()
figureEvent (Line ps) = mapM_ (uncurry line) $ pairList ps
figureEvent (Polygon ps) = polygon ps
figureEvent (Ellipse p w h) = ellipse p w h
figureEvent (Circle p r) = circle p r
figureEvent (Arc p w h start end) = arc p w h start end
figureEvent (Rectangle p w h) = rect p w h
figureEvent (Bezier start p1 p2 end) = bezier start p1 p2 end
figureEvent (Text p t) = drawtext t p 0 0 -- font size doesn't work anyway (?)
figureEvent (LineColor c f) = stroke c >> figureEvent f
figureEvent (FillColor c f) = fill c >> figureEvent f
figureEvent (Translate (x,y) f) = translate x y >> figureEvent f >> translate (-x) (-y)
figureEvent (Rotate a f) = rotate a >> figureEvent f >> rotate (-a)
figureEvent (Scale x y f) = scale x y >> figureEvent f >> scale (recip x) (recip y)
figureEvent (Figures fs) = mapM_ figureEvent fs

-- | Display a figure using a Processing script.
displayFigure ::
     Maybe Int -- ^ Width (if none, takes as much as is available).
  -> Maybe Int -- ^ Height (if none, takes as much as is available).
  -> Color -- ^ Background color.
  -> Figure -- ^ Figure to display.
  -> ProcScript
displayFigure w h bgc f = execScriptM $ do
  on Setup $ do
     size (maybe screenWidth fromInt w) (maybe screenHeight fromInt h)
     background bgc
  on Draw $ do
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
animateFigure w h fr bgc f = execScriptM $ do
  on Setup $ do
     size (maybe screenWidth fromInt w) (maybe screenHeight fromInt h)
     setFrameRate $ fromInt fr
  on Draw $ do
     translate (intToFloat screenWidth/2) (intToFloat screenHeight/2)
     background bgc
     frameCount >>= figureEvent . f
