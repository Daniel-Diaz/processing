
{-# LANGUAGE OverloadedStrings #-}

-- Recursive picture animation.
--
-- Inspired by a similar animation (not equal)
-- to be found in the examples of the gloss library.

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html

main :: IO ()
main = writeHtml "processing.js" "mill.pde" "Mill demo" "mill.html" mill

mill :: ProcScript
mill = animateFigure Nothing Nothing 50 (Color 0 0 0 255) millf

speed :: Proc_Float
speed = 0.02

millf :: Proc_Int -> Figure
millf n =
 let t = intToFloat n * speed
 in  FillColor (Color 0 0 0 0)
   $ LineColor (Color 255 255 255 255)
   $ millUnit 0 250 t

millUnit :: Int -- ^ Recursive level.
         -> Proc_Float -- ^ Radius.
         -> Proc_Float -- ^ Angle.
         -> Figure
-- Recursion stop level.
millUnit 5 _ _ = mempty
-- Recursive call.
millUnit n r alpha =
 let parity = even n
     r2  = r/2
     f a = (r2 * sin a, r2 * cos a)
     p0  = (0,0)
     p1  = f 0
     p2  = f $ 2*pi/3
     p3  = f $ 4*pi/3
     r'  = dist p1 p2 / 2
 in Rotate (if parity then alpha else (-2) * alpha)
     $ mconcat [
         Line [p0,p1] , Line [p0,p2] , Line [p0,p3]
       , Circle p1 r' , Circle p2 r' , Circle p3 r'
       , Translate p1 $ millUnit (n+1) r' alpha
       , Translate p2 $ millUnit (n+1) r' alpha
       , Translate p3 $ millUnit (n+1) r' alpha
         ]

-- | Distance between two points.
dist :: Proc_Point -> Proc_Point -> Proc_Float
dist (a,b) (c,d) = sqrt $ (a - c)^2 + (b - d)^2
