
{- Keyboard interaction demo

One of the squares in a 3x3 matrix of squares
is black and the rest is white. Use WASD keys
to change the position of the black square.

-}

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html

main :: IO ()
main = writeHtml "processing.js" "keydemo.pde" "Keyboard demo" "keydemo.html" theScript

---------------
-- 0 | 1 | 2 --
-- 3 | 4 | 5 --
-- 6 | 7 | 8 --
---------------

-- | State indicates the current position.
type State = Proc_Int

theScript :: ProcScript
theScript =
  interactiveFigure
    Nothing -- Maximum width
    Nothing -- Maximum height
    30      -- Frames per second
    4       -- Initial position
    cubes   -- Drawing function
    (const $ Color 255 255 255 255) -- Constant background
    (const id) -- No step function
    (const id) -- No mouse event
    keyEvents  -- Key events

-- | Width of each cube.
cubew :: Proc_Float
cubew = 40

cubew2 :: Proc_Float
cubew2 = cubew/2

-- | Top-left corner of the 3x3 square.
corner :: Proc_Point
corner = let w = cubew + cubew2
         in  (negate w,w)

-- | Addition of points.
(<+>) :: Proc_Point -> Proc_Point -> Proc_Point
(a,b) <+> (c,d) = (a+c,b+d)

-- | Draw the cube at a certain position.
--   The boolean indicates if it is black or white.
cubeUnit :: Proc_Bool -> Proc_Int -> Figure
cubeUnit b n = 
 let q = if_ b 0 255
     c = Color q q q 255
     (d,m) = divMod n 3
     p = corner <+> (cubew * intToFloat m, negate $ cubew * intToFloat d)
 in  FillColor c $ Rectangle p cubew cubew

-- | Draw all the cubes.
cubes :: State -> Figure
cubes n = mconcat $ fmap (\i -> cubeUnit (n #== i) i) [0 .. 8]

-- | WASD keys to move the black box.
keyEvents :: [(Key,State -> State)]
keyEvents = [
   ( CharKey 'w'
   , (\n -> if_ (n #< 3) n (n - 3))
     )
 , ( CharKey 'a'
   , (\n -> if_ (mod n 3 #== 0) n (n - 1))
     )
 , ( CharKey 's'
   , (\n -> if_ (n #> 5) n (n + 3))
     )
 , ( CharKey 'd'
   , (\n -> if_ (mod n 3 #== 2) n (n + 1))
     )
 ]
