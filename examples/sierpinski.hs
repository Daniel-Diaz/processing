
{- Sierpinski triangle with colors -}

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html

main :: IO ()
main = writeHtml "processing.js" "sierpinski.pde" "Sierpinski demo" "sierpinski.html" theScript

theScript :: ProcScript
theScript = displayFigure Nothing Nothing (Color 0 0 0 255) $
 LineColor (Color 255 255 255 255) $ sierpinski 1 (0,sizefactor*(sqrt 3/4)) $ Color 0 0 0 255

iterlevel :: Int
iterlevel = 7

sizefactor :: Proc_Float
sizefactor = 520

scale :: Proc_Float -> Proc_Point -> Proc_Point
scale k (x,y) = (k*x,k*y)

(<+>) :: Proc_Point -> Proc_Point -> Proc_Point
(a,b) <+> (c,d) = (a+c,b+d)

(<->) :: Proc_Point -> Proc_Point -> Proc_Point
(a,b) <-> (c,d) = (a-c,b-d)

redColor :: Color -> Color
redColor (Color r g b a) = Color (div (255+r) 2) g b a

greenColor :: Color -> Color
greenColor (Color r g b a) = Color r (div (255+g) 2) b a

blueColor :: Color -> Color
blueColor (Color r g b a) = Color r g (div (255+b) 2) a

sierpinski :: Int        -- Recursive level
           -> Proc_Point -- Base point
           -> Color      -- Base color
           -> Figure
sierpinski i p c =
  let q  = recip $ 2 ^ i
      l  = sizefactor * q
      a1 = 5*pi/3
      p1 = p <+> scale l (cos a1,sin a1)
      a2 = 4*pi/3
      p2 = p <+> scale l (cos a2,sin a2)
  in  if i == iterlevel
         then LineColor c $ FillColor c $ Polygon [ p
                      , p <+> scale 2 (p1 <-> p)
                      , p <+> scale 2 (p2 <-> p)
                        ]
         else sierpinski (i+1) p  (redColor c)
           <> sierpinski (i+1) p1 (greenColor c)
           <> sierpinski (i+1) p2 (blueColor c)
