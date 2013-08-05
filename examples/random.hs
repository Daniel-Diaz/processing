
{-# LANGUAGE OverloadedStrings #-}

import Graphics.Web.Processing.Mid
import Graphics.Web.Processing.Html
import Control.Applicative ((<$>))

main :: IO ()
main = writeHtml "processing.js" "random.pde" "Random demo" "random.html" randomDemo

randomDemo :: ProcScript
randomDemo = execScriptM $ do
  vi <- newVar 0
  vj <- newVar 0
  rv <- newVar 0
  gv <- newVar 0
  bv <- newVar 0
  on Setup $ do
     size screenWidth screenHeight
     background $ Color 255 255 255 255
     setFrameRate 30
  on Draw $ do
     strokeWeight $ div screenWidth 40
     -- Random color
     random rv 0 50
     r <- pround <$> readVar rv
     random gv 0 255
     g <- pround <$> readVar gv
     random bv 0 255
     b <- pround <$> readVar bv
     stroke $ Color r g b 100
     -- Top position
     i <- readVar vi
     -- Bottom position
     random vj 0 (intToFloat screenWidth)
     j <- readVar vj
     -- Line
     line (intToFloat i,0) (j,intToFloat screenHeight)
     -- Update top position
     ifM (i #>= screenWidth)
         (writeVar vi 0)
         (writeVar vi $ i + div screenWidth 250)
