
{-# LANGUAGE OverloadedStrings #-}

import Graphics.Web.Processing.Baked

main :: IO ()
main = renderFile "twist.pde" theScript

theScript :: ProcScript
theScript = animationScript 0.03 theSetup drawFunction

theSetup :: ProcCode Setup
theSetup = execProcM $ do
  -- Set to the maximum size, filling the available screen.
  size screenWidth screenHeight
  -- Set the filling color to be white.
  fill $ Color 255 255 255 255

drawFunction :: Proc_Float -> ProcM Draw ()
drawFunction t = do
  -- Fill the entire canvas with black.
  background $ Color 0 0 0 255
  -- Draw a circle
  circle (x0 + r * sin t , y0 + r * cos t) 20
 where
  -- Radius of the twist
  r  = 100
  -- Center of the screen
  x0 = intToFloat screenWidth / 2
  y0 = intToFloat screenHeight / 2
