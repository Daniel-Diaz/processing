
{-# LANGUAGE OverloadedStrings #-}

import Graphics.Web.Processing.Simple

main :: IO ()
main = renderFile "twist.pde" theScript

theScript :: ProcScript
theScript = animateFigure Nothing Nothing 50 (Color 0 0 0 255) drawFunction

drawFunction :: Proc_Int -> Figure
drawFunction n =
  let t = intToFloat n * 0.03
      r = 100
  in  Circle (r * sin t, r * cos t) 20
