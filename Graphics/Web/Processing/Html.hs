
{-# LANGUAGE OverloadedStrings #-}

-- | Once created, processing scripts can be included in HTML canvas.
--   To be able to reproduce the animation, you must import the /processing.js/
--   library, downloadable from <http://processingjs.org/download> (do not import
--   it from the original link, download it and use your own copy).
--   To import /processing.js/, use a @script@ tag.
--
-- > <script src="processing.js"></script>
--
--   See 'importScript'.
--
--   /Note from the author: I didn't manage to run a processing animation locally,/
--   /so you may have the same issue. Once I uploaded them to my server, they worked/
--   /just fine./
module Graphics.Web.Processing.Html (
    procCanvas
  , importScript
  , defaultHtml, writeHtml
  ) where

import Graphics.Web.Processing.Core.Types
-- HTML
import Prelude hiding (head)
import Data.Monoid (Monoid (..))
import Data.String
import Text.Blaze.Html5
  ( Html, docTypeHtml, preEscapedToHtml
  , toHtml
  , head, body, title
  , canvas, (!), customAttribute ,script
  , style)
import Text.Blaze.Html5.Attributes (src,type_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text (Text)
import Data.Text.Lazy.IO (writeFile)
-- System
import System.FilePath
import System.Directory

-- | Create a canvas element which contain a Processing animation.
--   The output is of the following form:
--
-- > <canvas data-processing-sources="specified path"></canvas>
--
procCanvas :: FilePath -- ^ File path to the script.
           -> Html
procCanvas fp = canvas ! customAttribute "data-processing-sources" (fromString fp) $ mempty

-- | Create the following HTML element:
--
-- > <script src="specified path"></script>
--
--   Use it to import the /processing.js/ script,
--   inside the @head@ tag.
importScript :: FilePath -- ^ Location of the /processing.js/ script.
             -> Html
importScript fp = script ! src (fromString fp) $ mempty

-- | Default template for visualizing Processing scripts in HTML.
defaultHtml :: FilePath   -- ^ Where to find the /processing.js/ module.
            -> FilePath   -- ^ File name for the Processing script (with @.pde@ extension).
            -> Text       -- ^ Html title.
            -> Html       -- ^ Html output.
defaultHtml pfp sfp tit = docTypeHtml $ do
  head $ do title $ toHtml tit
            importScript pfp
            style ! type_ "text/css" $
              preEscapedToHtml defaultCSS
  body $ procCanvas sfp

defaultCSS :: Text
defaultCSS = "body {margin: 0 ; overflow:hidden ;} canvas {width: 100% ; outline:none ;}"

-- | Write a Processing script and the HTML default template for it
--   to files, using 'renderFile' and 'defaultHtml'.
--   All the 'FilePath's must be relative to where the HTML file is written.
writeHtml :: FilePath   -- ^ Where to find the /processing.js/ module.
          -> FilePath   -- ^ Where to write the Processing script (with @.pde@ extension).
          -> Text       -- ^ Html title.
          -> FilePath   -- ^ Where to write the HTML file (with @.html@ extension).
          -> ProcScript -- ^ Processing script.
          -> IO ()
writeHtml pfp sfp tit hfp ps = do
  d0 <- getCurrentDirectory
  setCurrentDirectory $ takeDirectory hfp
  renderFile sfp ps
  let fn = takeFileName hfp
  Data.Text.Lazy.IO.writeFile fn (renderHtml $ defaultHtml pfp sfp tit)
  setCurrentDirectory d0
