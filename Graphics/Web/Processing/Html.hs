
{-# LANGUAGE OverloadedStrings #-}

-- | Once created, processing scripts can be included in HTML canvas.
--   To be able to reproduce the animation, you must import the /processing.js/
--   library, downloadable from <http://processingjs.org/download> (do not import
--   it from the original link, download it and use your own copy).
--   To import /processing.js/, use a /script/ tag.
--
-- > <script src="processing.js"></script>
--
module Graphics.Web.Processing.Html (
  procCanvas
  ) where

import Graphics.Web.Processing.Core.Types
-- HTML
import Data.Monoid (Monoid (..))
import Data.String
import Text.Blaze.Html5 (Html, canvas, (!), customAttribute)

-- | Create a canvas element which contain a Processing animation.
procCanvas :: FilePath -- ^ File path to the script.
           -> Html
procCanvas fp = canvas ! customAttribute "data-processing-sources" (fromString fp) $ mempty
