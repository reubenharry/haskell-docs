--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.DebugMarker
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.DebugMarker (
  -- * Extension Support
  glGetEXTDebugMarker,
  gl_EXT_debug_marker,
  -- * Functions
  glInsertEventMarkerEXT,
  glPopGroupMarkerEXT,
  glPushGroupMarkerEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
