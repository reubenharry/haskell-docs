--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.MapObjectBuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.MapObjectBuffer (
  -- * Extension Support
  glGetATIMapObjectBuffer,
  gl_ATI_map_object_buffer,
  -- * Functions
  glMapObjectBufferATI,
  glUnmapObjectBufferATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
