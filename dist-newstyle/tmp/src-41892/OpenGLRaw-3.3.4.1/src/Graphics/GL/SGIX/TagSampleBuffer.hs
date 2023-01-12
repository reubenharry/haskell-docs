--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.TagSampleBuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.TagSampleBuffer (
  -- * Extension Support
  glGetSGIXTagSampleBuffer,
  gl_SGIX_tag_sample_buffer,
  -- * Functions
  glTagSampleBufferSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
