--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ExternalBuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ExternalBuffer (
  -- * Extension Support
  glGetEXTExternalBuffer,
  gl_EXT_external_buffer,
  -- * Functions
  glBufferStorageExternalEXT,
  glNamedBufferStorageExternalEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
