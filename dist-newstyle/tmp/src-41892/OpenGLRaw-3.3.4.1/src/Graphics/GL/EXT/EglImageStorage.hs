--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.EglImageStorage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.EglImageStorage (
  -- * Extension Support
  glGetEXTEglImageStorage,
  gl_EXT_EGL_image_storage,
  -- * Types
  GLeglImageOES,
  -- * Functions
  glEGLImageTargetTexStorageEXT,
  glEGLImageTargetTextureStorageEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Types
import Graphics.GL.Functions
