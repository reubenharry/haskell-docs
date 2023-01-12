--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.CopyImage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.CopyImage (
  -- * Extension Support
  glGetARBCopyImage,
  gl_ARB_copy_image,
  -- * Functions
  glCopyImageSubData
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
