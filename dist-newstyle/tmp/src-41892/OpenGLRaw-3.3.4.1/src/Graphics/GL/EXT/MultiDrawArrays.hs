--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.MultiDrawArrays
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.MultiDrawArrays (
  -- * Extension Support
  glGetEXTMultiDrawArrays,
  gl_EXT_multi_draw_arrays,
  -- * Functions
  glMultiDrawArraysEXT,
  glMultiDrawElementsEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
