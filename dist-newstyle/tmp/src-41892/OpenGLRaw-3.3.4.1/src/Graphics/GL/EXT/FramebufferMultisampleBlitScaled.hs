{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.FramebufferMultisampleBlitScaled
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.FramebufferMultisampleBlitScaled (
  -- * Extension Support
  glGetEXTFramebufferMultisampleBlitScaled,
  gl_EXT_framebuffer_multisample_blit_scaled,
  -- * Enums
  pattern GL_SCALED_RESOLVE_FASTEST_EXT,
  pattern GL_SCALED_RESOLVE_NICEST_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
