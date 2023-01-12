{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureMirrorClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureMirrorClamp (
  -- * Extension Support
  glGetEXTTextureMirrorClamp,
  gl_EXT_texture_mirror_clamp,
  -- * Enums
  pattern GL_MIRROR_CLAMP_EXT,
  pattern GL_MIRROR_CLAMP_TO_BORDER_EXT,
  pattern GL_MIRROR_CLAMP_TO_EDGE_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
