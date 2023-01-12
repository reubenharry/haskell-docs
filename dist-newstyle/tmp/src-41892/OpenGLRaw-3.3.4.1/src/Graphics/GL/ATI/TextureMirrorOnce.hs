{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.TextureMirrorOnce
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.TextureMirrorOnce (
  -- * Extension Support
  glGetATITextureMirrorOnce,
  gl_ATI_texture_mirror_once,
  -- * Enums
  pattern GL_MIRROR_CLAMP_ATI,
  pattern GL_MIRROR_CLAMP_TO_EDGE_ATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
