{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TexturePerturbNormal
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TexturePerturbNormal (
  -- * Extension Support
  glGetEXTTexturePerturbNormal,
  gl_EXT_texture_perturb_normal,
  -- * Enums
  pattern GL_PERTURB_EXT,
  pattern GL_TEXTURE_NORMAL_EXT,
  -- * Functions
  glTextureNormalEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
