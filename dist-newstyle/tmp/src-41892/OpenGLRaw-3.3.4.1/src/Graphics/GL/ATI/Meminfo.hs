{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.Meminfo
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.Meminfo (
  -- * Extension Support
  glGetATIMeminfo,
  gl_ATI_meminfo,
  -- * Enums
  pattern GL_RENDERBUFFER_FREE_MEMORY_ATI,
  pattern GL_TEXTURE_FREE_MEMORY_ATI,
  pattern GL_VBO_FREE_MEMORY_ATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
