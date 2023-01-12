{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.PinnedMemory
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.PinnedMemory (
  -- * Extension Support
  glGetAMDPinnedMemory,
  gl_AMD_pinned_memory,
  -- * Enums
  pattern GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
