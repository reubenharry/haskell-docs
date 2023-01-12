{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.PGI.MiscHints
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.PGI.MiscHints (
  -- * Extension Support
  glGetPGIMiscHints,
  gl_PGI_misc_hints,
  -- * Enums
  pattern GL_ALLOW_DRAW_FRG_HINT_PGI,
  pattern GL_ALLOW_DRAW_MEM_HINT_PGI,
  pattern GL_ALLOW_DRAW_OBJ_HINT_PGI,
  pattern GL_ALLOW_DRAW_WIN_HINT_PGI,
  pattern GL_ALWAYS_FAST_HINT_PGI,
  pattern GL_ALWAYS_SOFT_HINT_PGI,
  pattern GL_BACK_NORMALS_HINT_PGI,
  pattern GL_CLIP_FAR_HINT_PGI,
  pattern GL_CLIP_NEAR_HINT_PGI,
  pattern GL_CONSERVE_MEMORY_HINT_PGI,
  pattern GL_FULL_STIPPLE_HINT_PGI,
  pattern GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI,
  pattern GL_NATIVE_GRAPHICS_END_HINT_PGI,
  pattern GL_NATIVE_GRAPHICS_HANDLE_PGI,
  pattern GL_PREFER_DOUBLEBUFFER_HINT_PGI,
  pattern GL_RECLAIM_MEMORY_HINT_PGI,
  pattern GL_STRICT_DEPTHFUNC_HINT_PGI,
  pattern GL_STRICT_LIGHTING_HINT_PGI,
  pattern GL_STRICT_SCISSOR_HINT_PGI,
  pattern GL_WIDE_LINE_HINT_PGI,
  -- * Functions
  glHintPGI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
