{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.NameGenDelete
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.NameGenDelete (
  -- * Extension Support
  glGetAMDNameGenDelete,
  gl_AMD_name_gen_delete,
  -- * Enums
  pattern GL_DATA_BUFFER_AMD,
  pattern GL_PERFORMANCE_MONITOR_AMD,
  pattern GL_QUERY_OBJECT_AMD,
  pattern GL_SAMPLER_OBJECT_AMD,
  pattern GL_VERTEX_ARRAY_OBJECT_AMD,
  -- * Functions
  glDeleteNamesAMD,
  glGenNamesAMD,
  glIsNameAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
