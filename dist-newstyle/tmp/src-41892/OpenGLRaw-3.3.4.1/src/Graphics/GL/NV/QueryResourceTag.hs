--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.QueryResourceTag
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.QueryResourceTag (
  -- * Extension Support
  glGetNVQueryResourceTag,
  gl_NV_query_resource_tag,
  -- * Functions
  glDeleteQueryResourceTagNV,
  glGenQueryResourceTagNV,
  glQueryResourceTagNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
