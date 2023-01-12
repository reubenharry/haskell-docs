{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.QueryResource
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.QueryResource (
  -- * Extension Support
  glGetNVQueryResource,
  gl_NV_query_resource,
  -- * Enums
  pattern GL_QUERY_RESOURCE_BUFFEROBJECT_NV,
  pattern GL_QUERY_RESOURCE_MEMTYPE_VIDMEM_NV,
  pattern GL_QUERY_RESOURCE_RENDERBUFFER_NV,
  pattern GL_QUERY_RESOURCE_SYS_RESERVED_NV,
  pattern GL_QUERY_RESOURCE_TEXTURE_NV,
  pattern GL_QUERY_RESOURCE_TYPE_VIDMEM_ALLOC_NV,
  -- * Functions
  glQueryResourceNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
