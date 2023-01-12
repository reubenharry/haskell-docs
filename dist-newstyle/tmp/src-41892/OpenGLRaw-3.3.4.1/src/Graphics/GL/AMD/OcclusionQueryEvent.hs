{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.OcclusionQueryEvent
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.OcclusionQueryEvent (
  -- * Extension Support
  glGetAMDOcclusionQueryEvent,
  gl_AMD_occlusion_query_event,
  -- * Enums
  pattern GL_OCCLUSION_QUERY_EVENT_MASK_AMD,
  pattern GL_QUERY_ALL_EVENT_BITS_AMD,
  pattern GL_QUERY_DEPTH_BOUNDS_FAIL_EVENT_BIT_AMD,
  pattern GL_QUERY_DEPTH_FAIL_EVENT_BIT_AMD,
  pattern GL_QUERY_DEPTH_PASS_EVENT_BIT_AMD,
  pattern GL_QUERY_STENCIL_FAIL_EVENT_BIT_AMD,
  -- * Functions
  glQueryObjectParameteruiAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
