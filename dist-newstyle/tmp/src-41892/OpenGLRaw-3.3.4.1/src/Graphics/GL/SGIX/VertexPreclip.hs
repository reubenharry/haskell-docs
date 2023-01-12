{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.VertexPreclip
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.VertexPreclip (
  -- * Extension Support
  glGetSGIXVertexPreclip,
  gl_SGIX_vertex_preclip,
  -- * Enums
  pattern GL_VERTEX_PRECLIP_HINT_SGIX,
  pattern GL_VERTEX_PRECLIP_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
