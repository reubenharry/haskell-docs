{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.IndexMaterial
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.IndexMaterial (
  -- * Extension Support
  glGetEXTIndexMaterial,
  gl_EXT_index_material,
  -- * Enums
  pattern GL_INDEX_MATERIAL_EXT,
  pattern GL_INDEX_MATERIAL_FACE_EXT,
  pattern GL_INDEX_MATERIAL_PARAMETER_EXT,
  -- * Functions
  glIndexMaterialEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
