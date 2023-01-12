{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.FogFunction
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.FogFunction (
  -- * Extension Support
  glGetSGISFogFunction,
  gl_SGIS_fog_function,
  -- * Enums
  pattern GL_FOG_FUNC_POINTS_SGIS,
  pattern GL_FOG_FUNC_SGIS,
  pattern GL_MAX_FOG_FUNC_POINTS_SGIS,
  -- * Functions
  glFogFuncSGIS,
  glGetFogFuncSGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
