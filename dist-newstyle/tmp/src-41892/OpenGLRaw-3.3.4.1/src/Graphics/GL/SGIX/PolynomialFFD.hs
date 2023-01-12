{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.PolynomialFFD
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.PolynomialFFD (
  -- * Extension Support
  glGetSGIXPolynomialFFD,
  gl_SGIX_polynomial_ffd,
  -- * Enums
  pattern GL_DEFORMATIONS_MASK_SGIX,
  pattern GL_GEOMETRY_DEFORMATION_BIT_SGIX,
  pattern GL_GEOMETRY_DEFORMATION_SGIX,
  pattern GL_MAX_DEFORMATION_ORDER_SGIX,
  pattern GL_TEXTURE_DEFORMATION_BIT_SGIX,
  pattern GL_TEXTURE_DEFORMATION_SGIX,
  -- * Functions
  glDeformSGIX,
  glDeformationMap3dSGIX,
  glDeformationMap3fSGIX,
  glLoadIdentityDeformationMapSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
