{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.IBM.StaticData
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.IBM.StaticData (
  -- * Extension Support
  glGetIBMStaticData,
  gl_IBM_static_data,
  -- * Enums
  pattern GL_ALL_STATIC_DATA_IBM,
  pattern GL_STATIC_VERTEX_ARRAY_IBM,
  -- * Functions
  glFlushStaticDataIBM
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
