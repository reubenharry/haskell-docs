{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.IndirectParameters
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.IndirectParameters (
  -- * Extension Support
  glGetARBIndirectParameters,
  gl_ARB_indirect_parameters,
  -- * Enums
  pattern GL_PARAMETER_BUFFER_ARB,
  pattern GL_PARAMETER_BUFFER_BINDING_ARB,
  -- * Functions
  glMultiDrawArraysIndirectCountARB,
  glMultiDrawElementsIndirectCountARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
