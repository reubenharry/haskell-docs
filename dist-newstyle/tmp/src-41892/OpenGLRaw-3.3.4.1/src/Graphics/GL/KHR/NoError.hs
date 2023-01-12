{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR.NoError
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR.NoError (
  -- * Extension Support
  glGetKHRNoError,
  gl_KHR_no_error,
  -- * Enums
  pattern GL_CONTEXT_FLAG_NO_ERROR_BIT_KHR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
