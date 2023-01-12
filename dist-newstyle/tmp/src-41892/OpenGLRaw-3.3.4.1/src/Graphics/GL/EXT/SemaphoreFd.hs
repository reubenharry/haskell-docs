{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.SemaphoreFd
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.SemaphoreFd (
  -- * Extension Support
  glGetEXTSemaphoreFd,
  gl_EXT_semaphore_fd,
  -- * Enums
  pattern GL_HANDLE_TYPE_OPAQUE_FD_EXT,
  -- * Functions
  glImportSemaphoreFdEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
