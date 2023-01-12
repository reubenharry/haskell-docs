{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.ClientStorage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.ClientStorage (
  -- * Extension Support
  glGetAPPLEClientStorage,
  gl_APPLE_client_storage,
  -- * Enums
  pattern GL_UNPACK_CLIENT_STORAGE_APPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
