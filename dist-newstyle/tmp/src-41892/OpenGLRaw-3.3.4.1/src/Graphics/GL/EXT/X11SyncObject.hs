{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.X11SyncObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.X11SyncObject (
  -- * Extension Support
  glGetEXTX11SyncObject,
  gl_EXT_x11_sync_object,
  -- * Enums
  pattern GL_SYNC_X11_FENCE_EXT,
  -- * Functions
  glImportSyncEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
