--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.Win32KeyedMutex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.Win32KeyedMutex (
  -- * Extension Support
  glGetEXTWin32KeyedMutex,
  gl_EXT_win32_keyed_mutex,
  -- * Functions
  glAcquireKeyedMutexWin32EXT,
  glReleaseKeyedMutexWin32EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
