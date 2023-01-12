{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.ObjectPurgeable
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.ObjectPurgeable (
  -- * Extension Support
  glGetAPPLEObjectPurgeable,
  gl_APPLE_object_purgeable,
  -- * Enums
  pattern GL_BUFFER_OBJECT_APPLE,
  pattern GL_PURGEABLE_APPLE,
  pattern GL_RELEASED_APPLE,
  pattern GL_RETAINED_APPLE,
  pattern GL_UNDEFINED_APPLE,
  pattern GL_VOLATILE_APPLE,
  -- * Functions
  glGetObjectParameterivAPPLE,
  glObjectPurgeableAPPLE,
  glObjectUnpurgeableAPPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
