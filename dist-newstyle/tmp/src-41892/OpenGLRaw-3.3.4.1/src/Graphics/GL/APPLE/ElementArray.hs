{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.ElementArray
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.ElementArray (
  -- * Extension Support
  glGetAPPLEElementArray,
  gl_APPLE_element_array,
  -- * Enums
  pattern GL_ELEMENT_ARRAY_APPLE,
  pattern GL_ELEMENT_ARRAY_POINTER_APPLE,
  pattern GL_ELEMENT_ARRAY_TYPE_APPLE,
  -- * Functions
  glDrawElementArrayAPPLE,
  glDrawRangeElementArrayAPPLE,
  glElementPointerAPPLE,
  glMultiDrawElementArrayAPPLE,
  glMultiDrawRangeElementArrayAPPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
