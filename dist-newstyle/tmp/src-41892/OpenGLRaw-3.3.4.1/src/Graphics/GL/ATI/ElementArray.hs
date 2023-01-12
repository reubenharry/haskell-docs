{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.ElementArray
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.ElementArray (
  -- * Extension Support
  glGetATIElementArray,
  gl_ATI_element_array,
  -- * Enums
  pattern GL_ELEMENT_ARRAY_ATI,
  pattern GL_ELEMENT_ARRAY_POINTER_ATI,
  pattern GL_ELEMENT_ARRAY_TYPE_ATI,
  -- * Functions
  glDrawElementArrayATI,
  glDrawRangeElementArrayATI,
  glElementPointerATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
