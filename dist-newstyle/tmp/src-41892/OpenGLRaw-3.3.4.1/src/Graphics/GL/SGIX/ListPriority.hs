{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.ListPriority
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.ListPriority (
  -- * Extension Support
  glGetSGIXListPriority,
  gl_SGIX_list_priority,
  -- * Enums
  pattern GL_LIST_PRIORITY_SGIX,
  -- * Functions
  glGetListParameterfvSGIX,
  glGetListParameterivSGIX,
  glListParameterfSGIX,
  glListParameterfvSGIX,
  glListParameteriSGIX,
  glListParameterivSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
