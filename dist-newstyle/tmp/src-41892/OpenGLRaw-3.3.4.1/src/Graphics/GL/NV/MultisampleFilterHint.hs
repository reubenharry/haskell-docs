{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.MultisampleFilterHint
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.MultisampleFilterHint (
  -- * Extension Support
  glGetNVMultisampleFilterHint,
  gl_NV_multisample_filter_hint,
  -- * Enums
  pattern GL_MULTISAMPLE_FILTER_HINT_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
