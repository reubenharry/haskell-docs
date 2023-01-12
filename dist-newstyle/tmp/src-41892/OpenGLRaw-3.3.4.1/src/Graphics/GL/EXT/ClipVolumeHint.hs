{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ClipVolumeHint
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ClipVolumeHint (
  -- * Extension Support
  glGetEXTClipVolumeHint,
  gl_EXT_clip_volume_hint,
  -- * Enums
  pattern GL_CLIP_VOLUME_CLIPPING_HINT_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
