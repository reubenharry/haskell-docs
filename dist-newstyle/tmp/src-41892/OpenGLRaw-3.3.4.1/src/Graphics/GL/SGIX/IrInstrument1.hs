{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.IrInstrument1
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.IrInstrument1 (
  -- * Extension Support
  glGetSGIXIrInstrument1,
  gl_SGIX_ir_instrument1,
  -- * Enums
  pattern GL_IR_INSTRUMENT1_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
