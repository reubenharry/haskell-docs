--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ColorSubtable
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ColorSubtable (
  -- * Extension Support
  glGetEXTColorSubtable,
  gl_EXT_color_subtable,
  -- * Functions
  glColorSubTableEXT,
  glCopyColorSubTableEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
