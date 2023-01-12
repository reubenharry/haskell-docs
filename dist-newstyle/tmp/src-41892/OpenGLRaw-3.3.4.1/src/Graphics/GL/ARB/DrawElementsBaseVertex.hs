--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DrawElementsBaseVertex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DrawElementsBaseVertex (
  -- * Extension Support
  glGetARBDrawElementsBaseVertex,
  gl_ARB_draw_elements_base_vertex,
  -- * Functions
  glDrawElementsBaseVertex,
  glDrawElementsInstancedBaseVertex,
  glDrawRangeElementsBaseVertex,
  glMultiDrawElementsBaseVertex
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
