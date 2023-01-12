--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.VertexAttribArrayObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.VertexAttribArrayObject (
  -- * Extension Support
  glGetATIVertexAttribArrayObject,
  gl_ATI_vertex_attrib_array_object,
  -- * Functions
  glGetVertexAttribArrayObjectfvATI,
  glGetVertexAttribArrayObjectivATI,
  glVertexAttribArrayObjectATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
