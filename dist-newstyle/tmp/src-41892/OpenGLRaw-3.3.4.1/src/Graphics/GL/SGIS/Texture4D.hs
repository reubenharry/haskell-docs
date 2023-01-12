{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.Texture4D
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.Texture4D (
  -- * Extension Support
  glGetSGISTexture4D,
  gl_SGIS_texture4D,
  -- * Enums
  pattern GL_MAX_4D_TEXTURE_SIZE_SGIS,
  pattern GL_PACK_IMAGE_DEPTH_SGIS,
  pattern GL_PACK_SKIP_VOLUMES_SGIS,
  pattern GL_PROXY_TEXTURE_4D_SGIS,
  pattern GL_TEXTURE_4DSIZE_SGIS,
  pattern GL_TEXTURE_4D_BINDING_SGIS,
  pattern GL_TEXTURE_4D_SGIS,
  pattern GL_TEXTURE_WRAP_Q_SGIS,
  pattern GL_UNPACK_IMAGE_DEPTH_SGIS,
  pattern GL_UNPACK_SKIP_VOLUMES_SGIS,
  -- * Functions
  glTexImage4DSGIS,
  glTexSubImage4DSGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
