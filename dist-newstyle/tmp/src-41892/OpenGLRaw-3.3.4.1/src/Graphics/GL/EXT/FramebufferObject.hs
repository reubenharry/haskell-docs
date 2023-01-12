{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.FramebufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.FramebufferObject (
  -- * Extension Support
  glGetEXTFramebufferObject,
  gl_EXT_framebuffer_object,
  -- * Enums
  pattern GL_COLOR_ATTACHMENT0_EXT,
  pattern GL_COLOR_ATTACHMENT10_EXT,
  pattern GL_COLOR_ATTACHMENT11_EXT,
  pattern GL_COLOR_ATTACHMENT12_EXT,
  pattern GL_COLOR_ATTACHMENT13_EXT,
  pattern GL_COLOR_ATTACHMENT14_EXT,
  pattern GL_COLOR_ATTACHMENT15_EXT,
  pattern GL_COLOR_ATTACHMENT1_EXT,
  pattern GL_COLOR_ATTACHMENT2_EXT,
  pattern GL_COLOR_ATTACHMENT3_EXT,
  pattern GL_COLOR_ATTACHMENT4_EXT,
  pattern GL_COLOR_ATTACHMENT5_EXT,
  pattern GL_COLOR_ATTACHMENT6_EXT,
  pattern GL_COLOR_ATTACHMENT7_EXT,
  pattern GL_COLOR_ATTACHMENT8_EXT,
  pattern GL_COLOR_ATTACHMENT9_EXT,
  pattern GL_DEPTH_ATTACHMENT_EXT,
  pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT,
  pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT,
  pattern GL_FRAMEBUFFER_BINDING_EXT,
  pattern GL_FRAMEBUFFER_COMPLETE_EXT,
  pattern GL_FRAMEBUFFER_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT,
  pattern GL_FRAMEBUFFER_UNSUPPORTED_EXT,
  pattern GL_INVALID_FRAMEBUFFER_OPERATION_EXT,
  pattern GL_MAX_COLOR_ATTACHMENTS_EXT,
  pattern GL_MAX_RENDERBUFFER_SIZE_EXT,
  pattern GL_RENDERBUFFER_ALPHA_SIZE_EXT,
  pattern GL_RENDERBUFFER_BINDING_EXT,
  pattern GL_RENDERBUFFER_BLUE_SIZE_EXT,
  pattern GL_RENDERBUFFER_DEPTH_SIZE_EXT,
  pattern GL_RENDERBUFFER_EXT,
  pattern GL_RENDERBUFFER_GREEN_SIZE_EXT,
  pattern GL_RENDERBUFFER_HEIGHT_EXT,
  pattern GL_RENDERBUFFER_INTERNAL_FORMAT_EXT,
  pattern GL_RENDERBUFFER_RED_SIZE_EXT,
  pattern GL_RENDERBUFFER_STENCIL_SIZE_EXT,
  pattern GL_RENDERBUFFER_WIDTH_EXT,
  pattern GL_STENCIL_ATTACHMENT_EXT,
  pattern GL_STENCIL_INDEX16_EXT,
  pattern GL_STENCIL_INDEX1_EXT,
  pattern GL_STENCIL_INDEX4_EXT,
  pattern GL_STENCIL_INDEX8_EXT,
  -- * Functions
  glBindFramebufferEXT,
  glBindRenderbufferEXT,
  glCheckFramebufferStatusEXT,
  glDeleteFramebuffersEXT,
  glDeleteRenderbuffersEXT,
  glFramebufferRenderbufferEXT,
  glFramebufferTexture1DEXT,
  glFramebufferTexture2DEXT,
  glFramebufferTexture3DEXT,
  glGenFramebuffersEXT,
  glGenRenderbuffersEXT,
  glGenerateMipmapEXT,
  glGetFramebufferAttachmentParameterivEXT,
  glGetRenderbufferParameterivEXT,
  glIsFramebufferEXT,
  glIsRenderbufferEXT,
  glRenderbufferStorageEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
