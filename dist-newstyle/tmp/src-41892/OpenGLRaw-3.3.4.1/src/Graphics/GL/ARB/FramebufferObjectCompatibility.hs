{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.FramebufferObjectCompatibility
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.FramebufferObjectCompatibility (
  -- * Extension Support
  glGetARBFramebufferObject,
  gl_ARB_framebuffer_object,
  -- * Enums
  pattern GL_COLOR_ATTACHMENT0,
  pattern GL_COLOR_ATTACHMENT1,
  pattern GL_COLOR_ATTACHMENT10,
  pattern GL_COLOR_ATTACHMENT11,
  pattern GL_COLOR_ATTACHMENT12,
  pattern GL_COLOR_ATTACHMENT13,
  pattern GL_COLOR_ATTACHMENT14,
  pattern GL_COLOR_ATTACHMENT15,
  pattern GL_COLOR_ATTACHMENT2,
  pattern GL_COLOR_ATTACHMENT3,
  pattern GL_COLOR_ATTACHMENT4,
  pattern GL_COLOR_ATTACHMENT5,
  pattern GL_COLOR_ATTACHMENT6,
  pattern GL_COLOR_ATTACHMENT7,
  pattern GL_COLOR_ATTACHMENT8,
  pattern GL_COLOR_ATTACHMENT9,
  pattern GL_DEPTH24_STENCIL8,
  pattern GL_DEPTH_ATTACHMENT,
  pattern GL_DEPTH_STENCIL,
  pattern GL_DEPTH_STENCIL_ATTACHMENT,
  pattern GL_DRAW_FRAMEBUFFER,
  pattern GL_DRAW_FRAMEBUFFER_BINDING,
  pattern GL_FRAMEBUFFER,
  pattern GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING,
  pattern GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
  pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL,
  pattern GL_FRAMEBUFFER_BINDING,
  pattern GL_FRAMEBUFFER_COMPLETE,
  pattern GL_FRAMEBUFFER_DEFAULT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER,
  pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE,
  pattern GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER,
  pattern GL_FRAMEBUFFER_UNDEFINED,
  pattern GL_FRAMEBUFFER_UNSUPPORTED,
  pattern GL_INDEX,
  pattern GL_INVALID_FRAMEBUFFER_OPERATION,
  pattern GL_MAX_COLOR_ATTACHMENTS,
  pattern GL_MAX_RENDERBUFFER_SIZE,
  pattern GL_MAX_SAMPLES,
  pattern GL_READ_FRAMEBUFFER,
  pattern GL_READ_FRAMEBUFFER_BINDING,
  pattern GL_RENDERBUFFER,
  pattern GL_RENDERBUFFER_ALPHA_SIZE,
  pattern GL_RENDERBUFFER_BINDING,
  pattern GL_RENDERBUFFER_BLUE_SIZE,
  pattern GL_RENDERBUFFER_DEPTH_SIZE,
  pattern GL_RENDERBUFFER_GREEN_SIZE,
  pattern GL_RENDERBUFFER_HEIGHT,
  pattern GL_RENDERBUFFER_INTERNAL_FORMAT,
  pattern GL_RENDERBUFFER_RED_SIZE,
  pattern GL_RENDERBUFFER_SAMPLES,
  pattern GL_RENDERBUFFER_STENCIL_SIZE,
  pattern GL_RENDERBUFFER_WIDTH,
  pattern GL_STENCIL_ATTACHMENT,
  pattern GL_STENCIL_INDEX1,
  pattern GL_STENCIL_INDEX16,
  pattern GL_STENCIL_INDEX4,
  pattern GL_STENCIL_INDEX8,
  pattern GL_TEXTURE_STENCIL_SIZE,
  pattern GL_UNSIGNED_INT_24_8,
  pattern GL_UNSIGNED_NORMALIZED,
  -- * Functions
  glBindFramebuffer,
  glBindRenderbuffer,
  glBlitFramebuffer,
  glCheckFramebufferStatus,
  glDeleteFramebuffers,
  glDeleteRenderbuffers,
  glFramebufferRenderbuffer,
  glFramebufferTexture1D,
  glFramebufferTexture2D,
  glFramebufferTexture3D,
  glFramebufferTextureLayer,
  glGenFramebuffers,
  glGenRenderbuffers,
  glGenerateMipmap,
  glGetFramebufferAttachmentParameteriv,
  glGetRenderbufferParameteriv,
  glIsFramebuffer,
  glIsRenderbuffer,
  glRenderbufferStorage,
  glRenderbufferStorageMultisample
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
