{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VideoCapture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VideoCapture (
  -- * Extension Support
  glGetNVVideoCapture,
  gl_NV_video_capture,
  -- * Enums
  pattern GL_FAILURE_NV,
  pattern GL_FIELD_LOWER_NV,
  pattern GL_FIELD_UPPER_NV,
  pattern GL_LAST_VIDEO_CAPTURE_STATUS_NV,
  pattern GL_NEXT_VIDEO_CAPTURE_BUFFER_STATUS_NV,
  pattern GL_NUM_VIDEO_CAPTURE_STREAMS_NV,
  pattern GL_PARTIAL_SUCCESS_NV,
  pattern GL_SUCCESS_NV,
  pattern GL_VIDEO_BUFFER_BINDING_NV,
  pattern GL_VIDEO_BUFFER_INTERNAL_FORMAT_NV,
  pattern GL_VIDEO_BUFFER_NV,
  pattern GL_VIDEO_BUFFER_PITCH_NV,
  pattern GL_VIDEO_CAPTURE_FIELD_LOWER_HEIGHT_NV,
  pattern GL_VIDEO_CAPTURE_FIELD_UPPER_HEIGHT_NV,
  pattern GL_VIDEO_CAPTURE_FRAME_HEIGHT_NV,
  pattern GL_VIDEO_CAPTURE_FRAME_WIDTH_NV,
  pattern GL_VIDEO_CAPTURE_SURFACE_ORIGIN_NV,
  pattern GL_VIDEO_CAPTURE_TO_422_SUPPORTED_NV,
  pattern GL_VIDEO_COLOR_CONVERSION_MATRIX_NV,
  pattern GL_VIDEO_COLOR_CONVERSION_MAX_NV,
  pattern GL_VIDEO_COLOR_CONVERSION_MIN_NV,
  pattern GL_VIDEO_COLOR_CONVERSION_OFFSET_NV,
  pattern GL_YCBAYCR8A_4224_NV,
  pattern GL_YCBYCR8_422_NV,
  pattern GL_Z4Y12Z4CB12Z4A12Z4Y12Z4CR12Z4A12_4224_NV,
  pattern GL_Z4Y12Z4CB12Z4CR12_444_NV,
  pattern GL_Z4Y12Z4CB12Z4Y12Z4CR12_422_NV,
  pattern GL_Z6Y10Z6CB10Z6A10Z6Y10Z6CR10Z6A10_4224_NV,
  pattern GL_Z6Y10Z6CB10Z6Y10Z6CR10_422_NV,
  -- * Functions
  glBeginVideoCaptureNV,
  glBindVideoCaptureStreamBufferNV,
  glBindVideoCaptureStreamTextureNV,
  glEndVideoCaptureNV,
  glGetVideoCaptureStreamdvNV,
  glGetVideoCaptureStreamfvNV,
  glGetVideoCaptureStreamivNV,
  glGetVideoCaptureivNV,
  glVideoCaptureNV,
  glVideoCaptureStreamParameterdvNV,
  glVideoCaptureStreamParameterfvNV,
  glVideoCaptureStreamParameterivNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
