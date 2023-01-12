--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INTEL.FramebufferCmaa
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INTEL.FramebufferCmaa (
  -- * Extension Support
  glGetINTELFramebufferCmaa,
  gl_INTEL_framebuffer_CMAA,
  -- * Functions
  glApplyFramebufferAttachmentCMAAINTEL
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
