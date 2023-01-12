--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.DrawVulkanImage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.DrawVulkanImage (
  -- * Extension Support
  glGetNVDrawVulkanImage,
  gl_NV_draw_vulkan_image,
  -- * Functions
  glDrawVkImageNV,
  glGetVkProcAddrNV,
  glSignalVkFenceNV,
  glSignalVkSemaphoreNV,
  glWaitVkSemaphoreNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
