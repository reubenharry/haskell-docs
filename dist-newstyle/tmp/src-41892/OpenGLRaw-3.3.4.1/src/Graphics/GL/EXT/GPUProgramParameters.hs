--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.GPUProgramParameters
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.GPUProgramParameters (
  -- * Extension Support
  glGetEXTGPUProgramParameters,
  gl_EXT_gpu_program_parameters,
  -- * Functions
  glProgramEnvParameters4fvEXT,
  glProgramLocalParameters4fvEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
