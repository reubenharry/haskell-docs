--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing KHR extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR (
  module Graphics.GL.KHR.BlendEquationAdvanced,
  module Graphics.GL.KHR.BlendEquationAdvancedCoherent,
  module Graphics.GL.KHR.ContextFlushControl,
  module Graphics.GL.KHR.DebugCompatibility,
  module Graphics.GL.KHR.DebugCore,
  module Graphics.GL.KHR.NoError,
  module Graphics.GL.KHR.ParallelShaderCompile,
  module Graphics.GL.KHR.Robustness,
  module Graphics.GL.KHR.ShaderSubgroup,
  module Graphics.GL.KHR.TextureCompressionASTCHDR,
  module Graphics.GL.KHR.TextureCompressionASTCLDR
) where

import Graphics.GL.KHR.BlendEquationAdvanced
import Graphics.GL.KHR.BlendEquationAdvancedCoherent
import Graphics.GL.KHR.ContextFlushControl
import Graphics.GL.KHR.DebugCompatibility
import Graphics.GL.KHR.DebugCore
import Graphics.GL.KHR.NoError
import Graphics.GL.KHR.ParallelShaderCompile
import Graphics.GL.KHR.Robustness
import Graphics.GL.KHR.ShaderSubgroup
import Graphics.GL.KHR.TextureCompressionASTCHDR
import Graphics.GL.KHR.TextureCompressionASTCLDR
