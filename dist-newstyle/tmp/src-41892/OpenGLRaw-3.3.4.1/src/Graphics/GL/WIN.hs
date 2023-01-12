--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.WIN
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing WIN extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.WIN (
  module Graphics.GL.WIN.PhongShading,
  module Graphics.GL.WIN.SpecularFog
) where

import Graphics.GL.WIN.PhongShading
import Graphics.GL.WIN.SpecularFog
