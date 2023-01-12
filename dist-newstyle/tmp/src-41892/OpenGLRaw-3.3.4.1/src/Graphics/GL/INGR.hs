--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INGR
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing INGR extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.INGR (
  module Graphics.GL.INGR.BlendFuncSeparate,
  module Graphics.GL.INGR.ColorClamp,
  module Graphics.GL.INGR.InterlaceRead
) where

import Graphics.GL.INGR.BlendFuncSeparate
import Graphics.GL.INGR.ColorClamp
import Graphics.GL.INGR.InterlaceRead
