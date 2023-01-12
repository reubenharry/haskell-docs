--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.OES.SinglePrecision
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.OES.SinglePrecision (
  -- * Extension Support
  glGetOESSinglePrecision,
  gl_OES_single_precision,
  -- * Functions
  glClearDepthfOES,
  glClipPlanefOES,
  glDepthRangefOES,
  glFrustumfOES,
  glGetClipPlanefOES,
  glOrthofOES
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
