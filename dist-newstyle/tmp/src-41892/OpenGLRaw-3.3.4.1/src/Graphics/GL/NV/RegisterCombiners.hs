{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.RegisterCombiners
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.RegisterCombiners (
  -- * Extension Support
  glGetNVRegisterCombiners,
  gl_NV_register_combiners,
  -- * Enums
  pattern GL_BIAS_BY_NEGATIVE_ONE_HALF_NV,
  pattern GL_COLOR_SUM_CLAMP_NV,
  pattern GL_COMBINER0_NV,
  pattern GL_COMBINER1_NV,
  pattern GL_COMBINER2_NV,
  pattern GL_COMBINER3_NV,
  pattern GL_COMBINER4_NV,
  pattern GL_COMBINER5_NV,
  pattern GL_COMBINER6_NV,
  pattern GL_COMBINER7_NV,
  pattern GL_COMBINER_AB_DOT_PRODUCT_NV,
  pattern GL_COMBINER_AB_OUTPUT_NV,
  pattern GL_COMBINER_BIAS_NV,
  pattern GL_COMBINER_CD_DOT_PRODUCT_NV,
  pattern GL_COMBINER_CD_OUTPUT_NV,
  pattern GL_COMBINER_COMPONENT_USAGE_NV,
  pattern GL_COMBINER_INPUT_NV,
  pattern GL_COMBINER_MAPPING_NV,
  pattern GL_COMBINER_MUX_SUM_NV,
  pattern GL_COMBINER_SCALE_NV,
  pattern GL_COMBINER_SUM_OUTPUT_NV,
  pattern GL_CONSTANT_COLOR0_NV,
  pattern GL_CONSTANT_COLOR1_NV,
  pattern GL_DISCARD_NV,
  pattern GL_EXPAND_NEGATE_NV,
  pattern GL_EXPAND_NORMAL_NV,
  pattern GL_E_TIMES_F_NV,
  pattern GL_FOG,
  pattern GL_HALF_BIAS_NEGATE_NV,
  pattern GL_HALF_BIAS_NORMAL_NV,
  pattern GL_MAX_GENERAL_COMBINERS_NV,
  pattern GL_NONE,
  pattern GL_NUM_GENERAL_COMBINERS_NV,
  pattern GL_PRIMARY_COLOR_NV,
  pattern GL_REGISTER_COMBINERS_NV,
  pattern GL_SCALE_BY_FOUR_NV,
  pattern GL_SCALE_BY_ONE_HALF_NV,
  pattern GL_SCALE_BY_TWO_NV,
  pattern GL_SECONDARY_COLOR_NV,
  pattern GL_SIGNED_IDENTITY_NV,
  pattern GL_SIGNED_NEGATE_NV,
  pattern GL_SPARE0_NV,
  pattern GL_SPARE0_PLUS_SECONDARY_COLOR_NV,
  pattern GL_SPARE1_NV,
  pattern GL_TEXTURE0_ARB,
  pattern GL_TEXTURE1_ARB,
  pattern GL_UNSIGNED_IDENTITY_NV,
  pattern GL_UNSIGNED_INVERT_NV,
  pattern GL_VARIABLE_A_NV,
  pattern GL_VARIABLE_B_NV,
  pattern GL_VARIABLE_C_NV,
  pattern GL_VARIABLE_D_NV,
  pattern GL_VARIABLE_E_NV,
  pattern GL_VARIABLE_F_NV,
  pattern GL_VARIABLE_G_NV,
  pattern GL_ZERO,
  -- * Functions
  glCombinerInputNV,
  glCombinerOutputNV,
  glCombinerParameterfNV,
  glCombinerParameterfvNV,
  glCombinerParameteriNV,
  glCombinerParameterivNV,
  glFinalCombinerInputNV,
  glGetCombinerInputParameterfvNV,
  glGetCombinerInputParameterivNV,
  glGetCombinerOutputParameterfvNV,
  glGetCombinerOutputParameterivNV,
  glGetFinalCombinerInputParameterfvNV,
  glGetFinalCombinerInputParameterivNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
