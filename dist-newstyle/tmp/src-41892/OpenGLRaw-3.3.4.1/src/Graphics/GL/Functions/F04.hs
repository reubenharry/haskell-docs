{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F04
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Raw functions from the
-- <http://www.opengl.org/registry/ OpenGL registry>.
--
--------------------------------------------------------------------------------

module Graphics.GL.Functions.F04 (
  glColorSubTableEXT,
  glColorTable,
  glColorTableEXT,
  glColorTableParameterfv,
  glColorTableParameterfvSGI,
  glColorTableParameteriv,
  glColorTableParameterivSGI,
  glColorTableSGI,
  glCombinerInputNV,
  glCombinerOutputNV,
  glCombinerParameterfNV,
  glCombinerParameterfvNV,
  glCombinerParameteriNV,
  glCombinerParameterivNV,
  glCombinerStageParameterfvNV,
  glCommandListSegmentsNV,
  glCompileCommandListNV,
  glCompileShader,
  glCompileShaderARB,
  glCompileShaderIncludeARB,
  glCompressedMultiTexImage1DEXT,
  glCompressedMultiTexImage2DEXT,
  glCompressedMultiTexImage3DEXT,
  glCompressedMultiTexSubImage1DEXT,
  glCompressedMultiTexSubImage2DEXT,
  glCompressedMultiTexSubImage3DEXT,
  glCompressedTexImage1D,
  glCompressedTexImage1DARB,
  glCompressedTexImage2D,
  glCompressedTexImage2DARB,
  glCompressedTexImage3D,
  glCompressedTexImage3DARB,
  glCompressedTexImage3DOES,
  glCompressedTexSubImage1D,
  glCompressedTexSubImage1DARB,
  glCompressedTexSubImage2D,
  glCompressedTexSubImage2DARB,
  glCompressedTexSubImage3D,
  glCompressedTexSubImage3DARB,
  glCompressedTexSubImage3DOES,
  glCompressedTextureImage1DEXT,
  glCompressedTextureImage2DEXT,
  glCompressedTextureImage3DEXT,
  glCompressedTextureSubImage1D,
  glCompressedTextureSubImage1DEXT,
  glCompressedTextureSubImage2D,
  glCompressedTextureSubImage2DEXT,
  glCompressedTextureSubImage3D,
  glCompressedTextureSubImage3DEXT,
  glConservativeRasterParameterfNV,
  glConservativeRasterParameteriNV,
  glConvolutionFilter1D,
  glConvolutionFilter1DEXT,
  glConvolutionFilter2D,
  glConvolutionFilter2DEXT,
  glConvolutionParameterf,
  glConvolutionParameterfEXT,
  glConvolutionParameterfv,
  glConvolutionParameterfvEXT,
  glConvolutionParameteri,
  glConvolutionParameteriEXT,
  glConvolutionParameteriv,
  glConvolutionParameterivEXT,
  glConvolutionParameterxOES,
  glConvolutionParameterxvOES,
  glCopyBufferSubData,
  glCopyBufferSubDataNV,
  glCopyColorSubTable,
  glCopyColorSubTableEXT,
  glCopyColorTable,
  glCopyColorTableSGI,
  glCopyConvolutionFilter1D,
  glCopyConvolutionFilter1DEXT,
  glCopyConvolutionFilter2D,
  glCopyConvolutionFilter2DEXT,
  glCopyImageSubData,
  glCopyImageSubDataEXT,
  glCopyImageSubDataNV,
  glCopyImageSubDataOES,
  glCopyMultiTexImage1DEXT,
  glCopyMultiTexImage2DEXT,
  glCopyMultiTexSubImage1DEXT,
  glCopyMultiTexSubImage2DEXT,
  glCopyMultiTexSubImage3DEXT,
  glCopyNamedBufferSubData,
  glCopyPathNV,
  glCopyPixels,
  glCopyTexImage1D,
  glCopyTexImage1DEXT,
  glCopyTexImage2D,
  glCopyTexImage2DEXT,
  glCopyTexSubImage1D,
  glCopyTexSubImage1DEXT,
  glCopyTexSubImage2D,
  glCopyTexSubImage2DEXT,
  glCopyTexSubImage3D,
  glCopyTexSubImage3DEXT,
  glCopyTexSubImage3DOES,
  glCopyTextureImage1DEXT,
  glCopyTextureImage2DEXT
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glColorSubTableEXT ----------------------------------------------------------

-- | This command is an alias for 'glColorSubTable'.
glColorSubTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLsizei -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type,count)@ elements of type @a@.
  -> m ()
glColorSubTableEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn137 ptr_glColorSubTableEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorSubTableEXT #-}
ptr_glColorSubTableEXT :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorSubTableEXT = unsafePerformIO $ getCommand "glColorSubTableEXT"

-- glColorTable ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorTable.xml OpenGL 2.x>.
glColorTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glColorTable v1 v2 v3 v4 v5 v6 = liftIO $ dyn138 ptr_glColorTable v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorTable #-}
ptr_glColorTable :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorTable = unsafePerformIO $ getCommand "glColorTable"

-- glColorTableEXT -------------------------------------------------------------

-- | This command is an alias for 'glColorTable'.
glColorTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @internalFormat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glColorTableEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn138 ptr_glColorTableEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorTableEXT #-}
ptr_glColorTableEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorTableEXT = unsafePerformIO $ getCommand "glColorTableEXT"

-- glColorTableParameterfv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml OpenGL 2.x>.
glColorTableParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @pname@ of type [ColorTableParameterPNameSGI](Graphics-GL-Groups.html#ColorTableParameterPNameSGI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glColorTableParameterfv v1 v2 v3 = liftIO $ dyn139 ptr_glColorTableParameterfv v1 v2 v3

{-# NOINLINE ptr_glColorTableParameterfv #-}
ptr_glColorTableParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glColorTableParameterfv = unsafePerformIO $ getCommand "glColorTableParameterfv"

-- glColorTableParameterfvSGI --------------------------------------------------

-- | This command is an alias for 'glColorTableParameterfv'.
glColorTableParameterfvSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [ColorTableParameterPNameSGI](Graphics-GL-Groups.html#ColorTableParameterPNameSGI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glColorTableParameterfvSGI v1 v2 v3 = liftIO $ dyn139 ptr_glColorTableParameterfvSGI v1 v2 v3

{-# NOINLINE ptr_glColorTableParameterfvSGI #-}
ptr_glColorTableParameterfvSGI :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glColorTableParameterfvSGI = unsafePerformIO $ getCommand "glColorTableParameterfvSGI"

-- glColorTableParameteriv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml OpenGL 2.x>.
glColorTableParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @pname@ of type [ColorTableParameterPNameSGI](Graphics-GL-Groups.html#ColorTableParameterPNameSGI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glColorTableParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glColorTableParameteriv v1 v2 v3

{-# NOINLINE ptr_glColorTableParameteriv #-}
ptr_glColorTableParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glColorTableParameteriv = unsafePerformIO $ getCommand "glColorTableParameteriv"

-- glColorTableParameterivSGI --------------------------------------------------

-- | This command is an alias for 'glColorTableParameteriv'.
glColorTableParameterivSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [ColorTableParameterPNameSGI](Graphics-GL-Groups.html#ColorTableParameterPNameSGI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glColorTableParameterivSGI v1 v2 v3 = liftIO $ dyn140 ptr_glColorTableParameterivSGI v1 v2 v3

{-# NOINLINE ptr_glColorTableParameterivSGI #-}
ptr_glColorTableParameterivSGI :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glColorTableParameterivSGI = unsafePerformIO $ getCommand "glColorTableParameterivSGI"

-- glColorTableSGI -------------------------------------------------------------

-- | This command is an alias for 'glColorTable'.
glColorTableSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glColorTableSGI v1 v2 v3 v4 v5 v6 = liftIO $ dyn138 ptr_glColorTableSGI v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorTableSGI #-}
ptr_glColorTableSGI :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorTableSGI = unsafePerformIO $ getCommand "glColorTableSGI"

-- glCombinerInputNV -----------------------------------------------------------

glCombinerInputNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @portion@ of type [CombinerPortionNV](Graphics-GL-Groups.html#CombinerPortionNV).
  -> GLenum -- ^ @variable@ of type [CombinerVariableNV](Graphics-GL-Groups.html#CombinerVariableNV).
  -> GLenum -- ^ @input@ of type [CombinerRegisterNV](Graphics-GL-Groups.html#CombinerRegisterNV).
  -> GLenum -- ^ @mapping@ of type [CombinerMappingNV](Graphics-GL-Groups.html#CombinerMappingNV).
  -> GLenum -- ^ @componentUsage@ of type [CombinerComponentUsageNV](Graphics-GL-Groups.html#CombinerComponentUsageNV).
  -> m ()
glCombinerInputNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn141 ptr_glCombinerInputNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCombinerInputNV #-}
ptr_glCombinerInputNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glCombinerInputNV = unsafePerformIO $ getCommand "glCombinerInputNV"

-- glCombinerOutputNV ----------------------------------------------------------

glCombinerOutputNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @portion@ of type [CombinerPortionNV](Graphics-GL-Groups.html#CombinerPortionNV).
  -> GLenum -- ^ @abOutput@ of type [CombinerRegisterNV](Graphics-GL-Groups.html#CombinerRegisterNV).
  -> GLenum -- ^ @cdOutput@ of type [CombinerRegisterNV](Graphics-GL-Groups.html#CombinerRegisterNV).
  -> GLenum -- ^ @sumOutput@ of type [CombinerRegisterNV](Graphics-GL-Groups.html#CombinerRegisterNV).
  -> GLenum -- ^ @scale@ of type [CombinerScaleNV](Graphics-GL-Groups.html#CombinerScaleNV).
  -> GLenum -- ^ @bias@ of type [CombinerBiasNV](Graphics-GL-Groups.html#CombinerBiasNV).
  -> GLboolean -- ^ @abDotProduct@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @cdDotProduct@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @muxSum@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glCombinerOutputNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn142 ptr_glCombinerOutputNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCombinerOutputNV #-}
ptr_glCombinerOutputNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glCombinerOutputNV = unsafePerformIO $ getCommand "glCombinerOutputNV"

-- glCombinerParameterfNV ------------------------------------------------------

glCombinerParameterfNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> GLfloat -- ^ @param@.
  -> m ()
glCombinerParameterfNV v1 v2 = liftIO $ dyn0 ptr_glCombinerParameterfNV v1 v2

{-# NOINLINE ptr_glCombinerParameterfNV #-}
ptr_glCombinerParameterfNV :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glCombinerParameterfNV = unsafePerformIO $ getCommand "glCombinerParameterfNV"

-- glCombinerParameterfvNV -----------------------------------------------------

glCombinerParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glCombinerParameterfvNV v1 v2 = liftIO $ dyn101 ptr_glCombinerParameterfvNV v1 v2

{-# NOINLINE ptr_glCombinerParameterfvNV #-}
ptr_glCombinerParameterfvNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glCombinerParameterfvNV = unsafePerformIO $ getCommand "glCombinerParameterfvNV"

-- glCombinerParameteriNV ------------------------------------------------------

glCombinerParameteriNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> GLint -- ^ @param@.
  -> m ()
glCombinerParameteriNV v1 v2 = liftIO $ dyn58 ptr_glCombinerParameteriNV v1 v2

{-# NOINLINE ptr_glCombinerParameteriNV #-}
ptr_glCombinerParameteriNV :: FunPtr (GLenum -> GLint -> IO ())
ptr_glCombinerParameteriNV = unsafePerformIO $ getCommand "glCombinerParameteriNV"

-- glCombinerParameterivNV -----------------------------------------------------

glCombinerParameterivNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glCombinerParameterivNV v1 v2 = liftIO $ dyn143 ptr_glCombinerParameterivNV v1 v2

{-# NOINLINE ptr_glCombinerParameterivNV #-}
ptr_glCombinerParameterivNV :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glCombinerParameterivNV = unsafePerformIO $ getCommand "glCombinerParameterivNV"

-- glCombinerStageParameterfvNV ------------------------------------------------

glCombinerStageParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glCombinerStageParameterfvNV v1 v2 v3 = liftIO $ dyn139 ptr_glCombinerStageParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glCombinerStageParameterfvNV #-}
ptr_glCombinerStageParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glCombinerStageParameterfvNV = unsafePerformIO $ getCommand "glCombinerStageParameterfvNV"

-- glCommandListSegmentsNV -----------------------------------------------------

glCommandListSegmentsNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> GLuint -- ^ @segments@.
  -> m ()
glCommandListSegmentsNV v1 v2 = liftIO $ dyn4 ptr_glCommandListSegmentsNV v1 v2

{-# NOINLINE ptr_glCommandListSegmentsNV #-}
ptr_glCommandListSegmentsNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glCommandListSegmentsNV = unsafePerformIO $ getCommand "glCommandListSegmentsNV"

-- glCompileCommandListNV ------------------------------------------------------

glCompileCommandListNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> m ()
glCompileCommandListNV v1 = liftIO $ dyn3 ptr_glCompileCommandListNV v1

{-# NOINLINE ptr_glCompileCommandListNV #-}
ptr_glCompileCommandListNV :: FunPtr (GLuint -> IO ())
ptr_glCompileCommandListNV = unsafePerformIO $ getCommand "glCompileCommandListNV"

-- glCompileShader -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompileShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompileShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompileShader.xhtml OpenGL 4.x>.
glCompileShader
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> m ()
glCompileShader v1 = liftIO $ dyn3 ptr_glCompileShader v1

{-# NOINLINE ptr_glCompileShader #-}
ptr_glCompileShader :: FunPtr (GLuint -> IO ())
ptr_glCompileShader = unsafePerformIO $ getCommand "glCompileShader"

-- glCompileShaderARB ----------------------------------------------------------

-- | This command is an alias for 'glCompileShader'.
glCompileShaderARB
  :: MonadIO m
  => GLhandleARB -- ^ @shaderObj@ of type @handleARB@.
  -> m ()
glCompileShaderARB v1 = liftIO $ dyn144 ptr_glCompileShaderARB v1

{-# NOINLINE ptr_glCompileShaderARB #-}
ptr_glCompileShaderARB :: FunPtr (GLhandleARB -> IO ())
ptr_glCompileShaderARB = unsafePerformIO $ getCommand "glCompileShaderARB"

-- glCompileShaderIncludeARB ---------------------------------------------------

glCompileShaderIncludeARB
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @path@ pointing to @count@ elements of type @Ptr GLchar@.
  -> Ptr GLint -- ^ @length@ pointing to @count@ elements of type @GLint@.
  -> m ()
glCompileShaderIncludeARB v1 v2 v3 v4 = liftIO $ dyn145 ptr_glCompileShaderIncludeARB v1 v2 v3 v4

{-# NOINLINE ptr_glCompileShaderIncludeARB #-}
ptr_glCompileShaderIncludeARB :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ())
ptr_glCompileShaderIncludeARB = unsafePerformIO $ getCommand "glCompileShaderIncludeARB"

-- glCompressedMultiTexImage1DEXT ----------------------------------------------

glCompressedMultiTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn146 ptr_glCompressedMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedMultiTexImage1DEXT #-}
ptr_glCompressedMultiTexImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexImage1DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexImage1DEXT"

-- glCompressedMultiTexImage2DEXT ----------------------------------------------

glCompressedMultiTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn147 ptr_glCompressedMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedMultiTexImage2DEXT #-}
ptr_glCompressedMultiTexImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexImage2DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexImage2DEXT"

-- glCompressedMultiTexImage3DEXT ----------------------------------------------

glCompressedMultiTexImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn148 ptr_glCompressedMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedMultiTexImage3DEXT #-}
ptr_glCompressedMultiTexImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexImage3DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexImage3DEXT"

-- glCompressedMultiTexSubImage1DEXT -------------------------------------------

glCompressedMultiTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn149 ptr_glCompressedMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedMultiTexSubImage1DEXT #-}
ptr_glCompressedMultiTexSubImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexSubImage1DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexSubImage1DEXT"

-- glCompressedMultiTexSubImage2DEXT -------------------------------------------

glCompressedMultiTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn150 ptr_glCompressedMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedMultiTexSubImage2DEXT #-}
ptr_glCompressedMultiTexSubImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexSubImage2DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexSubImage2DEXT"

-- glCompressedMultiTexSubImage3DEXT -------------------------------------------

glCompressedMultiTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn151 ptr_glCompressedMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glCompressedMultiTexSubImage3DEXT #-}
ptr_glCompressedMultiTexSubImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexSubImage3DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexSubImage3DEXT"

-- glCompressedTexImage1D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage1D.xhtml OpenGL 4.x>.
glCompressedTexImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn152 ptr_glCompressedTexImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexImage1D #-}
ptr_glCompressedTexImage1D :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage1D = unsafePerformIO $ getCommand "glCompressedTexImage1D"

-- glCompressedTexImage1DARB ---------------------------------------------------

-- | This command is an alias for 'glCompressedTexImage1D'.
glCompressedTexImage1DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage1DARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn152 ptr_glCompressedTexImage1DARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexImage1DARB #-}
ptr_glCompressedTexImage1DARB :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage1DARB = unsafePerformIO $ getCommand "glCompressedTexImage1DARB"

-- glCompressedTexImage2D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage2D.xhtml OpenGL 4.x>.
glCompressedTexImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn153 ptr_glCompressedTexImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTexImage2D #-}
ptr_glCompressedTexImage2D :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage2D = unsafePerformIO $ getCommand "glCompressedTexImage2D"

-- glCompressedTexImage2DARB ---------------------------------------------------

-- | This command is an alias for 'glCompressedTexImage2D'.
glCompressedTexImage2DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage2DARB v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn153 ptr_glCompressedTexImage2DARB v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTexImage2DARB #-}
ptr_glCompressedTexImage2DARB :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage2DARB = unsafePerformIO $ getCommand "glCompressedTexImage2DARB"

-- glCompressedTexImage3D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage3D.xhtml OpenGL 4.x>.
glCompressedTexImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn154 ptr_glCompressedTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexImage3D #-}
ptr_glCompressedTexImage3D :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage3D = unsafePerformIO $ getCommand "glCompressedTexImage3D"

-- glCompressedTexImage3DARB ---------------------------------------------------

-- | This command is an alias for 'glCompressedTexImage3D'.
glCompressedTexImage3DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn154 ptr_glCompressedTexImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexImage3DARB #-}
ptr_glCompressedTexImage3DARB :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage3DARB = unsafePerformIO $ getCommand "glCompressedTexImage3DARB"

-- glCompressedTexImage3DOES ---------------------------------------------------

glCompressedTexImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn154 ptr_glCompressedTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexImage3DOES #-}
ptr_glCompressedTexImage3DOES :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage3DOES = unsafePerformIO $ getCommand "glCompressedTexImage3DOES"

-- glCompressedTexSubImage1D ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexSubImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage1D.xhtml OpenGL 4.x>.
glCompressedTexSubImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn155 ptr_glCompressedTexSubImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexSubImage1D #-}
ptr_glCompressedTexSubImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage1D = unsafePerformIO $ getCommand "glCompressedTexSubImage1D"

-- glCompressedTexSubImage1DARB ------------------------------------------------

-- | This command is an alias for 'glCompressedTexSubImage1D'.
glCompressedTexSubImage1DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage1DARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn155 ptr_glCompressedTexSubImage1DARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexSubImage1DARB #-}
ptr_glCompressedTexSubImage1DARB :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage1DARB = unsafePerformIO $ getCommand "glCompressedTexSubImage1DARB"

-- glCompressedTexSubImage2D ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexSubImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage2D.xhtml OpenGL 4.x>.
glCompressedTexSubImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn156 ptr_glCompressedTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexSubImage2D #-}
ptr_glCompressedTexSubImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage2D = unsafePerformIO $ getCommand "glCompressedTexSubImage2D"

-- glCompressedTexSubImage2DARB ------------------------------------------------

-- | This command is an alias for 'glCompressedTexSubImage2D'.
glCompressedTexSubImage2DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage2DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn156 ptr_glCompressedTexSubImage2DARB v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexSubImage2DARB #-}
ptr_glCompressedTexSubImage2DARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage2DARB = unsafePerformIO $ getCommand "glCompressedTexSubImage2DARB"

-- glCompressedTexSubImage3D ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexSubImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage3D.xhtml OpenGL 4.x>.
glCompressedTexSubImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn157 ptr_glCompressedTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTexSubImage3D #-}
ptr_glCompressedTexSubImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage3D = unsafePerformIO $ getCommand "glCompressedTexSubImage3D"

-- glCompressedTexSubImage3DARB ------------------------------------------------

-- | This command is an alias for 'glCompressedTexSubImage3D'.
glCompressedTexSubImage3DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn157 ptr_glCompressedTexSubImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTexSubImage3DARB #-}
ptr_glCompressedTexSubImage3DARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage3DARB = unsafePerformIO $ getCommand "glCompressedTexSubImage3DARB"

-- glCompressedTexSubImage3DOES ------------------------------------------------

glCompressedTexSubImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn157 ptr_glCompressedTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTexSubImage3DOES #-}
ptr_glCompressedTexSubImage3DOES :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage3DOES = unsafePerformIO $ getCommand "glCompressedTexSubImage3DOES"

-- glCompressedTextureImage1DEXT -----------------------------------------------

glCompressedTextureImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn158 ptr_glCompressedTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTextureImage1DEXT #-}
ptr_glCompressedTextureImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureImage1DEXT = unsafePerformIO $ getCommand "glCompressedTextureImage1DEXT"

-- glCompressedTextureImage2DEXT -----------------------------------------------

glCompressedTextureImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn159 ptr_glCompressedTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTextureImage2DEXT #-}
ptr_glCompressedTextureImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureImage2DEXT = unsafePerformIO $ getCommand "glCompressedTextureImage2DEXT"

-- glCompressedTextureImage3DEXT -----------------------------------------------

glCompressedTextureImage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn160 ptr_glCompressedTextureImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedTextureImage3DEXT #-}
ptr_glCompressedTextureImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureImage3DEXT = unsafePerformIO $ getCommand "glCompressedTextureImage3DEXT"

-- glCompressedTextureSubImage1D -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage1D.xhtml OpenGL 4.x>.
glCompressedTextureSubImage1D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glCompressedTextureSubImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn161 ptr_glCompressedTextureSubImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTextureSubImage1D #-}
ptr_glCompressedTextureSubImage1D :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage1D = unsafePerformIO $ getCommand "glCompressedTextureSubImage1D"

-- glCompressedTextureSubImage1DEXT --------------------------------------------

glCompressedTextureSubImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn162 ptr_glCompressedTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTextureSubImage1DEXT #-}
ptr_glCompressedTextureSubImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage1DEXT = unsafePerformIO $ getCommand "glCompressedTextureSubImage1DEXT"

-- glCompressedTextureSubImage2D -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage2D.xhtml OpenGL 4.x>.
glCompressedTextureSubImage2D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glCompressedTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn163 ptr_glCompressedTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTextureSubImage2D #-}
ptr_glCompressedTextureSubImage2D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage2D = unsafePerformIO $ getCommand "glCompressedTextureSubImage2D"

-- glCompressedTextureSubImage2DEXT --------------------------------------------

glCompressedTextureSubImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn164 ptr_glCompressedTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedTextureSubImage2DEXT #-}
ptr_glCompressedTextureSubImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage2DEXT = unsafePerformIO $ getCommand "glCompressedTextureSubImage2DEXT"

-- glCompressedTextureSubImage3D -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage3D.xhtml OpenGL 4.x>.
glCompressedTextureSubImage3D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glCompressedTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn165 ptr_glCompressedTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTextureSubImage3D #-}
ptr_glCompressedTextureSubImage3D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage3D = unsafePerformIO $ getCommand "glCompressedTextureSubImage3D"

-- glCompressedTextureSubImage3DEXT --------------------------------------------

glCompressedTextureSubImage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn166 ptr_glCompressedTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glCompressedTextureSubImage3DEXT #-}
ptr_glCompressedTextureSubImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage3DEXT = unsafePerformIO $ getCommand "glCompressedTextureSubImage3DEXT"

-- glConservativeRasterParameterfNV --------------------------------------------

glConservativeRasterParameterfNV
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfloat -- ^ @value@.
  -> m ()
glConservativeRasterParameterfNV v1 v2 = liftIO $ dyn0 ptr_glConservativeRasterParameterfNV v1 v2

{-# NOINLINE ptr_glConservativeRasterParameterfNV #-}
ptr_glConservativeRasterParameterfNV :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glConservativeRasterParameterfNV = unsafePerformIO $ getCommand "glConservativeRasterParameterfNV"

-- glConservativeRasterParameteriNV --------------------------------------------

glConservativeRasterParameteriNV
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glConservativeRasterParameteriNV v1 v2 = liftIO $ dyn58 ptr_glConservativeRasterParameteriNV v1 v2

{-# NOINLINE ptr_glConservativeRasterParameteriNV #-}
ptr_glConservativeRasterParameteriNV :: FunPtr (GLenum -> GLint -> IO ())
ptr_glConservativeRasterParameteriNV = unsafePerformIO $ getCommand "glConservativeRasterParameteriNV"

-- glConvolutionFilter1D -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter1D.xml OpenGL 2.x>.
glConvolutionFilter1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glConvolutionFilter1D v1 v2 v3 v4 v5 v6 = liftIO $ dyn138 ptr_glConvolutionFilter1D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glConvolutionFilter1D #-}
ptr_glConvolutionFilter1D :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter1D = unsafePerformIO $ getCommand "glConvolutionFilter1D"

-- glConvolutionFilter1DEXT ----------------------------------------------------

-- | This command is an alias for 'glConvolutionFilter1D'.
glConvolutionFilter1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glConvolutionFilter1DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn138 ptr_glConvolutionFilter1DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glConvolutionFilter1DEXT #-}
ptr_glConvolutionFilter1DEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter1DEXT = unsafePerformIO $ getCommand "glConvolutionFilter1DEXT"

-- glConvolutionFilter2D -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter2D.xml OpenGL 2.x>.
glConvolutionFilter2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glConvolutionFilter2D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn167 ptr_glConvolutionFilter2D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glConvolutionFilter2D #-}
ptr_glConvolutionFilter2D :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter2D = unsafePerformIO $ getCommand "glConvolutionFilter2D"

-- glConvolutionFilter2DEXT ----------------------------------------------------

-- | This command is an alias for 'glConvolutionFilter2D'.
glConvolutionFilter2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn167 ptr_glConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glConvolutionFilter2DEXT #-}
ptr_glConvolutionFilter2DEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter2DEXT = unsafePerformIO $ getCommand "glConvolutionFilter2DEXT"

-- glConvolutionParameterf -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameterf
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> GLfloat -- ^ @params@ of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterf v1 v2 v3 = liftIO $ dyn168 ptr_glConvolutionParameterf v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterf #-}
ptr_glConvolutionParameterf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glConvolutionParameterf = unsafePerformIO $ getCommand "glConvolutionParameterf"

-- glConvolutionParameterfEXT --------------------------------------------------

-- | This command is an alias for 'glConvolutionParameterf'.
glConvolutionParameterfEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> GLfloat -- ^ @params@ of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterfEXT v1 v2 v3 = liftIO $ dyn168 ptr_glConvolutionParameterfEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterfEXT #-}
ptr_glConvolutionParameterfEXT :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glConvolutionParameterfEXT = unsafePerformIO $ getCommand "glConvolutionParameterfEXT"

-- glConvolutionParameterfv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterfv v1 v2 v3 = liftIO $ dyn139 ptr_glConvolutionParameterfv v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterfv #-}
ptr_glConvolutionParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glConvolutionParameterfv = unsafePerformIO $ getCommand "glConvolutionParameterfv"

-- glConvolutionParameterfvEXT -------------------------------------------------

-- | This command is an alias for 'glConvolutionParameterfv'.
glConvolutionParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterfvEXT v1 v2 v3 = liftIO $ dyn139 ptr_glConvolutionParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterfvEXT #-}
ptr_glConvolutionParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glConvolutionParameterfvEXT = unsafePerformIO $ getCommand "glConvolutionParameterfvEXT"

-- glConvolutionParameteri -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameteri
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> GLint -- ^ @params@ of type @CheckedInt32@.
  -> m ()
glConvolutionParameteri v1 v2 v3 = liftIO $ dyn66 ptr_glConvolutionParameteri v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameteri #-}
ptr_glConvolutionParameteri :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glConvolutionParameteri = unsafePerformIO $ getCommand "glConvolutionParameteri"

-- glConvolutionParameteriEXT --------------------------------------------------

-- | This command is an alias for 'glConvolutionParameteri'.
glConvolutionParameteriEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> GLint -- ^ @params@ of type @CheckedInt32@.
  -> m ()
glConvolutionParameteriEXT v1 v2 v3 = liftIO $ dyn66 ptr_glConvolutionParameteriEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameteriEXT #-}
ptr_glConvolutionParameteriEXT :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glConvolutionParameteriEXT = unsafePerformIO $ getCommand "glConvolutionParameteriEXT"

-- glConvolutionParameteriv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glConvolutionParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glConvolutionParameteriv v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameteriv #-}
ptr_glConvolutionParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glConvolutionParameteriv = unsafePerformIO $ getCommand "glConvolutionParameteriv"

-- glConvolutionParameterivEXT -------------------------------------------------

-- | This command is an alias for 'glConvolutionParameteriv'.
glConvolutionParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glConvolutionParameterivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glConvolutionParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterivEXT #-}
ptr_glConvolutionParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glConvolutionParameterivEXT = unsafePerformIO $ getCommand "glConvolutionParameterivEXT"

-- glConvolutionParameterxOES --------------------------------------------------

glConvolutionParameterxOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> GLfixed -- ^ @param@.
  -> m ()
glConvolutionParameterxOES v1 v2 v3 = liftIO $ dyn169 ptr_glConvolutionParameterxOES v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterxOES #-}
ptr_glConvolutionParameterxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glConvolutionParameterxOES = unsafePerformIO $ getCommand "glConvolutionParameterxOES"

-- glConvolutionParameterxvOES -------------------------------------------------

glConvolutionParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glConvolutionParameterxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glConvolutionParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterxvOES #-}
ptr_glConvolutionParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glConvolutionParameterxvOES = unsafePerformIO $ getCommand "glConvolutionParameterxvOES"

-- glCopyBufferSubData ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyBufferSubData.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyBufferSubData.xhtml OpenGL 4.x>.
glCopyBufferSubData
  :: MonadIO m
  => GLenum -- ^ @readTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLenum -- ^ @writeTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLintptr -- ^ @readOffset@ of type @BufferOffset@.
  -> GLintptr -- ^ @writeOffset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glCopyBufferSubData v1 v2 v3 v4 v5 = liftIO $ dyn171 ptr_glCopyBufferSubData v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyBufferSubData #-}
ptr_glCopyBufferSubData :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glCopyBufferSubData = unsafePerformIO $ getCommand "glCopyBufferSubData"

-- glCopyBufferSubDataNV -------------------------------------------------------

-- | This command is an alias for 'glCopyBufferSubData'.
glCopyBufferSubDataNV
  :: MonadIO m
  => GLenum -- ^ @readTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLenum -- ^ @writeTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLintptr -- ^ @readOffset@ of type @BufferOffset@.
  -> GLintptr -- ^ @writeOffset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glCopyBufferSubDataNV v1 v2 v3 v4 v5 = liftIO $ dyn171 ptr_glCopyBufferSubDataNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyBufferSubDataNV #-}
ptr_glCopyBufferSubDataNV :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glCopyBufferSubDataNV = unsafePerformIO $ getCommand "glCopyBufferSubDataNV"

-- glCopyColorSubTable ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorSubTable.xml OpenGL 2.x>.
glCopyColorSubTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLsizei -- ^ @start@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorSubTable v1 v2 v3 v4 v5 = liftIO $ dyn172 ptr_glCopyColorSubTable v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorSubTable #-}
ptr_glCopyColorSubTable :: FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorSubTable = unsafePerformIO $ getCommand "glCopyColorSubTable"

-- glCopyColorSubTableEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyColorSubTable'.
glCopyColorSubTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLsizei -- ^ @start@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorSubTableEXT v1 v2 v3 v4 v5 = liftIO $ dyn172 ptr_glCopyColorSubTableEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorSubTableEXT #-}
ptr_glCopyColorSubTableEXT :: FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorSubTableEXT = unsafePerformIO $ getCommand "glCopyColorSubTableEXT"

-- glCopyColorTable ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorTable.xml OpenGL 2.x>.
glCopyColorTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorTable v1 v2 v3 v4 v5 = liftIO $ dyn173 ptr_glCopyColorTable v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorTable #-}
ptr_glCopyColorTable :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorTable = unsafePerformIO $ getCommand "glCopyColorTable"

-- glCopyColorTableSGI ---------------------------------------------------------

-- | This command is an alias for 'glCopyColorTable'.
glCopyColorTableSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorTableSGI v1 v2 v3 v4 v5 = liftIO $ dyn173 ptr_glCopyColorTableSGI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorTableSGI #-}
ptr_glCopyColorTableSGI :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorTableSGI = unsafePerformIO $ getCommand "glCopyColorTableSGI"

-- glCopyConvolutionFilter1D ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter1D.xml OpenGL 2.x>.
glCopyConvolutionFilter1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyConvolutionFilter1D v1 v2 v3 v4 v5 = liftIO $ dyn173 ptr_glCopyConvolutionFilter1D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyConvolutionFilter1D #-}
ptr_glCopyConvolutionFilter1D :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter1D = unsafePerformIO $ getCommand "glCopyConvolutionFilter1D"

-- glCopyConvolutionFilter1DEXT ------------------------------------------------

-- | This command is an alias for 'glCopyConvolutionFilter1D'.
glCopyConvolutionFilter1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyConvolutionFilter1DEXT v1 v2 v3 v4 v5 = liftIO $ dyn173 ptr_glCopyConvolutionFilter1DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyConvolutionFilter1DEXT #-}
ptr_glCopyConvolutionFilter1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter1DEXT = unsafePerformIO $ getCommand "glCopyConvolutionFilter1DEXT"

-- glCopyConvolutionFilter2D ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter2D.xml OpenGL 2.x>.
glCopyConvolutionFilter2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyConvolutionFilter2D v1 v2 v3 v4 v5 v6 = liftIO $ dyn174 ptr_glCopyConvolutionFilter2D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyConvolutionFilter2D #-}
ptr_glCopyConvolutionFilter2D :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter2D = unsafePerformIO $ getCommand "glCopyConvolutionFilter2D"

-- glCopyConvolutionFilter2DEXT ------------------------------------------------

-- | This command is an alias for 'glCopyConvolutionFilter2D'.
glCopyConvolutionFilter2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn174 ptr_glCopyConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyConvolutionFilter2DEXT #-}
ptr_glCopyConvolutionFilter2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter2DEXT = unsafePerformIO $ getCommand "glCopyConvolutionFilter2DEXT"

-- glCopyImageSubData ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyImageSubData.xhtml OpenGL 4.x>.
glCopyImageSubData
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@ of type [CopyImageSubDataTarget](Graphics-GL-Groups.html#CopyImageSubDataTarget).
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@ of type [CopyImageSubDataTarget](Graphics-GL-Groups.html#CopyImageSubDataTarget).
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @srcWidth@.
  -> GLsizei -- ^ @srcHeight@.
  -> GLsizei -- ^ @srcDepth@.
  -> m ()
glCopyImageSubData v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn175 ptr_glCopyImageSubData v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubData #-}
ptr_glCopyImageSubData :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubData = unsafePerformIO $ getCommand "glCopyImageSubData"

-- glCopyImageSubDataEXT -------------------------------------------------------

-- | This command is an alias for 'glCopyImageSubData'.
glCopyImageSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @srcWidth@.
  -> GLsizei -- ^ @srcHeight@.
  -> GLsizei -- ^ @srcDepth@.
  -> m ()
glCopyImageSubDataEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn175 ptr_glCopyImageSubDataEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubDataEXT #-}
ptr_glCopyImageSubDataEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubDataEXT = unsafePerformIO $ getCommand "glCopyImageSubDataEXT"

-- glCopyImageSubDataNV --------------------------------------------------------

glCopyImageSubDataNV
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glCopyImageSubDataNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn175 ptr_glCopyImageSubDataNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubDataNV #-}
ptr_glCopyImageSubDataNV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubDataNV = unsafePerformIO $ getCommand "glCopyImageSubDataNV"

-- glCopyImageSubDataOES -------------------------------------------------------

-- | This command is an alias for 'glCopyImageSubData'.
glCopyImageSubDataOES
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@ of type [CopyBufferSubDataTarget](Graphics-GL-Groups.html#CopyBufferSubDataTarget).
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @srcWidth@.
  -> GLsizei -- ^ @srcHeight@.
  -> GLsizei -- ^ @srcDepth@.
  -> m ()
glCopyImageSubDataOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn175 ptr_glCopyImageSubDataOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubDataOES #-}
ptr_glCopyImageSubDataOES :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubDataOES = unsafePerformIO $ getCommand "glCopyImageSubDataOES"

-- glCopyMultiTexImage1DEXT ----------------------------------------------------

glCopyMultiTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn176 ptr_glCopyMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyMultiTexImage1DEXT #-}
ptr_glCopyMultiTexImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyMultiTexImage1DEXT = unsafePerformIO $ getCommand "glCopyMultiTexImage1DEXT"

-- glCopyMultiTexImage2DEXT ----------------------------------------------------

glCopyMultiTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn177 ptr_glCopyMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyMultiTexImage2DEXT #-}
ptr_glCopyMultiTexImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyMultiTexImage2DEXT = unsafePerformIO $ getCommand "glCopyMultiTexImage2DEXT"

-- glCopyMultiTexSubImage1DEXT -------------------------------------------------

glCopyMultiTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn178 ptr_glCopyMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyMultiTexSubImage1DEXT #-}
ptr_glCopyMultiTexSubImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyMultiTexSubImage1DEXT = unsafePerformIO $ getCommand "glCopyMultiTexSubImage1DEXT"

-- glCopyMultiTexSubImage2DEXT -------------------------------------------------

glCopyMultiTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn179 ptr_glCopyMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyMultiTexSubImage2DEXT #-}
ptr_glCopyMultiTexSubImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyMultiTexSubImage2DEXT = unsafePerformIO $ getCommand "glCopyMultiTexSubImage2DEXT"

-- glCopyMultiTexSubImage3DEXT -------------------------------------------------

glCopyMultiTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn180 ptr_glCopyMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCopyMultiTexSubImage3DEXT #-}
ptr_glCopyMultiTexSubImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyMultiTexSubImage3DEXT = unsafePerformIO $ getCommand "glCopyMultiTexSubImage3DEXT"

-- glCopyNamedBufferSubData ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyBufferSubData.xhtml OpenGL 4.x>.
glCopyNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @readBuffer@.
  -> GLuint -- ^ @writeBuffer@.
  -> GLintptr -- ^ @readOffset@.
  -> GLintptr -- ^ @writeOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glCopyNamedBufferSubData v1 v2 v3 v4 v5 = liftIO $ dyn181 ptr_glCopyNamedBufferSubData v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyNamedBufferSubData #-}
ptr_glCopyNamedBufferSubData :: FunPtr (GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glCopyNamedBufferSubData = unsafePerformIO $ getCommand "glCopyNamedBufferSubData"

-- glCopyPathNV ----------------------------------------------------------------

glCopyPathNV
  :: MonadIO m
  => GLuint -- ^ @resultPath@ of type @Path@.
  -> GLuint -- ^ @srcPath@ of type @Path@.
  -> m ()
glCopyPathNV v1 v2 = liftIO $ dyn4 ptr_glCopyPathNV v1 v2

{-# NOINLINE ptr_glCopyPathNV #-}
ptr_glCopyPathNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glCopyPathNV = unsafePerformIO $ getCommand "glCopyPathNV"

-- glCopyPixels ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyPixels.xml OpenGL 2.x>.
glCopyPixels
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @type@ of type [PixelCopyType](Graphics-GL-Groups.html#PixelCopyType).
  -> m ()
glCopyPixels v1 v2 v3 v4 v5 = liftIO $ dyn182 ptr_glCopyPixels v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyPixels #-}
ptr_glCopyPixels :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> IO ())
ptr_glCopyPixels = unsafePerformIO $ getCommand "glCopyPixels"

-- glCopyTexImage1D ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexImage1D.xhtml OpenGL 4.x>.
glCopyTexImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn183 ptr_glCopyTexImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyTexImage1D #-}
ptr_glCopyTexImage1D :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage1D = unsafePerformIO $ getCommand "glCopyTexImage1D"

-- glCopyTexImage1DEXT ---------------------------------------------------------

-- | This command is an alias for 'glCopyTexImage1D'.
glCopyTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn183 ptr_glCopyTexImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyTexImage1DEXT #-}
ptr_glCopyTexImage1DEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage1DEXT = unsafePerformIO $ getCommand "glCopyTexImage1DEXT"

-- glCopyTexImage2D ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexImage2D.xhtml OpenGL 4.x>.
glCopyTexImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn184 ptr_glCopyTexImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexImage2D #-}
ptr_glCopyTexImage2D :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage2D = unsafePerformIO $ getCommand "glCopyTexImage2D"

-- glCopyTexImage2DEXT ---------------------------------------------------------

-- | This command is an alias for 'glCopyTexImage2D'.
glCopyTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn184 ptr_glCopyTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexImage2DEXT #-}
ptr_glCopyTexImage2DEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage2DEXT = unsafePerformIO $ getCommand "glCopyTexImage2DEXT"

-- glCopyTexSubImage1D ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexSubImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage1D.xhtml OpenGL 4.x>.
glCopyTexSubImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTexSubImage1D v1 v2 v3 v4 v5 v6 = liftIO $ dyn185 ptr_glCopyTexSubImage1D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyTexSubImage1D #-}
ptr_glCopyTexSubImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTexSubImage1D = unsafePerformIO $ getCommand "glCopyTexSubImage1D"

-- glCopyTexSubImage1DEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyTexSubImage1D'.
glCopyTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTexSubImage1DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn185 ptr_glCopyTexSubImage1DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyTexSubImage1DEXT #-}
ptr_glCopyTexSubImage1DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTexSubImage1DEXT = unsafePerformIO $ getCommand "glCopyTexSubImage1DEXT"

-- glCopyTexSubImage2D ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexSubImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage2D.xhtml OpenGL 4.x>.
glCopyTexSubImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn186 ptr_glCopyTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexSubImage2D #-}
ptr_glCopyTexSubImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage2D = unsafePerformIO $ getCommand "glCopyTexSubImage2D"

-- glCopyTexSubImage2DEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyTexSubImage2D'.
glCopyTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn186 ptr_glCopyTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexSubImage2DEXT #-}
ptr_glCopyTexSubImage2DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage2DEXT = unsafePerformIO $ getCommand "glCopyTexSubImage2DEXT"

-- glCopyTexSubImage3D ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexSubImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage3D.xhtml OpenGL 4.x>.
glCopyTexSubImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn187 ptr_glCopyTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTexSubImage3D #-}
ptr_glCopyTexSubImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage3D = unsafePerformIO $ getCommand "glCopyTexSubImage3D"

-- glCopyTexSubImage3DEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyTexSubImage3D'.
glCopyTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn187 ptr_glCopyTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTexSubImage3DEXT #-}
ptr_glCopyTexSubImage3DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage3DEXT = unsafePerformIO $ getCommand "glCopyTexSubImage3DEXT"

-- glCopyTexSubImage3DOES ------------------------------------------------------

glCopyTexSubImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn187 ptr_glCopyTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTexSubImage3DOES #-}
ptr_glCopyTexSubImage3DOES :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage3DOES = unsafePerformIO $ getCommand "glCopyTexSubImage3DOES"

-- glCopyTextureImage1DEXT -----------------------------------------------------

glCopyTextureImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn188 ptr_glCopyTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTextureImage1DEXT #-}
ptr_glCopyTextureImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyTextureImage1DEXT = unsafePerformIO $ getCommand "glCopyTextureImage1DEXT"

-- glCopyTextureImage2DEXT -----------------------------------------------------

glCopyTextureImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn189 ptr_glCopyTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTextureImage2DEXT #-}
ptr_glCopyTextureImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyTextureImage2DEXT = unsafePerformIO $ getCommand "glCopyTextureImage2DEXT"

