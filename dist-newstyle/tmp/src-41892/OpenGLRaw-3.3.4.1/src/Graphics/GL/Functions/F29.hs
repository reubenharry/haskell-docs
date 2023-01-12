{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F29
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

module Graphics.GL.Functions.F29 (
  glUpdateObjectBufferATI,
  glUploadGpuMaskNVX,
  glUseProgram,
  glUseProgramObjectARB,
  glUseProgramStages,
  glUseProgramStagesEXT,
  glUseShaderProgramEXT,
  glVDPAUFiniNV,
  glVDPAUGetSurfaceivNV,
  glVDPAUInitNV,
  glVDPAUIsSurfaceNV,
  glVDPAUMapSurfacesNV,
  glVDPAURegisterOutputSurfaceNV,
  glVDPAURegisterVideoSurfaceNV,
  glVDPAURegisterVideoSurfaceWithPictureStructureNV,
  glVDPAUSurfaceAccessNV,
  glVDPAUUnmapSurfacesNV,
  glVDPAUUnregisterSurfaceNV,
  glValidateProgram,
  glValidateProgramARB,
  glValidateProgramPipeline,
  glValidateProgramPipelineEXT,
  glVariantArrayObjectATI,
  glVariantPointerEXT,
  glVariantbvEXT,
  glVariantdvEXT,
  glVariantfvEXT,
  glVariantivEXT,
  glVariantsvEXT,
  glVariantubvEXT,
  glVariantuivEXT,
  glVariantusvEXT,
  glVertex2bOES,
  glVertex2bvOES,
  glVertex2d,
  glVertex2dv,
  glVertex2f,
  glVertex2fv,
  glVertex2hNV,
  glVertex2hvNV,
  glVertex2i,
  glVertex2iv,
  glVertex2s,
  glVertex2sv,
  glVertex2xOES,
  glVertex2xvOES,
  glVertex3bOES,
  glVertex3bvOES,
  glVertex3d,
  glVertex3dv,
  glVertex3f,
  glVertex3fv,
  glVertex3hNV,
  glVertex3hvNV,
  glVertex3i,
  glVertex3iv,
  glVertex3s,
  glVertex3sv,
  glVertex3xOES,
  glVertex3xvOES,
  glVertex4bOES,
  glVertex4bvOES,
  glVertex4d,
  glVertex4dv,
  glVertex4f,
  glVertex4fv,
  glVertex4hNV,
  glVertex4hvNV,
  glVertex4i,
  glVertex4iv,
  glVertex4s,
  glVertex4sv,
  glVertex4xOES,
  glVertex4xvOES,
  glVertexArrayAttribBinding,
  glVertexArrayAttribFormat,
  glVertexArrayAttribIFormat,
  glVertexArrayAttribLFormat,
  glVertexArrayBindVertexBufferEXT,
  glVertexArrayBindingDivisor,
  glVertexArrayColorOffsetEXT,
  glVertexArrayEdgeFlagOffsetEXT,
  glVertexArrayElementBuffer,
  glVertexArrayFogCoordOffsetEXT,
  glVertexArrayIndexOffsetEXT,
  glVertexArrayMultiTexCoordOffsetEXT,
  glVertexArrayNormalOffsetEXT,
  glVertexArrayParameteriAPPLE,
  glVertexArrayRangeAPPLE,
  glVertexArrayRangeNV,
  glVertexArraySecondaryColorOffsetEXT,
  glVertexArrayTexCoordOffsetEXT,
  glVertexArrayVertexAttribBindingEXT,
  glVertexArrayVertexAttribDivisorEXT,
  glVertexArrayVertexAttribFormatEXT,
  glVertexArrayVertexAttribIFormatEXT,
  glVertexArrayVertexAttribIOffsetEXT,
  glVertexArrayVertexAttribLFormatEXT,
  glVertexArrayVertexAttribLOffsetEXT,
  glVertexArrayVertexAttribOffsetEXT
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glUpdateObjectBufferATI -----------------------------------------------------

glUpdateObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> GLsizei -- ^ @size@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> GLenum -- ^ @preserve@ of type [PreserveModeATI](Graphics-GL-Groups.html#PreserveModeATI).
  -> m ()
glUpdateObjectBufferATI v1 v2 v3 v4 v5 = liftIO $ dyn869 ptr_glUpdateObjectBufferATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUpdateObjectBufferATI #-}
ptr_glUpdateObjectBufferATI :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr a -> GLenum -> IO ())
ptr_glUpdateObjectBufferATI = unsafePerformIO $ getCommand "glUpdateObjectBufferATI"

-- glUploadGpuMaskNVX ----------------------------------------------------------

glUploadGpuMaskNVX
  :: MonadIO m
  => GLbitfield -- ^ @mask@.
  -> m ()
glUploadGpuMaskNVX v1 = liftIO $ dyn75 ptr_glUploadGpuMaskNVX v1

{-# NOINLINE ptr_glUploadGpuMaskNVX #-}
ptr_glUploadGpuMaskNVX :: FunPtr (GLbitfield -> IO ())
ptr_glUploadGpuMaskNVX = unsafePerformIO $ getCommand "glUploadGpuMaskNVX"

-- glUseProgram ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUseProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUseProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUseProgram.xhtml OpenGL 4.x>.
glUseProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glUseProgram v1 = liftIO $ dyn3 ptr_glUseProgram v1

{-# NOINLINE ptr_glUseProgram #-}
ptr_glUseProgram :: FunPtr (GLuint -> IO ())
ptr_glUseProgram = unsafePerformIO $ getCommand "glUseProgram"

-- glUseProgramObjectARB -------------------------------------------------------

-- | This command is an alias for 'glUseProgram'.
glUseProgramObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> m ()
glUseProgramObjectARB v1 = liftIO $ dyn144 ptr_glUseProgramObjectARB v1

{-# NOINLINE ptr_glUseProgramObjectARB #-}
ptr_glUseProgramObjectARB :: FunPtr (GLhandleARB -> IO ())
ptr_glUseProgramObjectARB = unsafePerformIO $ getCommand "glUseProgramObjectARB"

-- glUseProgramStages ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glUseProgramStages.xhtml OpenGL 4.x>.
glUseProgramStages
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLbitfield -- ^ @stages@ of type [UseProgramStageMask](Graphics-GL-Groups.html#UseProgramStageMask).
  -> GLuint -- ^ @program@.
  -> m ()
glUseProgramStages v1 v2 v3 = liftIO $ dyn870 ptr_glUseProgramStages v1 v2 v3

{-# NOINLINE ptr_glUseProgramStages #-}
ptr_glUseProgramStages :: FunPtr (GLuint -> GLbitfield -> GLuint -> IO ())
ptr_glUseProgramStages = unsafePerformIO $ getCommand "glUseProgramStages"

-- glUseProgramStagesEXT -------------------------------------------------------

glUseProgramStagesEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLbitfield -- ^ @stages@ of type [UseProgramStageMask](Graphics-GL-Groups.html#UseProgramStageMask).
  -> GLuint -- ^ @program@.
  -> m ()
glUseProgramStagesEXT v1 v2 v3 = liftIO $ dyn870 ptr_glUseProgramStagesEXT v1 v2 v3

{-# NOINLINE ptr_glUseProgramStagesEXT #-}
ptr_glUseProgramStagesEXT :: FunPtr (GLuint -> GLbitfield -> GLuint -> IO ())
ptr_glUseProgramStagesEXT = unsafePerformIO $ getCommand "glUseProgramStagesEXT"

-- glUseShaderProgramEXT -------------------------------------------------------

glUseShaderProgramEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @program@.
  -> m ()
glUseShaderProgramEXT v1 v2 = liftIO $ dyn19 ptr_glUseShaderProgramEXT v1 v2

{-# NOINLINE ptr_glUseShaderProgramEXT #-}
ptr_glUseShaderProgramEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glUseShaderProgramEXT = unsafePerformIO $ getCommand "glUseShaderProgramEXT"

-- glVDPAUFiniNV ---------------------------------------------------------------

glVDPAUFiniNV
  :: MonadIO m
  => m ()
glVDPAUFiniNV = liftIO $ dyn11 ptr_glVDPAUFiniNV

{-# NOINLINE ptr_glVDPAUFiniNV #-}
ptr_glVDPAUFiniNV :: FunPtr (IO ())
ptr_glVDPAUFiniNV = unsafePerformIO $ getCommand "glVDPAUFiniNV"

-- glVDPAUGetSurfaceivNV -------------------------------------------------------

glVDPAUGetSurfaceivNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLint -- ^ @values@ pointing to @count@ elements of type @GLint@.
  -> m ()
glVDPAUGetSurfaceivNV v1 v2 v3 v4 v5 = liftIO $ dyn871 ptr_glVDPAUGetSurfaceivNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVDPAUGetSurfaceivNV #-}
ptr_glVDPAUGetSurfaceivNV :: FunPtr (GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glVDPAUGetSurfaceivNV = unsafePerformIO $ getCommand "glVDPAUGetSurfaceivNV"

-- glVDPAUInitNV ---------------------------------------------------------------

glVDPAUInitNV
  :: MonadIO m
  => Ptr a -- ^ @vdpDevice@.
  -> Ptr b -- ^ @getProcAddress@.
  -> m ()
glVDPAUInitNV v1 v2 = liftIO $ dyn872 ptr_glVDPAUInitNV v1 v2

{-# NOINLINE ptr_glVDPAUInitNV #-}
ptr_glVDPAUInitNV :: FunPtr (Ptr a -> Ptr b -> IO ())
ptr_glVDPAUInitNV = unsafePerformIO $ getCommand "glVDPAUInitNV"

-- glVDPAUIsSurfaceNV ----------------------------------------------------------

glVDPAUIsSurfaceNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glVDPAUIsSurfaceNV v1 = liftIO $ dyn873 ptr_glVDPAUIsSurfaceNV v1

{-# NOINLINE ptr_glVDPAUIsSurfaceNV #-}
ptr_glVDPAUIsSurfaceNV :: FunPtr (GLvdpauSurfaceNV -> IO GLboolean)
ptr_glVDPAUIsSurfaceNV = unsafePerformIO $ getCommand "glVDPAUIsSurfaceNV"

-- glVDPAUMapSurfacesNV --------------------------------------------------------

glVDPAUMapSurfacesNV
  :: MonadIO m
  => GLsizei -- ^ @numSurfaces@.
  -> Ptr GLvdpauSurfaceNV -- ^ @surfaces@ pointing to @numSurfaces@ elements of type @vdpauSurfaceNV@.
  -> m ()
glVDPAUMapSurfacesNV v1 v2 = liftIO $ dyn874 ptr_glVDPAUMapSurfacesNV v1 v2

{-# NOINLINE ptr_glVDPAUMapSurfacesNV #-}
ptr_glVDPAUMapSurfacesNV :: FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ())
ptr_glVDPAUMapSurfacesNV = unsafePerformIO $ getCommand "glVDPAUMapSurfacesNV"

-- glVDPAURegisterOutputSurfaceNV ----------------------------------------------

glVDPAURegisterOutputSurfaceNV
  :: MonadIO m
  => Ptr a -- ^ @vdpSurface@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @numTextureNames@.
  -> Ptr GLuint -- ^ @textureNames@ pointing to @numTextureNames@ elements of type @GLuint@.
  -> m GLvdpauSurfaceNV -- ^ of type @vdpauSurfaceNV@.
glVDPAURegisterOutputSurfaceNV v1 v2 v3 v4 = liftIO $ dyn875 ptr_glVDPAURegisterOutputSurfaceNV v1 v2 v3 v4

{-# NOINLINE ptr_glVDPAURegisterOutputSurfaceNV #-}
ptr_glVDPAURegisterOutputSurfaceNV :: FunPtr (Ptr a -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV)
ptr_glVDPAURegisterOutputSurfaceNV = unsafePerformIO $ getCommand "glVDPAURegisterOutputSurfaceNV"

-- glVDPAURegisterVideoSurfaceNV -----------------------------------------------

glVDPAURegisterVideoSurfaceNV
  :: MonadIO m
  => Ptr a -- ^ @vdpSurface@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @numTextureNames@.
  -> Ptr GLuint -- ^ @textureNames@ pointing to @numTextureNames@ elements of type @GLuint@.
  -> m GLvdpauSurfaceNV -- ^ of type @vdpauSurfaceNV@.
glVDPAURegisterVideoSurfaceNV v1 v2 v3 v4 = liftIO $ dyn875 ptr_glVDPAURegisterVideoSurfaceNV v1 v2 v3 v4

{-# NOINLINE ptr_glVDPAURegisterVideoSurfaceNV #-}
ptr_glVDPAURegisterVideoSurfaceNV :: FunPtr (Ptr a -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV)
ptr_glVDPAURegisterVideoSurfaceNV = unsafePerformIO $ getCommand "glVDPAURegisterVideoSurfaceNV"

-- glVDPAURegisterVideoSurfaceWithPictureStructureNV ---------------------------

glVDPAURegisterVideoSurfaceWithPictureStructureNV
  :: MonadIO m
  => Ptr a -- ^ @vdpSurface@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @numTextureNames@.
  -> Ptr GLuint -- ^ @textureNames@ pointing to @numTextureNames@ elements of type @GLuint@.
  -> GLboolean -- ^ @isFrameStructure@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m GLvdpauSurfaceNV -- ^ of type @vdpauSurfaceNV@.
glVDPAURegisterVideoSurfaceWithPictureStructureNV v1 v2 v3 v4 v5 = liftIO $ dyn876 ptr_glVDPAURegisterVideoSurfaceWithPictureStructureNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVDPAURegisterVideoSurfaceWithPictureStructureNV #-}
ptr_glVDPAURegisterVideoSurfaceWithPictureStructureNV :: FunPtr (Ptr a -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO GLvdpauSurfaceNV)
ptr_glVDPAURegisterVideoSurfaceWithPictureStructureNV = unsafePerformIO $ getCommand "glVDPAURegisterVideoSurfaceWithPictureStructureNV"

-- glVDPAUSurfaceAccessNV ------------------------------------------------------

glVDPAUSurfaceAccessNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> GLenum -- ^ @access@.
  -> m ()
glVDPAUSurfaceAccessNV v1 v2 = liftIO $ dyn877 ptr_glVDPAUSurfaceAccessNV v1 v2

{-# NOINLINE ptr_glVDPAUSurfaceAccessNV #-}
ptr_glVDPAUSurfaceAccessNV :: FunPtr (GLvdpauSurfaceNV -> GLenum -> IO ())
ptr_glVDPAUSurfaceAccessNV = unsafePerformIO $ getCommand "glVDPAUSurfaceAccessNV"

-- glVDPAUUnmapSurfacesNV ------------------------------------------------------

glVDPAUUnmapSurfacesNV
  :: MonadIO m
  => GLsizei -- ^ @numSurface@.
  -> Ptr GLvdpauSurfaceNV -- ^ @surfaces@ pointing to @numSurface@ elements of type @vdpauSurfaceNV@.
  -> m ()
glVDPAUUnmapSurfacesNV v1 v2 = liftIO $ dyn874 ptr_glVDPAUUnmapSurfacesNV v1 v2

{-# NOINLINE ptr_glVDPAUUnmapSurfacesNV #-}
ptr_glVDPAUUnmapSurfacesNV :: FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ())
ptr_glVDPAUUnmapSurfacesNV = unsafePerformIO $ getCommand "glVDPAUUnmapSurfacesNV"

-- glVDPAUUnregisterSurfaceNV --------------------------------------------------

glVDPAUUnregisterSurfaceNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> m ()
glVDPAUUnregisterSurfaceNV v1 = liftIO $ dyn878 ptr_glVDPAUUnregisterSurfaceNV v1

{-# NOINLINE ptr_glVDPAUUnregisterSurfaceNV #-}
ptr_glVDPAUUnregisterSurfaceNV :: FunPtr (GLvdpauSurfaceNV -> IO ())
ptr_glVDPAUUnregisterSurfaceNV = unsafePerformIO $ getCommand "glVDPAUUnregisterSurfaceNV"

-- glValidateProgram -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glValidateProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glValidateProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glValidateProgram.xhtml OpenGL 4.x>.
glValidateProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glValidateProgram v1 = liftIO $ dyn3 ptr_glValidateProgram v1

{-# NOINLINE ptr_glValidateProgram #-}
ptr_glValidateProgram :: FunPtr (GLuint -> IO ())
ptr_glValidateProgram = unsafePerformIO $ getCommand "glValidateProgram"

-- glValidateProgramARB --------------------------------------------------------

-- | This command is an alias for 'glValidateProgram'.
glValidateProgramARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> m ()
glValidateProgramARB v1 = liftIO $ dyn144 ptr_glValidateProgramARB v1

{-# NOINLINE ptr_glValidateProgramARB #-}
ptr_glValidateProgramARB :: FunPtr (GLhandleARB -> IO ())
ptr_glValidateProgramARB = unsafePerformIO $ getCommand "glValidateProgramARB"

-- glValidateProgramPipeline ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glValidateProgramPipeline.xhtml OpenGL 4.x>.
glValidateProgramPipeline
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glValidateProgramPipeline v1 = liftIO $ dyn3 ptr_glValidateProgramPipeline v1

{-# NOINLINE ptr_glValidateProgramPipeline #-}
ptr_glValidateProgramPipeline :: FunPtr (GLuint -> IO ())
ptr_glValidateProgramPipeline = unsafePerformIO $ getCommand "glValidateProgramPipeline"

-- glValidateProgramPipelineEXT ------------------------------------------------

glValidateProgramPipelineEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glValidateProgramPipelineEXT v1 = liftIO $ dyn3 ptr_glValidateProgramPipelineEXT v1

{-# NOINLINE ptr_glValidateProgramPipelineEXT #-}
ptr_glValidateProgramPipelineEXT :: FunPtr (GLuint -> IO ())
ptr_glValidateProgramPipelineEXT = unsafePerformIO $ getCommand "glValidateProgramPipelineEXT"

-- glVariantArrayObjectATI -----------------------------------------------------

glVariantArrayObjectATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @type@ of type [ScalarType](Graphics-GL-Groups.html#ScalarType).
  -> GLsizei -- ^ @stride@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> m ()
glVariantArrayObjectATI v1 v2 v3 v4 v5 = liftIO $ dyn879 ptr_glVariantArrayObjectATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVariantArrayObjectATI #-}
ptr_glVariantArrayObjectATI :: FunPtr (GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ())
ptr_glVariantArrayObjectATI = unsafePerformIO $ getCommand "glVariantArrayObjectATI"

-- glVariantPointerEXT ---------------------------------------------------------

glVariantPointerEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @type@ of type [ScalarType](Graphics-GL-Groups.html#ScalarType).
  -> GLuint -- ^ @stride@.
  -> Ptr a -- ^ @addr@ pointing to @COMPSIZE(id,type,stride)@ elements of type @a@.
  -> m ()
glVariantPointerEXT v1 v2 v3 v4 = liftIO $ dyn880 ptr_glVariantPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVariantPointerEXT #-}
ptr_glVariantPointerEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr a -> IO ())
ptr_glVariantPointerEXT = unsafePerformIO $ getCommand "glVariantPointerEXT"

-- glVariantbvEXT --------------------------------------------------------------

glVariantbvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLbyte -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLbyte@.
  -> m ()
glVariantbvEXT v1 v2 = liftIO $ dyn881 ptr_glVariantbvEXT v1 v2

{-# NOINLINE ptr_glVariantbvEXT #-}
ptr_glVariantbvEXT :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVariantbvEXT = unsafePerformIO $ getCommand "glVariantbvEXT"

-- glVariantdvEXT --------------------------------------------------------------

glVariantdvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLdouble -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLdouble@.
  -> m ()
glVariantdvEXT v1 v2 = liftIO $ dyn882 ptr_glVariantdvEXT v1 v2

{-# NOINLINE ptr_glVariantdvEXT #-}
ptr_glVariantdvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVariantdvEXT = unsafePerformIO $ getCommand "glVariantdvEXT"

-- glVariantfvEXT --------------------------------------------------------------

glVariantfvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLfloat -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLfloat@.
  -> m ()
glVariantfvEXT v1 v2 = liftIO $ dyn394 ptr_glVariantfvEXT v1 v2

{-# NOINLINE ptr_glVariantfvEXT #-}
ptr_glVariantfvEXT :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVariantfvEXT = unsafePerformIO $ getCommand "glVariantfvEXT"

-- glVariantivEXT --------------------------------------------------------------

glVariantivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLint -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLint@.
  -> m ()
glVariantivEXT v1 v2 = liftIO $ dyn741 ptr_glVariantivEXT v1 v2

{-# NOINLINE ptr_glVariantivEXT #-}
ptr_glVariantivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVariantivEXT = unsafePerformIO $ getCommand "glVariantivEXT"

-- glVariantsvEXT --------------------------------------------------------------

glVariantsvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLshort -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLshort@.
  -> m ()
glVariantsvEXT v1 v2 = liftIO $ dyn883 ptr_glVariantsvEXT v1 v2

{-# NOINLINE ptr_glVariantsvEXT #-}
ptr_glVariantsvEXT :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVariantsvEXT = unsafePerformIO $ getCommand "glVariantsvEXT"

-- glVariantubvEXT -------------------------------------------------------------

glVariantubvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLubyte -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLubyte@.
  -> m ()
glVariantubvEXT v1 v2 = liftIO $ dyn393 ptr_glVariantubvEXT v1 v2

{-# NOINLINE ptr_glVariantubvEXT #-}
ptr_glVariantubvEXT :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVariantubvEXT = unsafePerformIO $ getCommand "glVariantubvEXT"

-- glVariantuivEXT -------------------------------------------------------------

glVariantuivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLuint -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLuint@.
  -> m ()
glVariantuivEXT v1 v2 = liftIO $ dyn201 ptr_glVariantuivEXT v1 v2

{-# NOINLINE ptr_glVariantuivEXT #-}
ptr_glVariantuivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVariantuivEXT = unsafePerformIO $ getCommand "glVariantuivEXT"

-- glVariantusvEXT -------------------------------------------------------------

glVariantusvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLushort -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLushort@.
  -> m ()
glVariantusvEXT v1 v2 = liftIO $ dyn884 ptr_glVariantusvEXT v1 v2

{-# NOINLINE ptr_glVariantusvEXT #-}
ptr_glVariantusvEXT :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVariantusvEXT = unsafePerformIO $ getCommand "glVariantusvEXT"

-- glVertex2bOES ---------------------------------------------------------------

glVertex2bOES
  :: MonadIO m
  => GLbyte -- ^ @x@.
  -> GLbyte -- ^ @y@.
  -> m ()
glVertex2bOES v1 v2 = liftIO $ dyn765 ptr_glVertex2bOES v1 v2

{-# NOINLINE ptr_glVertex2bOES #-}
ptr_glVertex2bOES :: FunPtr (GLbyte -> GLbyte -> IO ())
ptr_glVertex2bOES = unsafePerformIO $ getCommand "glVertex2bOES"

-- glVertex2bvOES --------------------------------------------------------------

glVertex2bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @2@ elements of type @GLbyte@.
  -> m ()
glVertex2bvOES v1 = liftIO $ dyn40 ptr_glVertex2bvOES v1

{-# NOINLINE ptr_glVertex2bvOES #-}
ptr_glVertex2bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glVertex2bvOES = unsafePerformIO $ getCommand "glVertex2bvOES"

-- glVertex2d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2dv'.
glVertex2d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glVertex2d v1 v2 = liftIO $ dyn225 ptr_glVertex2d v1 v2

{-# NOINLINE ptr_glVertex2d #-}
ptr_glVertex2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glVertex2d = unsafePerformIO $ getCommand "glVertex2d"

-- glVertex2dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glVertex2dv v1 = liftIO $ dyn42 ptr_glVertex2dv v1

{-# NOINLINE ptr_glVertex2dv #-}
ptr_glVertex2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glVertex2dv = unsafePerformIO $ getCommand "glVertex2dv"

-- glVertex2f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2fv'.
glVertex2f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glVertex2f v1 v2 = liftIO $ dyn230 ptr_glVertex2f v1 v2

{-# NOINLINE ptr_glVertex2f #-}
ptr_glVertex2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glVertex2f = unsafePerformIO $ getCommand "glVertex2f"

-- glVertex2fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glVertex2fv v1 = liftIO $ dyn44 ptr_glVertex2fv v1

{-# NOINLINE ptr_glVertex2fv #-}
ptr_glVertex2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertex2fv = unsafePerformIO $ getCommand "glVertex2fv"

-- glVertex2hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glVertex2hvNV'.
glVertex2hNV
  :: MonadIO m
  => GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> m ()
glVertex2hNV v1 v2 = liftIO $ dyn770 ptr_glVertex2hNV v1 v2

{-# NOINLINE ptr_glVertex2hNV #-}
ptr_glVertex2hNV :: FunPtr (GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertex2hNV = unsafePerformIO $ getCommand "glVertex2hNV"

-- glVertex2hvNV ---------------------------------------------------------------

glVertex2hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @2@ elements of type @Half16NV@.
  -> m ()
glVertex2hvNV v1 = liftIO $ dyn106 ptr_glVertex2hvNV v1

{-# NOINLINE ptr_glVertex2hvNV #-}
ptr_glVertex2hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertex2hvNV = unsafePerformIO $ getCommand "glVertex2hvNV"

-- glVertex2i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2iv'.
glVertex2i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glVertex2i v1 v2 = liftIO $ dyn277 ptr_glVertex2i v1 v2

{-# NOINLINE ptr_glVertex2i #-}
ptr_glVertex2i :: FunPtr (GLint -> GLint -> IO ())
ptr_glVertex2i = unsafePerformIO $ getCommand "glVertex2i"

-- glVertex2iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glVertex2iv v1 = liftIO $ dyn46 ptr_glVertex2iv v1

{-# NOINLINE ptr_glVertex2iv #-}
ptr_glVertex2iv :: FunPtr (Ptr GLint -> IO ())
ptr_glVertex2iv = unsafePerformIO $ getCommand "glVertex2iv"

-- glVertex2s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2sv'.
glVertex2s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glVertex2s v1 v2 = liftIO $ dyn709 ptr_glVertex2s v1 v2

{-# NOINLINE ptr_glVertex2s #-}
ptr_glVertex2s :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glVertex2s = unsafePerformIO $ getCommand "glVertex2s"

-- glVertex2sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glVertex2sv v1 = liftIO $ dyn48 ptr_glVertex2sv v1

{-# NOINLINE ptr_glVertex2sv #-}
ptr_glVertex2sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glVertex2sv = unsafePerformIO $ getCommand "glVertex2sv"

-- glVertex2xOES ---------------------------------------------------------------

glVertex2xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> m ()
glVertex2xOES v1 = liftIO $ dyn87 ptr_glVertex2xOES v1

{-# NOINLINE ptr_glVertex2xOES #-}
ptr_glVertex2xOES :: FunPtr (GLfixed -> IO ())
ptr_glVertex2xOES = unsafePerformIO $ getCommand "glVertex2xOES"

-- glVertex2xvOES --------------------------------------------------------------

glVertex2xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glVertex2xvOES v1 = liftIO $ dyn114 ptr_glVertex2xvOES v1

{-# NOINLINE ptr_glVertex2xvOES #-}
ptr_glVertex2xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glVertex2xvOES = unsafePerformIO $ getCommand "glVertex2xvOES"

-- glVertex3bOES ---------------------------------------------------------------

glVertex3bOES
  :: MonadIO m
  => GLbyte -- ^ @x@.
  -> GLbyte -- ^ @y@.
  -> GLbyte -- ^ @z@.
  -> m ()
glVertex3bOES v1 v2 v3 = liftIO $ dyn39 ptr_glVertex3bOES v1 v2 v3

{-# NOINLINE ptr_glVertex3bOES #-}
ptr_glVertex3bOES :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glVertex3bOES = unsafePerformIO $ getCommand "glVertex3bOES"

-- glVertex3bvOES --------------------------------------------------------------

glVertex3bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glVertex3bvOES v1 = liftIO $ dyn40 ptr_glVertex3bvOES v1

{-# NOINLINE ptr_glVertex3bvOES #-}
ptr_glVertex3bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glVertex3bvOES = unsafePerformIO $ getCommand "glVertex3bvOES"

-- glVertex3d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3dv'.
glVertex3d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glVertex3d v1 v2 v3 = liftIO $ dyn41 ptr_glVertex3d v1 v2 v3

{-# NOINLINE ptr_glVertex3d #-}
ptr_glVertex3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertex3d = unsafePerformIO $ getCommand "glVertex3d"

-- glVertex3dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glVertex3dv v1 = liftIO $ dyn42 ptr_glVertex3dv v1

{-# NOINLINE ptr_glVertex3dv #-}
ptr_glVertex3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glVertex3dv = unsafePerformIO $ getCommand "glVertex3dv"

-- glVertex3f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3fv'.
glVertex3f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glVertex3f v1 v2 v3 = liftIO $ dyn43 ptr_glVertex3f v1 v2 v3

{-# NOINLINE ptr_glVertex3f #-}
ptr_glVertex3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertex3f = unsafePerformIO $ getCommand "glVertex3f"

-- glVertex3fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glVertex3fv v1 = liftIO $ dyn44 ptr_glVertex3fv v1

{-# NOINLINE ptr_glVertex3fv #-}
ptr_glVertex3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertex3fv = unsafePerformIO $ getCommand "glVertex3fv"

-- glVertex3hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glVertex3hvNV'.
glVertex3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> m ()
glVertex3hNV v1 v2 v3 = liftIO $ dyn105 ptr_glVertex3hNV v1 v2 v3

{-# NOINLINE ptr_glVertex3hNV #-}
ptr_glVertex3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertex3hNV = unsafePerformIO $ getCommand "glVertex3hNV"

-- glVertex3hvNV ---------------------------------------------------------------

glVertex3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glVertex3hvNV v1 = liftIO $ dyn106 ptr_glVertex3hvNV v1

{-# NOINLINE ptr_glVertex3hvNV #-}
ptr_glVertex3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertex3hvNV = unsafePerformIO $ getCommand "glVertex3hvNV"

-- glVertex3i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3iv'.
glVertex3i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glVertex3i v1 v2 v3 = liftIO $ dyn45 ptr_glVertex3i v1 v2 v3

{-# NOINLINE ptr_glVertex3i #-}
ptr_glVertex3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glVertex3i = unsafePerformIO $ getCommand "glVertex3i"

-- glVertex3iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glVertex3iv v1 = liftIO $ dyn46 ptr_glVertex3iv v1

{-# NOINLINE ptr_glVertex3iv #-}
ptr_glVertex3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glVertex3iv = unsafePerformIO $ getCommand "glVertex3iv"

-- glVertex3s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3sv'.
glVertex3s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glVertex3s v1 v2 v3 = liftIO $ dyn47 ptr_glVertex3s v1 v2 v3

{-# NOINLINE ptr_glVertex3s #-}
ptr_glVertex3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertex3s = unsafePerformIO $ getCommand "glVertex3s"

-- glVertex3sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glVertex3sv v1 = liftIO $ dyn48 ptr_glVertex3sv v1

{-# NOINLINE ptr_glVertex3sv #-}
ptr_glVertex3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glVertex3sv = unsafePerformIO $ getCommand "glVertex3sv"

-- glVertex3xOES ---------------------------------------------------------------

glVertex3xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> m ()
glVertex3xOES v1 v2 = liftIO $ dyn232 ptr_glVertex3xOES v1 v2

{-# NOINLINE ptr_glVertex3xOES #-}
ptr_glVertex3xOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glVertex3xOES = unsafePerformIO $ getCommand "glVertex3xOES"

-- glVertex3xvOES --------------------------------------------------------------

glVertex3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glVertex3xvOES v1 = liftIO $ dyn114 ptr_glVertex3xvOES v1

{-# NOINLINE ptr_glVertex3xvOES #-}
ptr_glVertex3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glVertex3xvOES = unsafePerformIO $ getCommand "glVertex3xvOES"

-- glVertex4bOES ---------------------------------------------------------------

glVertex4bOES
  :: MonadIO m
  => GLbyte -- ^ @x@.
  -> GLbyte -- ^ @y@.
  -> GLbyte -- ^ @z@.
  -> GLbyte -- ^ @w@.
  -> m ()
glVertex4bOES v1 v2 v3 v4 = liftIO $ dyn115 ptr_glVertex4bOES v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4bOES #-}
ptr_glVertex4bOES :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glVertex4bOES = unsafePerformIO $ getCommand "glVertex4bOES"

-- glVertex4bvOES --------------------------------------------------------------

glVertex4bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertex4bvOES v1 = liftIO $ dyn40 ptr_glVertex4bvOES v1

{-# NOINLINE ptr_glVertex4bvOES #-}
ptr_glVertex4bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glVertex4bvOES = unsafePerformIO $ getCommand "glVertex4bvOES"

-- glVertex4d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4dv'.
glVertex4d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> GLdouble -- ^ @w@ of type @CoordD@.
  -> m ()
glVertex4d v1 v2 v3 v4 = liftIO $ dyn116 ptr_glVertex4d v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4d #-}
ptr_glVertex4d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertex4d = unsafePerformIO $ getCommand "glVertex4d"

-- glVertex4dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glVertex4dv v1 = liftIO $ dyn42 ptr_glVertex4dv v1

{-# NOINLINE ptr_glVertex4dv #-}
ptr_glVertex4dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glVertex4dv = unsafePerformIO $ getCommand "glVertex4dv"

-- glVertex4f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4fv'.
glVertex4f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> GLfloat -- ^ @w@ of type @CoordF@.
  -> m ()
glVertex4f v1 v2 v3 v4 = liftIO $ dyn52 ptr_glVertex4f v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4f #-}
ptr_glVertex4f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertex4f = unsafePerformIO $ getCommand "glVertex4f"

-- glVertex4fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glVertex4fv v1 = liftIO $ dyn44 ptr_glVertex4fv v1

{-# NOINLINE ptr_glVertex4fv #-}
ptr_glVertex4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertex4fv = unsafePerformIO $ getCommand "glVertex4fv"

-- glVertex4hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glVertex4hvNV'.
glVertex4hNV
  :: MonadIO m
  => GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> GLhalfNV -- ^ @w@ of type @Half16NV@.
  -> m ()
glVertex4hNV v1 v2 v3 v4 = liftIO $ dyn119 ptr_glVertex4hNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4hNV #-}
ptr_glVertex4hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertex4hNV = unsafePerformIO $ getCommand "glVertex4hNV"

-- glVertex4hvNV ---------------------------------------------------------------

glVertex4hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glVertex4hvNV v1 = liftIO $ dyn106 ptr_glVertex4hvNV v1

{-# NOINLINE ptr_glVertex4hvNV #-}
ptr_glVertex4hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertex4hvNV = unsafePerformIO $ getCommand "glVertex4hvNV"

-- glVertex4i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4iv'.
glVertex4i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> GLint -- ^ @w@ of type @CoordI@.
  -> m ()
glVertex4i v1 v2 v3 v4 = liftIO $ dyn82 ptr_glVertex4i v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4i #-}
ptr_glVertex4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertex4i = unsafePerformIO $ getCommand "glVertex4i"

-- glVertex4iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glVertex4iv v1 = liftIO $ dyn46 ptr_glVertex4iv v1

{-# NOINLINE ptr_glVertex4iv #-}
ptr_glVertex4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glVertex4iv = unsafePerformIO $ getCommand "glVertex4iv"

-- glVertex4s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4sv'.
glVertex4s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> GLshort -- ^ @w@ of type @CoordS@.
  -> m ()
glVertex4s v1 v2 v3 v4 = liftIO $ dyn120 ptr_glVertex4s v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4s #-}
ptr_glVertex4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertex4s = unsafePerformIO $ getCommand "glVertex4s"

-- glVertex4sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glVertex4sv v1 = liftIO $ dyn48 ptr_glVertex4sv v1

{-# NOINLINE ptr_glVertex4sv #-}
ptr_glVertex4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glVertex4sv = unsafePerformIO $ getCommand "glVertex4sv"

-- glVertex4xOES ---------------------------------------------------------------

glVertex4xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glVertex4xOES v1 v2 v3 = liftIO $ dyn113 ptr_glVertex4xOES v1 v2 v3

{-# NOINLINE ptr_glVertex4xOES #-}
ptr_glVertex4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glVertex4xOES = unsafePerformIO $ getCommand "glVertex4xOES"

-- glVertex4xvOES --------------------------------------------------------------

glVertex4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glVertex4xvOES v1 = liftIO $ dyn114 ptr_glVertex4xvOES v1

{-# NOINLINE ptr_glVertex4xvOES #-}
ptr_glVertex4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glVertex4xvOES = unsafePerformIO $ getCommand "glVertex4xvOES"

-- glVertexArrayAttribBinding --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribBinding.xhtml OpenGL 4.x>.
glVertexArrayAttribBinding
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLuint -- ^ @bindingindex@.
  -> m ()
glVertexArrayAttribBinding v1 v2 v3 = liftIO $ dyn109 ptr_glVertexArrayAttribBinding v1 v2 v3

{-# NOINLINE ptr_glVertexArrayAttribBinding #-}
ptr_glVertexArrayAttribBinding :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayAttribBinding = unsafePerformIO $ getCommand "glVertexArrayAttribBinding"

-- glVertexArrayAttribFormat ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexArrayAttribFormat
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribType](Graphics-GL-Groups.html#VertexAttribType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayAttribFormat v1 v2 v3 v4 v5 v6 = liftIO $ dyn885 ptr_glVertexArrayAttribFormat v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayAttribFormat #-}
ptr_glVertexArrayAttribFormat :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexArrayAttribFormat = unsafePerformIO $ getCommand "glVertexArrayAttribFormat"

-- glVertexArrayAttribIFormat --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexArrayAttribIFormat
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribIType](Graphics-GL-Groups.html#VertexAttribIType).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayAttribIFormat v1 v2 v3 v4 v5 = liftIO $ dyn886 ptr_glVertexArrayAttribIFormat v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayAttribIFormat #-}
ptr_glVertexArrayAttribIFormat :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayAttribIFormat = unsafePerformIO $ getCommand "glVertexArrayAttribIFormat"

-- glVertexArrayAttribLFormat --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexArrayAttribLFormat
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribLType](Graphics-GL-Groups.html#VertexAttribLType).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayAttribLFormat v1 v2 v3 v4 v5 = liftIO $ dyn886 ptr_glVertexArrayAttribLFormat v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayAttribLFormat #-}
ptr_glVertexArrayAttribLFormat :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayAttribLFormat = unsafePerformIO $ getCommand "glVertexArrayAttribLFormat"

-- glVertexArrayBindVertexBufferEXT --------------------------------------------

glVertexArrayBindVertexBufferEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexArrayBindVertexBufferEXT v1 v2 v3 v4 v5 = liftIO $ dyn887 ptr_glVertexArrayBindVertexBufferEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayBindVertexBufferEXT #-}
ptr_glVertexArrayBindVertexBufferEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
ptr_glVertexArrayBindVertexBufferEXT = unsafePerformIO $ getCommand "glVertexArrayBindVertexBufferEXT"

-- glVertexArrayBindingDivisor -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexBindingDivisor.xhtml OpenGL 4.x>.
glVertexArrayBindingDivisor
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexArrayBindingDivisor v1 v2 v3 = liftIO $ dyn109 ptr_glVertexArrayBindingDivisor v1 v2 v3

{-# NOINLINE ptr_glVertexArrayBindingDivisor #-}
ptr_glVertexArrayBindingDivisor :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayBindingDivisor = unsafePerformIO $ getCommand "glVertexArrayBindingDivisor"

-- glVertexArrayColorOffsetEXT -------------------------------------------------

glVertexArrayColorOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayColorOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn888 ptr_glVertexArrayColorOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayColorOffsetEXT #-}
ptr_glVertexArrayColorOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayColorOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayColorOffsetEXT"

-- glVertexArrayEdgeFlagOffsetEXT ----------------------------------------------

glVertexArrayEdgeFlagOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayEdgeFlagOffsetEXT v1 v2 v3 v4 = liftIO $ dyn889 ptr_glVertexArrayEdgeFlagOffsetEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexArrayEdgeFlagOffsetEXT #-}
ptr_glVertexArrayEdgeFlagOffsetEXT :: FunPtr (GLuint -> GLuint -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayEdgeFlagOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayEdgeFlagOffsetEXT"

-- glVertexArrayElementBuffer --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexArrayElementBuffer.xhtml OpenGL 4.x>.
glVertexArrayElementBuffer
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glVertexArrayElementBuffer v1 v2 = liftIO $ dyn4 ptr_glVertexArrayElementBuffer v1 v2

{-# NOINLINE ptr_glVertexArrayElementBuffer #-}
ptr_glVertexArrayElementBuffer :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexArrayElementBuffer = unsafePerformIO $ getCommand "glVertexArrayElementBuffer"

-- glVertexArrayFogCoordOffsetEXT ----------------------------------------------

glVertexArrayFogCoordOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @type@ of type [FogCoordinatePointerType](Graphics-GL-Groups.html#FogCoordinatePointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayFogCoordOffsetEXT v1 v2 v3 v4 v5 = liftIO $ dyn890 ptr_glVertexArrayFogCoordOffsetEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayFogCoordOffsetEXT #-}
ptr_glVertexArrayFogCoordOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayFogCoordOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayFogCoordOffsetEXT"

-- glVertexArrayIndexOffsetEXT -------------------------------------------------

glVertexArrayIndexOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @type@ of type [IndexPointerType](Graphics-GL-Groups.html#IndexPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayIndexOffsetEXT v1 v2 v3 v4 v5 = liftIO $ dyn890 ptr_glVertexArrayIndexOffsetEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayIndexOffsetEXT #-}
ptr_glVertexArrayIndexOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayIndexOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayIndexOffsetEXT"

-- glVertexArrayMultiTexCoordOffsetEXT -----------------------------------------

glVertexArrayMultiTexCoordOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @texunit@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayMultiTexCoordOffsetEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn891 ptr_glVertexArrayMultiTexCoordOffsetEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexArrayMultiTexCoordOffsetEXT #-}
ptr_glVertexArrayMultiTexCoordOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayMultiTexCoordOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayMultiTexCoordOffsetEXT"

-- glVertexArrayNormalOffsetEXT ------------------------------------------------

glVertexArrayNormalOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayNormalOffsetEXT v1 v2 v3 v4 v5 = liftIO $ dyn890 ptr_glVertexArrayNormalOffsetEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayNormalOffsetEXT #-}
ptr_glVertexArrayNormalOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayNormalOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayNormalOffsetEXT"

-- glVertexArrayParameteriAPPLE ------------------------------------------------

glVertexArrayParameteriAPPLE
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [VertexArrayPNameAPPLE](Graphics-GL-Groups.html#VertexArrayPNameAPPLE).
  -> GLint -- ^ @param@.
  -> m ()
glVertexArrayParameteriAPPLE v1 v2 = liftIO $ dyn58 ptr_glVertexArrayParameteriAPPLE v1 v2

{-# NOINLINE ptr_glVertexArrayParameteriAPPLE #-}
ptr_glVertexArrayParameteriAPPLE :: FunPtr (GLenum -> GLint -> IO ())
ptr_glVertexArrayParameteriAPPLE = unsafePerformIO $ getCommand "glVertexArrayParameteriAPPLE"

-- glVertexArrayRangeAPPLE -----------------------------------------------------

glVertexArrayRangeAPPLE
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @length@ elements of type @a@.
  -> m ()
glVertexArrayRangeAPPLE v1 v2 = liftIO $ dyn271 ptr_glVertexArrayRangeAPPLE v1 v2

{-# NOINLINE ptr_glVertexArrayRangeAPPLE #-}
ptr_glVertexArrayRangeAPPLE :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glVertexArrayRangeAPPLE = unsafePerformIO $ getCommand "glVertexArrayRangeAPPLE"

-- glVertexArrayRangeNV --------------------------------------------------------

glVertexArrayRangeNV
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(length)@ elements of type @a@.
  -> m ()
glVertexArrayRangeNV v1 v2 = liftIO $ dyn271 ptr_glVertexArrayRangeNV v1 v2

{-# NOINLINE ptr_glVertexArrayRangeNV #-}
ptr_glVertexArrayRangeNV :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glVertexArrayRangeNV = unsafePerformIO $ getCommand "glVertexArrayRangeNV"

-- glVertexArraySecondaryColorOffsetEXT ----------------------------------------

glVertexArraySecondaryColorOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArraySecondaryColorOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn888 ptr_glVertexArraySecondaryColorOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArraySecondaryColorOffsetEXT #-}
ptr_glVertexArraySecondaryColorOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArraySecondaryColorOffsetEXT = unsafePerformIO $ getCommand "glVertexArraySecondaryColorOffsetEXT"

-- glVertexArrayTexCoordOffsetEXT ----------------------------------------------

glVertexArrayTexCoordOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayTexCoordOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn888 ptr_glVertexArrayTexCoordOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayTexCoordOffsetEXT #-}
ptr_glVertexArrayTexCoordOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayTexCoordOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayTexCoordOffsetEXT"

-- glVertexArrayVertexAttribBindingEXT -----------------------------------------

glVertexArrayVertexAttribBindingEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLuint -- ^ @bindingindex@.
  -> m ()
glVertexArrayVertexAttribBindingEXT v1 v2 v3 = liftIO $ dyn109 ptr_glVertexArrayVertexAttribBindingEXT v1 v2 v3

{-# NOINLINE ptr_glVertexArrayVertexAttribBindingEXT #-}
ptr_glVertexArrayVertexAttribBindingEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribBindingEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribBindingEXT"

-- glVertexArrayVertexAttribDivisorEXT -----------------------------------------

glVertexArrayVertexAttribDivisorEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexArrayVertexAttribDivisorEXT v1 v2 v3 = liftIO $ dyn109 ptr_glVertexArrayVertexAttribDivisorEXT v1 v2 v3

{-# NOINLINE ptr_glVertexArrayVertexAttribDivisorEXT #-}
ptr_glVertexArrayVertexAttribDivisorEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribDivisorEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribDivisorEXT"

-- glVertexArrayVertexAttribFormatEXT ------------------------------------------

glVertexArrayVertexAttribFormatEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribType](Graphics-GL-Groups.html#VertexAttribType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayVertexAttribFormatEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn885 ptr_glVertexArrayVertexAttribFormatEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayVertexAttribFormatEXT #-}
ptr_glVertexArrayVertexAttribFormatEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribFormatEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribFormatEXT"

-- glVertexArrayVertexAttribIFormatEXT -----------------------------------------

glVertexArrayVertexAttribIFormatEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribIType](Graphics-GL-Groups.html#VertexAttribIType).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayVertexAttribIFormatEXT v1 v2 v3 v4 v5 = liftIO $ dyn886 ptr_glVertexArrayVertexAttribIFormatEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayVertexAttribIFormatEXT #-}
ptr_glVertexArrayVertexAttribIFormatEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribIFormatEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribIFormatEXT"

-- glVertexArrayVertexAttribIOffsetEXT -----------------------------------------

glVertexArrayVertexAttribIOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribType](Graphics-GL-Groups.html#VertexAttribType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayVertexAttribIOffsetEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn892 ptr_glVertexArrayVertexAttribIOffsetEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexArrayVertexAttribIOffsetEXT #-}
ptr_glVertexArrayVertexAttribIOffsetEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexAttribIOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribIOffsetEXT"

-- glVertexArrayVertexAttribLFormatEXT -----------------------------------------

glVertexArrayVertexAttribLFormatEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribLType](Graphics-GL-Groups.html#VertexAttribLType).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayVertexAttribLFormatEXT v1 v2 v3 v4 v5 = liftIO $ dyn886 ptr_glVertexArrayVertexAttribLFormatEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayVertexAttribLFormatEXT #-}
ptr_glVertexArrayVertexAttribLFormatEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribLFormatEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribLFormatEXT"

-- glVertexArrayVertexAttribLOffsetEXT -----------------------------------------

glVertexArrayVertexAttribLOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribLType](Graphics-GL-Groups.html#VertexAttribLType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> m ()
glVertexArrayVertexAttribLOffsetEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn892 ptr_glVertexArrayVertexAttribLOffsetEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexArrayVertexAttribLOffsetEXT #-}
ptr_glVertexArrayVertexAttribLOffsetEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexAttribLOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribLOffsetEXT"

-- glVertexArrayVertexAttribOffsetEXT ------------------------------------------

glVertexArrayVertexAttribOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayVertexAttribOffsetEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn893 ptr_glVertexArrayVertexAttribOffsetEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glVertexArrayVertexAttribOffsetEXT #-}
ptr_glVertexArrayVertexAttribOffsetEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexAttribOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribOffsetEXT"

