{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F05
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

module Graphics.GL.Functions.F05 (
  glCopyTextureLevelsAPPLE,
  glCopyTextureSubImage1D,
  glCopyTextureSubImage1DEXT,
  glCopyTextureSubImage2D,
  glCopyTextureSubImage2DEXT,
  glCopyTextureSubImage3D,
  glCopyTextureSubImage3DEXT,
  glCoverFillPathInstancedNV,
  glCoverFillPathNV,
  glCoverStrokePathInstancedNV,
  glCoverStrokePathNV,
  glCoverageMaskNV,
  glCoverageModulationNV,
  glCoverageModulationTableNV,
  glCoverageOperationNV,
  glCreateBuffers,
  glCreateCommandListsNV,
  glCreateFramebuffers,
  glCreateMemoryObjectsEXT,
  glCreatePerfQueryINTEL,
  glCreateProgram,
  glCreateProgramObjectARB,
  glCreateProgramPipelines,
  glCreateProgressFenceNVX,
  glCreateQueries,
  glCreateRenderbuffers,
  glCreateSamplers,
  glCreateShader,
  glCreateShaderObjectARB,
  glCreateShaderProgramEXT,
  glCreateShaderProgramv,
  glCreateShaderProgramvEXT,
  glCreateStatesNV,
  glCreateSyncFromCLeventARB,
  glCreateTextures,
  glCreateTransformFeedbacks,
  glCreateVertexArrays,
  glCullFace,
  glCullParameterdvEXT,
  glCullParameterfvEXT,
  glCurrentPaletteMatrixARB,
  glCurrentPaletteMatrixOES,
  glDebugMessageCallback,
  glDebugMessageCallbackAMD,
  glDebugMessageCallbackARB,
  glDebugMessageCallbackKHR,
  glDebugMessageControl,
  glDebugMessageControlARB,
  glDebugMessageControlKHR,
  glDebugMessageEnableAMD,
  glDebugMessageInsert,
  glDebugMessageInsertAMD,
  glDebugMessageInsertARB,
  glDebugMessageInsertKHR,
  glDeformSGIX,
  glDeformationMap3dSGIX,
  glDeformationMap3fSGIX,
  glDeleteAsyncMarkersSGIX,
  glDeleteBuffers,
  glDeleteBuffersARB,
  glDeleteCommandListsNV,
  glDeleteFencesAPPLE,
  glDeleteFencesNV,
  glDeleteFragmentShaderATI,
  glDeleteFramebuffers,
  glDeleteFramebuffersEXT,
  glDeleteFramebuffersOES,
  glDeleteLists,
  glDeleteMemoryObjectsEXT,
  glDeleteNamedStringARB,
  glDeleteNamesAMD,
  glDeleteObjectARB,
  glDeleteOcclusionQueriesNV,
  glDeletePathsNV,
  glDeletePerfMonitorsAMD,
  glDeletePerfQueryINTEL,
  glDeleteProgram,
  glDeleteProgramPipelines,
  glDeleteProgramPipelinesEXT,
  glDeleteProgramsARB,
  glDeleteProgramsNV,
  glDeleteQueries,
  glDeleteQueriesARB,
  glDeleteQueriesEXT,
  glDeleteQueryResourceTagNV,
  glDeleteRenderbuffers,
  glDeleteRenderbuffersEXT,
  glDeleteRenderbuffersOES,
  glDeleteSamplers,
  glDeleteSemaphoresEXT,
  glDeleteShader,
  glDeleteStatesNV,
  glDeleteSync,
  glDeleteSyncAPPLE,
  glDeleteTextures,
  glDeleteTexturesEXT,
  glDeleteTransformFeedbacks,
  glDeleteTransformFeedbacksNV,
  glDeleteVertexArrays,
  glDeleteVertexArraysAPPLE
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glCopyTextureLevelsAPPLE ----------------------------------------------------

glCopyTextureLevelsAPPLE
  :: MonadIO m
  => GLuint -- ^ @destinationTexture@.
  -> GLuint -- ^ @sourceTexture@.
  -> GLint -- ^ @sourceBaseLevel@.
  -> GLsizei -- ^ @sourceLevelCount@.
  -> m ()
glCopyTextureLevelsAPPLE v1 v2 v3 v4 = liftIO $ dyn190 ptr_glCopyTextureLevelsAPPLE v1 v2 v3 v4

{-# NOINLINE ptr_glCopyTextureLevelsAPPLE #-}
ptr_glCopyTextureLevelsAPPLE :: FunPtr (GLuint -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glCopyTextureLevelsAPPLE = unsafePerformIO $ getCommand "glCopyTextureLevelsAPPLE"

-- glCopyTextureSubImage1D -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage1D.xhtml OpenGL 4.x>.
glCopyTextureSubImage1D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTextureSubImage1D v1 v2 v3 v4 v5 v6 = liftIO $ dyn191 ptr_glCopyTextureSubImage1D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyTextureSubImage1D #-}
ptr_glCopyTextureSubImage1D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTextureSubImage1D = unsafePerformIO $ getCommand "glCopyTextureSubImage1D"

-- glCopyTextureSubImage1DEXT --------------------------------------------------

glCopyTextureSubImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn192 ptr_glCopyTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyTextureSubImage1DEXT #-}
ptr_glCopyTextureSubImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTextureSubImage1DEXT = unsafePerformIO $ getCommand "glCopyTextureSubImage1DEXT"

-- glCopyTextureSubImage2D -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage2D.xhtml OpenGL 4.x>.
glCopyTextureSubImage2D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn193 ptr_glCopyTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTextureSubImage2D #-}
ptr_glCopyTextureSubImage2D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage2D = unsafePerformIO $ getCommand "glCopyTextureSubImage2D"

-- glCopyTextureSubImage2DEXT --------------------------------------------------

glCopyTextureSubImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn194 ptr_glCopyTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTextureSubImage2DEXT #-}
ptr_glCopyTextureSubImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage2DEXT = unsafePerformIO $ getCommand "glCopyTextureSubImage2DEXT"

-- glCopyTextureSubImage3D -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage3D.xhtml OpenGL 4.x>.
glCopyTextureSubImage3D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn195 ptr_glCopyTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTextureSubImage3D #-}
ptr_glCopyTextureSubImage3D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage3D = unsafePerformIO $ getCommand "glCopyTextureSubImage3D"

-- glCopyTextureSubImage3DEXT --------------------------------------------------

glCopyTextureSubImage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
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
glCopyTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn196 ptr_glCopyTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCopyTextureSubImage3DEXT #-}
ptr_glCopyTextureSubImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage3DEXT = unsafePerformIO $ getCommand "glCopyTextureSubImage3DEXT"

-- glCoverFillPathInstancedNV --------------------------------------------------

glCoverFillPathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type [PathElementType](Graphics-GL-Groups.html#PathElementType).
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type [PathCoverMode](Graphics-GL-Groups.html#PathCoverMode).
  -> GLenum -- ^ @transformType@ of type [PathTransformType](Graphics-GL-Groups.html#PathTransformType).
  -> Ptr GLfloat -- ^ @transformValues@ pointing to @COMPSIZE(numPaths,transformType)@ elements of type @GLfloat@.
  -> m ()
glCoverFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn197 ptr_glCoverFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCoverFillPathInstancedNV #-}
ptr_glCoverFillPathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glCoverFillPathInstancedNV = unsafePerformIO $ getCommand "glCoverFillPathInstancedNV"

-- glCoverFillPathNV -----------------------------------------------------------

glCoverFillPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type [PathCoverMode](Graphics-GL-Groups.html#PathCoverMode).
  -> m ()
glCoverFillPathNV v1 v2 = liftIO $ dyn18 ptr_glCoverFillPathNV v1 v2

{-# NOINLINE ptr_glCoverFillPathNV #-}
ptr_glCoverFillPathNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glCoverFillPathNV = unsafePerformIO $ getCommand "glCoverFillPathNV"

-- glCoverStrokePathInstancedNV ------------------------------------------------

glCoverStrokePathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type [PathElementType](Graphics-GL-Groups.html#PathElementType).
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type [PathCoverMode](Graphics-GL-Groups.html#PathCoverMode).
  -> GLenum -- ^ @transformType@ of type [PathTransformType](Graphics-GL-Groups.html#PathTransformType).
  -> Ptr GLfloat -- ^ @transformValues@ pointing to @COMPSIZE(numPaths,transformType)@ elements of type @GLfloat@.
  -> m ()
glCoverStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn197 ptr_glCoverStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCoverStrokePathInstancedNV #-}
ptr_glCoverStrokePathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glCoverStrokePathInstancedNV = unsafePerformIO $ getCommand "glCoverStrokePathInstancedNV"

-- glCoverStrokePathNV ---------------------------------------------------------

glCoverStrokePathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type [PathCoverMode](Graphics-GL-Groups.html#PathCoverMode).
  -> m ()
glCoverStrokePathNV v1 v2 = liftIO $ dyn18 ptr_glCoverStrokePathNV v1 v2

{-# NOINLINE ptr_glCoverStrokePathNV #-}
ptr_glCoverStrokePathNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glCoverStrokePathNV = unsafePerformIO $ getCommand "glCoverStrokePathNV"

-- glCoverageMaskNV ------------------------------------------------------------

glCoverageMaskNV
  :: MonadIO m
  => GLboolean -- ^ @mask@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glCoverageMaskNV v1 = liftIO $ dyn198 ptr_glCoverageMaskNV v1

{-# NOINLINE ptr_glCoverageMaskNV #-}
ptr_glCoverageMaskNV :: FunPtr (GLboolean -> IO ())
ptr_glCoverageMaskNV = unsafePerformIO $ getCommand "glCoverageMaskNV"

-- glCoverageModulationNV ------------------------------------------------------

glCoverageModulationNV
  :: MonadIO m
  => GLenum -- ^ @components@.
  -> m ()
glCoverageModulationNV v1 = liftIO $ dyn5 ptr_glCoverageModulationNV v1

{-# NOINLINE ptr_glCoverageModulationNV #-}
ptr_glCoverageModulationNV :: FunPtr (GLenum -> IO ())
ptr_glCoverageModulationNV = unsafePerformIO $ getCommand "glCoverageModulationNV"

-- glCoverageModulationTableNV -------------------------------------------------

glCoverageModulationTableNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @v@ pointing to @n@ elements of type @GLfloat@.
  -> m ()
glCoverageModulationTableNV v1 v2 = liftIO $ dyn199 ptr_glCoverageModulationTableNV v1 v2

{-# NOINLINE ptr_glCoverageModulationTableNV #-}
ptr_glCoverageModulationTableNV :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
ptr_glCoverageModulationTableNV = unsafePerformIO $ getCommand "glCoverageModulationTableNV"

-- glCoverageOperationNV -------------------------------------------------------

glCoverageOperationNV
  :: MonadIO m
  => GLenum -- ^ @operation@.
  -> m ()
glCoverageOperationNV v1 = liftIO $ dyn5 ptr_glCoverageOperationNV v1

{-# NOINLINE ptr_glCoverageOperationNV #-}
ptr_glCoverageOperationNV :: FunPtr (GLenum -> IO ())
ptr_glCoverageOperationNV = unsafePerformIO $ getCommand "glCoverageOperationNV"

-- glCreateBuffers -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateBuffers.xhtml OpenGL 4.x>.
glCreateBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateBuffers v1 v2 = liftIO $ dyn200 ptr_glCreateBuffers v1 v2

{-# NOINLINE ptr_glCreateBuffers #-}
ptr_glCreateBuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateBuffers = unsafePerformIO $ getCommand "glCreateBuffers"

-- glCreateCommandListsNV ------------------------------------------------------

glCreateCommandListsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @lists@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateCommandListsNV v1 v2 = liftIO $ dyn200 ptr_glCreateCommandListsNV v1 v2

{-# NOINLINE ptr_glCreateCommandListsNV #-}
ptr_glCreateCommandListsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateCommandListsNV = unsafePerformIO $ getCommand "glCreateCommandListsNV"

-- glCreateFramebuffers --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateFramebuffers.xhtml OpenGL 4.x>.
glCreateFramebuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateFramebuffers v1 v2 = liftIO $ dyn200 ptr_glCreateFramebuffers v1 v2

{-# NOINLINE ptr_glCreateFramebuffers #-}
ptr_glCreateFramebuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateFramebuffers = unsafePerformIO $ getCommand "glCreateFramebuffers"

-- glCreateMemoryObjectsEXT ----------------------------------------------------

glCreateMemoryObjectsEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @memoryObjects@.
  -> m ()
glCreateMemoryObjectsEXT v1 v2 = liftIO $ dyn200 ptr_glCreateMemoryObjectsEXT v1 v2

{-# NOINLINE ptr_glCreateMemoryObjectsEXT #-}
ptr_glCreateMemoryObjectsEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateMemoryObjectsEXT = unsafePerformIO $ getCommand "glCreateMemoryObjectsEXT"

-- glCreatePerfQueryINTEL ------------------------------------------------------

glCreatePerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> Ptr GLuint -- ^ @queryHandle@.
  -> m ()
glCreatePerfQueryINTEL v1 v2 = liftIO $ dyn201 ptr_glCreatePerfQueryINTEL v1 v2

{-# NOINLINE ptr_glCreatePerfQueryINTEL #-}
ptr_glCreatePerfQueryINTEL :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glCreatePerfQueryINTEL = unsafePerformIO $ getCommand "glCreatePerfQueryINTEL"

-- glCreateProgram -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCreateProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCreateProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCreateProgram.xhtml OpenGL 4.x>.
glCreateProgram
  :: MonadIO m
  => m GLuint
glCreateProgram = liftIO $ dyn202 ptr_glCreateProgram

{-# NOINLINE ptr_glCreateProgram #-}
ptr_glCreateProgram :: FunPtr (IO GLuint)
ptr_glCreateProgram = unsafePerformIO $ getCommand "glCreateProgram"

-- glCreateProgramObjectARB ----------------------------------------------------

-- | This command is an alias for 'glCreateProgram'.
glCreateProgramObjectARB
  :: MonadIO m
  => m GLhandleARB -- ^ of type @handleARB@.
glCreateProgramObjectARB = liftIO $ dyn203 ptr_glCreateProgramObjectARB

{-# NOINLINE ptr_glCreateProgramObjectARB #-}
ptr_glCreateProgramObjectARB :: FunPtr (IO GLhandleARB)
ptr_glCreateProgramObjectARB = unsafePerformIO $ getCommand "glCreateProgramObjectARB"

-- glCreateProgramPipelines ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateProgramPipelines.xhtml OpenGL 4.x>.
glCreateProgramPipelines
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateProgramPipelines v1 v2 = liftIO $ dyn200 ptr_glCreateProgramPipelines v1 v2

{-# NOINLINE ptr_glCreateProgramPipelines #-}
ptr_glCreateProgramPipelines :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateProgramPipelines = unsafePerformIO $ getCommand "glCreateProgramPipelines"

-- glCreateProgressFenceNVX ----------------------------------------------------

glCreateProgressFenceNVX
  :: MonadIO m
  => m GLuint
glCreateProgressFenceNVX = liftIO $ dyn202 ptr_glCreateProgressFenceNVX

{-# NOINLINE ptr_glCreateProgressFenceNVX #-}
ptr_glCreateProgressFenceNVX :: FunPtr (IO GLuint)
ptr_glCreateProgressFenceNVX = unsafePerformIO $ getCommand "glCreateProgressFenceNVX"

-- glCreateQueries -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateQueries.xhtml OpenGL 4.x>.
glCreateQueries
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateQueries v1 v2 v3 = liftIO $ dyn204 ptr_glCreateQueries v1 v2 v3

{-# NOINLINE ptr_glCreateQueries #-}
ptr_glCreateQueries :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateQueries = unsafePerformIO $ getCommand "glCreateQueries"

-- glCreateRenderbuffers -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateRenderbuffers.xhtml OpenGL 4.x>.
glCreateRenderbuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateRenderbuffers v1 v2 = liftIO $ dyn200 ptr_glCreateRenderbuffers v1 v2

{-# NOINLINE ptr_glCreateRenderbuffers #-}
ptr_glCreateRenderbuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateRenderbuffers = unsafePerformIO $ getCommand "glCreateRenderbuffers"

-- glCreateSamplers ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateSamplers.xhtml OpenGL 4.x>.
glCreateSamplers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @samplers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateSamplers v1 v2 = liftIO $ dyn200 ptr_glCreateSamplers v1 v2

{-# NOINLINE ptr_glCreateSamplers #-}
ptr_glCreateSamplers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateSamplers = unsafePerformIO $ getCommand "glCreateSamplers"

-- glCreateShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCreateShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCreateShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCreateShader.xhtml OpenGL 4.x>.
glCreateShader
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> m GLuint
glCreateShader v1 = liftIO $ dyn33 ptr_glCreateShader v1

{-# NOINLINE ptr_glCreateShader #-}
ptr_glCreateShader :: FunPtr (GLenum -> IO GLuint)
ptr_glCreateShader = unsafePerformIO $ getCommand "glCreateShader"

-- glCreateShaderObjectARB -----------------------------------------------------

-- | This command is an alias for 'glCreateShader'.
glCreateShaderObjectARB
  :: MonadIO m
  => GLenum -- ^ @shaderType@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> m GLhandleARB -- ^ of type @handleARB@.
glCreateShaderObjectARB v1 = liftIO $ dyn205 ptr_glCreateShaderObjectARB v1

{-# NOINLINE ptr_glCreateShaderObjectARB #-}
ptr_glCreateShaderObjectARB :: FunPtr (GLenum -> IO GLhandleARB)
ptr_glCreateShaderObjectARB = unsafePerformIO $ getCommand "glCreateShaderObjectARB"

-- glCreateShaderProgramEXT ----------------------------------------------------

glCreateShaderProgramEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> Ptr GLchar -- ^ @string@.
  -> m GLuint
glCreateShaderProgramEXT v1 v2 = liftIO $ dyn206 ptr_glCreateShaderProgramEXT v1 v2

{-# NOINLINE ptr_glCreateShaderProgramEXT #-}
ptr_glCreateShaderProgramEXT :: FunPtr (GLenum -> Ptr GLchar -> IO GLuint)
ptr_glCreateShaderProgramEXT = unsafePerformIO $ getCommand "glCreateShaderProgramEXT"

-- glCreateShaderProgramv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateShaderProgram.xhtml OpenGL 4.x>.
glCreateShaderProgramv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @strings@ pointing to @count@ elements of type @Ptr GLchar@.
  -> m GLuint
glCreateShaderProgramv v1 v2 v3 = liftIO $ dyn207 ptr_glCreateShaderProgramv v1 v2 v3

{-# NOINLINE ptr_glCreateShaderProgramv #-}
ptr_glCreateShaderProgramv :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint)
ptr_glCreateShaderProgramv = unsafePerformIO $ getCommand "glCreateShaderProgramv"

-- glCreateShaderProgramvEXT ---------------------------------------------------

glCreateShaderProgramvEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @strings@ pointing to @count@ elements of type @Ptr GLchar@.
  -> m GLuint
glCreateShaderProgramvEXT v1 v2 v3 = liftIO $ dyn207 ptr_glCreateShaderProgramvEXT v1 v2 v3

{-# NOINLINE ptr_glCreateShaderProgramvEXT #-}
ptr_glCreateShaderProgramvEXT :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint)
ptr_glCreateShaderProgramvEXT = unsafePerformIO $ getCommand "glCreateShaderProgramvEXT"

-- glCreateStatesNV ------------------------------------------------------------

glCreateStatesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @states@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateStatesNV v1 v2 = liftIO $ dyn200 ptr_glCreateStatesNV v1 v2

{-# NOINLINE ptr_glCreateStatesNV #-}
ptr_glCreateStatesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateStatesNV = unsafePerformIO $ getCommand "glCreateStatesNV"

-- glCreateSyncFromCLeventARB --------------------------------------------------

glCreateSyncFromCLeventARB
  :: MonadIO m
  => Ptr a -- ^ @context@ pointing to elements of type @cl_context@.
  -> Ptr b -- ^ @event@ pointing to elements of type @cl_event@.
  -> GLbitfield -- ^ @flags@.
  -> m GLsync -- ^ of type @sync@.
glCreateSyncFromCLeventARB v1 v2 v3 = liftIO $ dyn208 ptr_glCreateSyncFromCLeventARB v1 v2 v3

{-# NOINLINE ptr_glCreateSyncFromCLeventARB #-}
ptr_glCreateSyncFromCLeventARB :: FunPtr (Ptr a -> Ptr b -> GLbitfield -> IO GLsync)
ptr_glCreateSyncFromCLeventARB = unsafePerformIO $ getCommand "glCreateSyncFromCLeventARB"

-- glCreateTextures ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateTextures.xhtml OpenGL 4.x>.
glCreateTextures
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateTextures v1 v2 v3 = liftIO $ dyn204 ptr_glCreateTextures v1 v2 v3

{-# NOINLINE ptr_glCreateTextures #-}
ptr_glCreateTextures :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateTextures = unsafePerformIO $ getCommand "glCreateTextures"

-- glCreateTransformFeedbacks --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateTransformFeedbacks.xhtml OpenGL 4.x>.
glCreateTransformFeedbacks
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateTransformFeedbacks v1 v2 = liftIO $ dyn200 ptr_glCreateTransformFeedbacks v1 v2

{-# NOINLINE ptr_glCreateTransformFeedbacks #-}
ptr_glCreateTransformFeedbacks :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateTransformFeedbacks = unsafePerformIO $ getCommand "glCreateTransformFeedbacks"

-- glCreateVertexArrays --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateVertexArrays.xhtml OpenGL 4.x>.
glCreateVertexArrays
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glCreateVertexArrays v1 v2 = liftIO $ dyn200 ptr_glCreateVertexArrays v1 v2

{-# NOINLINE ptr_glCreateVertexArrays #-}
ptr_glCreateVertexArrays :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateVertexArrays = unsafePerformIO $ getCommand "glCreateVertexArrays"

-- glCullFace ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCullFace.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCullFace.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCullFace.xhtml OpenGL 4.x>.
glCullFace
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [CullFaceMode](Graphics-GL-Groups.html#CullFaceMode).
  -> m ()
glCullFace v1 = liftIO $ dyn5 ptr_glCullFace v1

{-# NOINLINE ptr_glCullFace #-}
ptr_glCullFace :: FunPtr (GLenum -> IO ())
ptr_glCullFace = unsafePerformIO $ getCommand "glCullFace"

-- glCullParameterdvEXT --------------------------------------------------------

glCullParameterdvEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [CullParameterEXT](Graphics-GL-Groups.html#CullParameterEXT).
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glCullParameterdvEXT v1 v2 = liftIO $ dyn100 ptr_glCullParameterdvEXT v1 v2

{-# NOINLINE ptr_glCullParameterdvEXT #-}
ptr_glCullParameterdvEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glCullParameterdvEXT = unsafePerformIO $ getCommand "glCullParameterdvEXT"

-- glCullParameterfvEXT --------------------------------------------------------

glCullParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [CullParameterEXT](Graphics-GL-Groups.html#CullParameterEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glCullParameterfvEXT v1 v2 = liftIO $ dyn101 ptr_glCullParameterfvEXT v1 v2

{-# NOINLINE ptr_glCullParameterfvEXT #-}
ptr_glCullParameterfvEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glCullParameterfvEXT = unsafePerformIO $ getCommand "glCullParameterfvEXT"

-- glCurrentPaletteMatrixARB ---------------------------------------------------

glCurrentPaletteMatrixARB
  :: MonadIO m
  => GLint -- ^ @index@.
  -> m ()
glCurrentPaletteMatrixARB v1 = liftIO $ dyn13 ptr_glCurrentPaletteMatrixARB v1

{-# NOINLINE ptr_glCurrentPaletteMatrixARB #-}
ptr_glCurrentPaletteMatrixARB :: FunPtr (GLint -> IO ())
ptr_glCurrentPaletteMatrixARB = unsafePerformIO $ getCommand "glCurrentPaletteMatrixARB"

-- glCurrentPaletteMatrixOES ---------------------------------------------------

glCurrentPaletteMatrixOES
  :: MonadIO m
  => GLuint -- ^ @matrixpaletteindex@.
  -> m ()
glCurrentPaletteMatrixOES v1 = liftIO $ dyn3 ptr_glCurrentPaletteMatrixOES v1

{-# NOINLINE ptr_glCurrentPaletteMatrixOES #-}
ptr_glCurrentPaletteMatrixOES :: FunPtr (GLuint -> IO ())
ptr_glCurrentPaletteMatrixOES = unsafePerformIO $ getCommand "glCurrentPaletteMatrixOES"

-- glDebugMessageCallback ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDebugMessageCallback.xhtml OpenGL 4.x>.
glDebugMessageCallback
  :: MonadIO m
  => GLDEBUGPROC -- ^ @callback@.
  -> Ptr a -- ^ @userParam@.
  -> m ()
glDebugMessageCallback v1 v2 = liftIO $ dyn209 ptr_glDebugMessageCallback v1 v2

{-# NOINLINE ptr_glDebugMessageCallback #-}
ptr_glDebugMessageCallback :: FunPtr (GLDEBUGPROC -> Ptr a -> IO ())
ptr_glDebugMessageCallback = unsafePerformIO $ getCommand "glDebugMessageCallback"

-- glDebugMessageCallbackAMD ---------------------------------------------------

glDebugMessageCallbackAMD
  :: MonadIO m
  => GLDEBUGPROCAMD -- ^ @callback@.
  -> Ptr a -- ^ @userParam@.
  -> m ()
glDebugMessageCallbackAMD v1 v2 = liftIO $ dyn210 ptr_glDebugMessageCallbackAMD v1 v2

{-# NOINLINE ptr_glDebugMessageCallbackAMD #-}
ptr_glDebugMessageCallbackAMD :: FunPtr (GLDEBUGPROCAMD -> Ptr a -> IO ())
ptr_glDebugMessageCallbackAMD = unsafePerformIO $ getCommand "glDebugMessageCallbackAMD"

-- glDebugMessageCallbackARB ---------------------------------------------------

-- | This command is an alias for 'glDebugMessageCallback'.
glDebugMessageCallbackARB
  :: MonadIO m
  => GLDEBUGPROCARB -- ^ @callback@.
  -> Ptr a -- ^ @userParam@ pointing to @COMPSIZE(callback)@ elements of type @a@.
  -> m ()
glDebugMessageCallbackARB v1 v2 = liftIO $ dyn211 ptr_glDebugMessageCallbackARB v1 v2

{-# NOINLINE ptr_glDebugMessageCallbackARB #-}
ptr_glDebugMessageCallbackARB :: FunPtr (GLDEBUGPROCARB -> Ptr a -> IO ())
ptr_glDebugMessageCallbackARB = unsafePerformIO $ getCommand "glDebugMessageCallbackARB"

-- glDebugMessageCallbackKHR ---------------------------------------------------

-- | This command is an alias for 'glDebugMessageCallback'.
glDebugMessageCallbackKHR
  :: MonadIO m
  => GLDEBUGPROCKHR -- ^ @callback@.
  -> Ptr a -- ^ @userParam@.
  -> m ()
glDebugMessageCallbackKHR v1 v2 = liftIO $ dyn212 ptr_glDebugMessageCallbackKHR v1 v2

{-# NOINLINE ptr_glDebugMessageCallbackKHR #-}
ptr_glDebugMessageCallbackKHR :: FunPtr (GLDEBUGPROCKHR -> Ptr a -> IO ())
ptr_glDebugMessageCallbackKHR = unsafePerformIO $ getCommand "glDebugMessageCallbackKHR"

-- glDebugMessageControl -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDebugMessageControl.xhtml OpenGL 4.x>.
glDebugMessageControl
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLenum -- ^ @type@ of type [DebugType](Graphics-GL-Groups.html#DebugType).
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> GLboolean -- ^ @enabled@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDebugMessageControl v1 v2 v3 v4 v5 v6 = liftIO $ dyn213 ptr_glDebugMessageControl v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageControl #-}
ptr_glDebugMessageControl :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageControl = unsafePerformIO $ getCommand "glDebugMessageControl"

-- glDebugMessageControlARB ----------------------------------------------------

-- | This command is an alias for 'glDebugMessageControl'.
glDebugMessageControlARB
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLenum -- ^ @type@ of type [DebugType](Graphics-GL-Groups.html#DebugType).
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> GLboolean -- ^ @enabled@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDebugMessageControlARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn213 ptr_glDebugMessageControlARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageControlARB #-}
ptr_glDebugMessageControlARB :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageControlARB = unsafePerformIO $ getCommand "glDebugMessageControlARB"

-- glDebugMessageControlKHR ----------------------------------------------------

-- | This command is an alias for 'glDebugMessageControl'.
glDebugMessageControlKHR
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLenum -- ^ @type@ of type [DebugType](Graphics-GL-Groups.html#DebugType).
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@.
  -> GLboolean -- ^ @enabled@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDebugMessageControlKHR v1 v2 v3 v4 v5 v6 = liftIO $ dyn213 ptr_glDebugMessageControlKHR v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageControlKHR #-}
ptr_glDebugMessageControlKHR :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageControlKHR = unsafePerformIO $ getCommand "glDebugMessageControlKHR"

-- glDebugMessageEnableAMD -----------------------------------------------------

glDebugMessageEnableAMD
  :: MonadIO m
  => GLenum -- ^ @category@.
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> GLboolean -- ^ @enabled@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDebugMessageEnableAMD v1 v2 v3 v4 v5 = liftIO $ dyn214 ptr_glDebugMessageEnableAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDebugMessageEnableAMD #-}
ptr_glDebugMessageEnableAMD :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageEnableAMD = unsafePerformIO $ getCommand "glDebugMessageEnableAMD"

-- glDebugMessageInsert --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDebugMessageInsert.xhtml OpenGL 4.x>.
glDebugMessageInsert
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLenum -- ^ @type@ of type [DebugType](Graphics-GL-Groups.html#DebugType).
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@ pointing to @COMPSIZE(buf,length)@ elements of type @GLchar@.
  -> m ()
glDebugMessageInsert v1 v2 v3 v4 v5 v6 = liftIO $ dyn215 ptr_glDebugMessageInsert v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageInsert #-}
ptr_glDebugMessageInsert :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsert = unsafePerformIO $ getCommand "glDebugMessageInsert"

-- glDebugMessageInsertAMD -----------------------------------------------------

glDebugMessageInsertAMD
  :: MonadIO m
  => GLenum -- ^ @category@.
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@ pointing to @length@ elements of type @GLchar@.
  -> m ()
glDebugMessageInsertAMD v1 v2 v3 v4 v5 = liftIO $ dyn216 ptr_glDebugMessageInsertAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDebugMessageInsertAMD #-}
ptr_glDebugMessageInsertAMD :: FunPtr (GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsertAMD = unsafePerformIO $ getCommand "glDebugMessageInsertAMD"

-- glDebugMessageInsertARB -----------------------------------------------------

-- | This command is an alias for 'glDebugMessageInsert'.
glDebugMessageInsertARB
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLenum -- ^ @type@ of type [DebugType](Graphics-GL-Groups.html#DebugType).
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@ pointing to @length@ elements of type @GLchar@.
  -> m ()
glDebugMessageInsertARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn215 ptr_glDebugMessageInsertARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageInsertARB #-}
ptr_glDebugMessageInsertARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsertARB = unsafePerformIO $ getCommand "glDebugMessageInsertARB"

-- glDebugMessageInsertKHR -----------------------------------------------------

-- | This command is an alias for 'glDebugMessageInsert'.
glDebugMessageInsertKHR
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLenum -- ^ @type@ of type [DebugType](Graphics-GL-Groups.html#DebugType).
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @severity@ of type [DebugSeverity](Graphics-GL-Groups.html#DebugSeverity).
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@.
  -> m ()
glDebugMessageInsertKHR v1 v2 v3 v4 v5 v6 = liftIO $ dyn215 ptr_glDebugMessageInsertKHR v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageInsertKHR #-}
ptr_glDebugMessageInsertKHR :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsertKHR = unsafePerformIO $ getCommand "glDebugMessageInsertKHR"

-- glDeformSGIX ----------------------------------------------------------------

glDeformSGIX
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [FfdMaskSGIX](Graphics-GL-Groups.html#FfdMaskSGIX).
  -> m ()
glDeformSGIX v1 = liftIO $ dyn75 ptr_glDeformSGIX v1

{-# NOINLINE ptr_glDeformSGIX #-}
ptr_glDeformSGIX :: FunPtr (GLbitfield -> IO ())
ptr_glDeformSGIX = unsafePerformIO $ getCommand "glDeformSGIX"

-- glDeformationMap3dSGIX ------------------------------------------------------

glDeformationMap3dSGIX
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FfdTargetSGIX](Graphics-GL-Groups.html#FfdTargetSGIX).
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @w1@ of type @CoordD@.
  -> GLdouble -- ^ @w2@ of type @CoordD@.
  -> GLint -- ^ @wstride@.
  -> GLint -- ^ @worder@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder,wstride,worder)@ elements of type @CoordD@.
  -> m ()
glDeformationMap3dSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 = liftIO $ dyn217 ptr_glDeformationMap3dSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14

{-# NOINLINE ptr_glDeformationMap3dSGIX #-}
ptr_glDeformationMap3dSGIX :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glDeformationMap3dSGIX = unsafePerformIO $ getCommand "glDeformationMap3dSGIX"

-- glDeformationMap3fSGIX ------------------------------------------------------

glDeformationMap3fSGIX
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FfdTargetSGIX](Graphics-GL-Groups.html#FfdTargetSGIX).
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @w1@ of type @CoordF@.
  -> GLfloat -- ^ @w2@ of type @CoordF@.
  -> GLint -- ^ @wstride@.
  -> GLint -- ^ @worder@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder,wstride,worder)@ elements of type @CoordF@.
  -> m ()
glDeformationMap3fSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 = liftIO $ dyn218 ptr_glDeformationMap3fSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14

{-# NOINLINE ptr_glDeformationMap3fSGIX #-}
ptr_glDeformationMap3fSGIX :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glDeformationMap3fSGIX = unsafePerformIO $ getCommand "glDeformationMap3fSGIX"

-- glDeleteAsyncMarkersSGIX ----------------------------------------------------

glDeleteAsyncMarkersSGIX
  :: MonadIO m
  => GLuint -- ^ @marker@.
  -> GLsizei -- ^ @range@.
  -> m ()
glDeleteAsyncMarkersSGIX v1 v2 = liftIO $ dyn219 ptr_glDeleteAsyncMarkersSGIX v1 v2

{-# NOINLINE ptr_glDeleteAsyncMarkersSGIX #-}
ptr_glDeleteAsyncMarkersSGIX :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glDeleteAsyncMarkersSGIX = unsafePerformIO $ getCommand "glDeleteAsyncMarkersSGIX"

-- glDeleteBuffers -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteBuffers.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteBuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteBuffers.xhtml OpenGL 4.x>.
glDeleteBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteBuffers v1 v2 = liftIO $ dyn200 ptr_glDeleteBuffers v1 v2

{-# NOINLINE ptr_glDeleteBuffers #-}
ptr_glDeleteBuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteBuffers = unsafePerformIO $ getCommand "glDeleteBuffers"

-- glDeleteBuffersARB ----------------------------------------------------------

-- | This command is an alias for 'glDeleteBuffers'.
glDeleteBuffersARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteBuffersARB v1 v2 = liftIO $ dyn200 ptr_glDeleteBuffersARB v1 v2

{-# NOINLINE ptr_glDeleteBuffersARB #-}
ptr_glDeleteBuffersARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteBuffersARB = unsafePerformIO $ getCommand "glDeleteBuffersARB"

-- glDeleteCommandListsNV ------------------------------------------------------

glDeleteCommandListsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @lists@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteCommandListsNV v1 v2 = liftIO $ dyn200 ptr_glDeleteCommandListsNV v1 v2

{-# NOINLINE ptr_glDeleteCommandListsNV #-}
ptr_glDeleteCommandListsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteCommandListsNV = unsafePerformIO $ getCommand "glDeleteCommandListsNV"

-- glDeleteFencesAPPLE ---------------------------------------------------------

glDeleteFencesAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glDeleteFencesAPPLE v1 v2 = liftIO $ dyn200 ptr_glDeleteFencesAPPLE v1 v2

{-# NOINLINE ptr_glDeleteFencesAPPLE #-}
ptr_glDeleteFencesAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFencesAPPLE = unsafePerformIO $ getCommand "glDeleteFencesAPPLE"

-- glDeleteFencesNV ------------------------------------------------------------

glDeleteFencesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glDeleteFencesNV v1 v2 = liftIO $ dyn200 ptr_glDeleteFencesNV v1 v2

{-# NOINLINE ptr_glDeleteFencesNV #-}
ptr_glDeleteFencesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFencesNV = unsafePerformIO $ getCommand "glDeleteFencesNV"

-- glDeleteFragmentShaderATI ---------------------------------------------------

glDeleteFragmentShaderATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glDeleteFragmentShaderATI v1 = liftIO $ dyn3 ptr_glDeleteFragmentShaderATI v1

{-# NOINLINE ptr_glDeleteFragmentShaderATI #-}
ptr_glDeleteFragmentShaderATI :: FunPtr (GLuint -> IO ())
ptr_glDeleteFragmentShaderATI = unsafePerformIO $ getCommand "glDeleteFragmentShaderATI"

-- glDeleteFramebuffers --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteFramebuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteFramebuffers.xhtml OpenGL 4.x>.
glDeleteFramebuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteFramebuffers v1 v2 = liftIO $ dyn200 ptr_glDeleteFramebuffers v1 v2

{-# NOINLINE ptr_glDeleteFramebuffers #-}
ptr_glDeleteFramebuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFramebuffers = unsafePerformIO $ getCommand "glDeleteFramebuffers"

-- glDeleteFramebuffersEXT -----------------------------------------------------

-- | This command is an alias for 'glDeleteFramebuffers'.
glDeleteFramebuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteFramebuffersEXT v1 v2 = liftIO $ dyn200 ptr_glDeleteFramebuffersEXT v1 v2

{-# NOINLINE ptr_glDeleteFramebuffersEXT #-}
ptr_glDeleteFramebuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFramebuffersEXT = unsafePerformIO $ getCommand "glDeleteFramebuffersEXT"

-- glDeleteFramebuffersOES -----------------------------------------------------

glDeleteFramebuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteFramebuffersOES v1 v2 = liftIO $ dyn200 ptr_glDeleteFramebuffersOES v1 v2

{-# NOINLINE ptr_glDeleteFramebuffersOES #-}
ptr_glDeleteFramebuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFramebuffersOES = unsafePerformIO $ getCommand "glDeleteFramebuffersOES"

-- glDeleteLists ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteLists.xml OpenGL 2.x>.
glDeleteLists
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLsizei -- ^ @range@.
  -> m ()
glDeleteLists v1 v2 = liftIO $ dyn219 ptr_glDeleteLists v1 v2

{-# NOINLINE ptr_glDeleteLists #-}
ptr_glDeleteLists :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glDeleteLists = unsafePerformIO $ getCommand "glDeleteLists"

-- glDeleteMemoryObjectsEXT ----------------------------------------------------

glDeleteMemoryObjectsEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @memoryObjects@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteMemoryObjectsEXT v1 v2 = liftIO $ dyn200 ptr_glDeleteMemoryObjectsEXT v1 v2

{-# NOINLINE ptr_glDeleteMemoryObjectsEXT #-}
ptr_glDeleteMemoryObjectsEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteMemoryObjectsEXT = unsafePerformIO $ getCommand "glDeleteMemoryObjectsEXT"

-- glDeleteNamedStringARB ------------------------------------------------------

glDeleteNamedStringARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> m ()
glDeleteNamedStringARB v1 v2 = liftIO $ dyn220 ptr_glDeleteNamedStringARB v1 v2

{-# NOINLINE ptr_glDeleteNamedStringARB #-}
ptr_glDeleteNamedStringARB :: FunPtr (GLint -> Ptr GLchar -> IO ())
ptr_glDeleteNamedStringARB = unsafePerformIO $ getCommand "glDeleteNamedStringARB"

-- glDeleteNamesAMD ------------------------------------------------------------

glDeleteNamesAMD
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @num@.
  -> Ptr GLuint -- ^ @names@ pointing to @num@ elements of type @GLuint@.
  -> m ()
glDeleteNamesAMD v1 v2 v3 = liftIO $ dyn221 ptr_glDeleteNamesAMD v1 v2 v3

{-# NOINLINE ptr_glDeleteNamesAMD #-}
ptr_glDeleteNamesAMD :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glDeleteNamesAMD = unsafePerformIO $ getCommand "glDeleteNamesAMD"

-- glDeleteObjectARB -----------------------------------------------------------

glDeleteObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> m ()
glDeleteObjectARB v1 = liftIO $ dyn144 ptr_glDeleteObjectARB v1

{-# NOINLINE ptr_glDeleteObjectARB #-}
ptr_glDeleteObjectARB :: FunPtr (GLhandleARB -> IO ())
ptr_glDeleteObjectARB = unsafePerformIO $ getCommand "glDeleteObjectARB"

-- glDeleteOcclusionQueriesNV --------------------------------------------------

glDeleteOcclusionQueriesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteOcclusionQueriesNV v1 v2 = liftIO $ dyn200 ptr_glDeleteOcclusionQueriesNV v1 v2

{-# NOINLINE ptr_glDeleteOcclusionQueriesNV #-}
ptr_glDeleteOcclusionQueriesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteOcclusionQueriesNV = unsafePerformIO $ getCommand "glDeleteOcclusionQueriesNV"

-- glDeletePathsNV -------------------------------------------------------------

glDeletePathsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @range@.
  -> m ()
glDeletePathsNV v1 v2 = liftIO $ dyn219 ptr_glDeletePathsNV v1 v2

{-# NOINLINE ptr_glDeletePathsNV #-}
ptr_glDeletePathsNV :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glDeletePathsNV = unsafePerformIO $ getCommand "glDeletePathsNV"

-- glDeletePerfMonitorsAMD -----------------------------------------------------

glDeletePerfMonitorsAMD
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @monitors@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeletePerfMonitorsAMD v1 v2 = liftIO $ dyn200 ptr_glDeletePerfMonitorsAMD v1 v2

{-# NOINLINE ptr_glDeletePerfMonitorsAMD #-}
ptr_glDeletePerfMonitorsAMD :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeletePerfMonitorsAMD = unsafePerformIO $ getCommand "glDeletePerfMonitorsAMD"

-- glDeletePerfQueryINTEL ------------------------------------------------------

glDeletePerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> m ()
glDeletePerfQueryINTEL v1 = liftIO $ dyn3 ptr_glDeletePerfQueryINTEL v1

{-# NOINLINE ptr_glDeletePerfQueryINTEL #-}
ptr_glDeletePerfQueryINTEL :: FunPtr (GLuint -> IO ())
ptr_glDeletePerfQueryINTEL = unsafePerformIO $ getCommand "glDeletePerfQueryINTEL"

-- glDeleteProgram -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteProgram.xhtml OpenGL 4.x>.
glDeleteProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glDeleteProgram v1 = liftIO $ dyn3 ptr_glDeleteProgram v1

{-# NOINLINE ptr_glDeleteProgram #-}
ptr_glDeleteProgram :: FunPtr (GLuint -> IO ())
ptr_glDeleteProgram = unsafePerformIO $ getCommand "glDeleteProgram"

-- glDeleteProgramPipelines ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDeleteProgramPipelines.xhtml OpenGL 4.x>.
glDeleteProgramPipelines
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramPipelines v1 v2 = liftIO $ dyn200 ptr_glDeleteProgramPipelines v1 v2

{-# NOINLINE ptr_glDeleteProgramPipelines #-}
ptr_glDeleteProgramPipelines :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramPipelines = unsafePerformIO $ getCommand "glDeleteProgramPipelines"

-- glDeleteProgramPipelinesEXT -------------------------------------------------

glDeleteProgramPipelinesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramPipelinesEXT v1 v2 = liftIO $ dyn200 ptr_glDeleteProgramPipelinesEXT v1 v2

{-# NOINLINE ptr_glDeleteProgramPipelinesEXT #-}
ptr_glDeleteProgramPipelinesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramPipelinesEXT = unsafePerformIO $ getCommand "glDeleteProgramPipelinesEXT"

-- glDeleteProgramsARB ---------------------------------------------------------

glDeleteProgramsARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramsARB v1 v2 = liftIO $ dyn200 ptr_glDeleteProgramsARB v1 v2

{-# NOINLINE ptr_glDeleteProgramsARB #-}
ptr_glDeleteProgramsARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramsARB = unsafePerformIO $ getCommand "glDeleteProgramsARB"

-- glDeleteProgramsNV ----------------------------------------------------------

-- | This command is an alias for 'glDeleteProgramsARB'.
glDeleteProgramsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramsNV v1 v2 = liftIO $ dyn200 ptr_glDeleteProgramsNV v1 v2

{-# NOINLINE ptr_glDeleteProgramsNV #-}
ptr_glDeleteProgramsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramsNV = unsafePerformIO $ getCommand "glDeleteProgramsNV"

-- glDeleteQueries -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteQueries.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteQueries.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteQueries.xhtml OpenGL 4.x>.
glDeleteQueries
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteQueries v1 v2 = liftIO $ dyn200 ptr_glDeleteQueries v1 v2

{-# NOINLINE ptr_glDeleteQueries #-}
ptr_glDeleteQueries :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteQueries = unsafePerformIO $ getCommand "glDeleteQueries"

-- glDeleteQueriesARB ----------------------------------------------------------

-- | This command is an alias for 'glDeleteQueries'.
glDeleteQueriesARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteQueriesARB v1 v2 = liftIO $ dyn200 ptr_glDeleteQueriesARB v1 v2

{-# NOINLINE ptr_glDeleteQueriesARB #-}
ptr_glDeleteQueriesARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteQueriesARB = unsafePerformIO $ getCommand "glDeleteQueriesARB"

-- glDeleteQueriesEXT ----------------------------------------------------------

glDeleteQueriesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteQueriesEXT v1 v2 = liftIO $ dyn200 ptr_glDeleteQueriesEXT v1 v2

{-# NOINLINE ptr_glDeleteQueriesEXT #-}
ptr_glDeleteQueriesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteQueriesEXT = unsafePerformIO $ getCommand "glDeleteQueriesEXT"

-- glDeleteQueryResourceTagNV --------------------------------------------------

glDeleteQueryResourceTagNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLint -- ^ @tagIds@ pointing to @n@ elements of type @GLint@.
  -> m ()
glDeleteQueryResourceTagNV v1 v2 = liftIO $ dyn222 ptr_glDeleteQueryResourceTagNV v1 v2

{-# NOINLINE ptr_glDeleteQueryResourceTagNV #-}
ptr_glDeleteQueryResourceTagNV :: FunPtr (GLsizei -> Ptr GLint -> IO ())
ptr_glDeleteQueryResourceTagNV = unsafePerformIO $ getCommand "glDeleteQueryResourceTagNV"

-- glDeleteRenderbuffers -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteRenderbuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteRenderbuffers.xhtml OpenGL 4.x>.
glDeleteRenderbuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteRenderbuffers v1 v2 = liftIO $ dyn200 ptr_glDeleteRenderbuffers v1 v2

{-# NOINLINE ptr_glDeleteRenderbuffers #-}
ptr_glDeleteRenderbuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteRenderbuffers = unsafePerformIO $ getCommand "glDeleteRenderbuffers"

-- glDeleteRenderbuffersEXT ----------------------------------------------------

-- | This command is an alias for 'glDeleteRenderbuffers'.
glDeleteRenderbuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteRenderbuffersEXT v1 v2 = liftIO $ dyn200 ptr_glDeleteRenderbuffersEXT v1 v2

{-# NOINLINE ptr_glDeleteRenderbuffersEXT #-}
ptr_glDeleteRenderbuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteRenderbuffersEXT = unsafePerformIO $ getCommand "glDeleteRenderbuffersEXT"

-- glDeleteRenderbuffersOES ----------------------------------------------------

glDeleteRenderbuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteRenderbuffersOES v1 v2 = liftIO $ dyn200 ptr_glDeleteRenderbuffersOES v1 v2

{-# NOINLINE ptr_glDeleteRenderbuffersOES #-}
ptr_glDeleteRenderbuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteRenderbuffersOES = unsafePerformIO $ getCommand "glDeleteRenderbuffersOES"

-- glDeleteSamplers ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteSamplers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteSamplers.xhtml OpenGL 4.x>.
glDeleteSamplers
  :: MonadIO m
  => GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @samplers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glDeleteSamplers v1 v2 = liftIO $ dyn200 ptr_glDeleteSamplers v1 v2

{-# NOINLINE ptr_glDeleteSamplers #-}
ptr_glDeleteSamplers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteSamplers = unsafePerformIO $ getCommand "glDeleteSamplers"

-- glDeleteSemaphoresEXT -------------------------------------------------------

glDeleteSemaphoresEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @semaphores@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteSemaphoresEXT v1 v2 = liftIO $ dyn200 ptr_glDeleteSemaphoresEXT v1 v2

{-# NOINLINE ptr_glDeleteSemaphoresEXT #-}
ptr_glDeleteSemaphoresEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteSemaphoresEXT = unsafePerformIO $ getCommand "glDeleteSemaphoresEXT"

-- glDeleteShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteShader.xhtml OpenGL 4.x>.
glDeleteShader
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> m ()
glDeleteShader v1 = liftIO $ dyn3 ptr_glDeleteShader v1

{-# NOINLINE ptr_glDeleteShader #-}
ptr_glDeleteShader :: FunPtr (GLuint -> IO ())
ptr_glDeleteShader = unsafePerformIO $ getCommand "glDeleteShader"

-- glDeleteStatesNV ------------------------------------------------------------

glDeleteStatesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @states@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteStatesNV v1 v2 = liftIO $ dyn200 ptr_glDeleteStatesNV v1 v2

{-# NOINLINE ptr_glDeleteStatesNV #-}
ptr_glDeleteStatesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteStatesNV = unsafePerformIO $ getCommand "glDeleteStatesNV"

-- glDeleteSync ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteSync.xhtml OpenGL 4.x>.
glDeleteSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> m ()
glDeleteSync v1 = liftIO $ dyn223 ptr_glDeleteSync v1

{-# NOINLINE ptr_glDeleteSync #-}
ptr_glDeleteSync :: FunPtr (GLsync -> IO ())
ptr_glDeleteSync = unsafePerformIO $ getCommand "glDeleteSync"

-- glDeleteSyncAPPLE -----------------------------------------------------------

-- | This command is an alias for 'glDeleteSync'.
glDeleteSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> m ()
glDeleteSyncAPPLE v1 = liftIO $ dyn223 ptr_glDeleteSyncAPPLE v1

{-# NOINLINE ptr_glDeleteSyncAPPLE #-}
ptr_glDeleteSyncAPPLE :: FunPtr (GLsync -> IO ())
ptr_glDeleteSyncAPPLE = unsafePerformIO $ getCommand "glDeleteSyncAPPLE"

-- glDeleteTextures ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteTextures.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteTextures.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteTextures.xhtml OpenGL 4.x>.
glDeleteTextures
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glDeleteTextures v1 v2 = liftIO $ dyn200 ptr_glDeleteTextures v1 v2

{-# NOINLINE ptr_glDeleteTextures #-}
ptr_glDeleteTextures :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTextures = unsafePerformIO $ getCommand "glDeleteTextures"

-- glDeleteTexturesEXT ---------------------------------------------------------

glDeleteTexturesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glDeleteTexturesEXT v1 v2 = liftIO $ dyn200 ptr_glDeleteTexturesEXT v1 v2

{-# NOINLINE ptr_glDeleteTexturesEXT #-}
ptr_glDeleteTexturesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTexturesEXT = unsafePerformIO $ getCommand "glDeleteTexturesEXT"

-- glDeleteTransformFeedbacks --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDeleteTransformFeedbacks.xhtml OpenGL 4.x>.
glDeleteTransformFeedbacks
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteTransformFeedbacks v1 v2 = liftIO $ dyn200 ptr_glDeleteTransformFeedbacks v1 v2

{-# NOINLINE ptr_glDeleteTransformFeedbacks #-}
ptr_glDeleteTransformFeedbacks :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTransformFeedbacks = unsafePerformIO $ getCommand "glDeleteTransformFeedbacks"

-- glDeleteTransformFeedbacksNV ------------------------------------------------

-- | This command is an alias for 'glDeleteTransformFeedbacks'.
glDeleteTransformFeedbacksNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteTransformFeedbacksNV v1 v2 = liftIO $ dyn200 ptr_glDeleteTransformFeedbacksNV v1 v2

{-# NOINLINE ptr_glDeleteTransformFeedbacksNV #-}
ptr_glDeleteTransformFeedbacksNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTransformFeedbacksNV = unsafePerformIO $ getCommand "glDeleteTransformFeedbacksNV"

-- glDeleteVertexArrays --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteVertexArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteVertexArrays.xhtml OpenGL 4.x>.
glDeleteVertexArrays
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteVertexArrays v1 v2 = liftIO $ dyn200 ptr_glDeleteVertexArrays v1 v2

{-# NOINLINE ptr_glDeleteVertexArrays #-}
ptr_glDeleteVertexArrays :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteVertexArrays = unsafePerformIO $ getCommand "glDeleteVertexArrays"

-- glDeleteVertexArraysAPPLE ---------------------------------------------------

-- | This command is an alias for 'glDeleteVertexArrays'.
glDeleteVertexArraysAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteVertexArraysAPPLE v1 v2 = liftIO $ dyn200 ptr_glDeleteVertexArraysAPPLE v1 v2

{-# NOINLINE ptr_glDeleteVertexArraysAPPLE #-}
ptr_glDeleteVertexArraysAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteVertexArraysAPPLE = unsafePerformIO $ getCommand "glDeleteVertexArraysAPPLE"

