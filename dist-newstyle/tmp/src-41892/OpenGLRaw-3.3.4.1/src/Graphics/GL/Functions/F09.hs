{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F09
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

module Graphics.GL.Functions.F09 (
  glGenFencesAPPLE,
  glGenFencesNV,
  glGenFragmentShadersATI,
  glGenFramebuffers,
  glGenFramebuffersEXT,
  glGenFramebuffersOES,
  glGenLists,
  glGenNamesAMD,
  glGenOcclusionQueriesNV,
  glGenPathsNV,
  glGenPerfMonitorsAMD,
  glGenProgramPipelines,
  glGenProgramPipelinesEXT,
  glGenProgramsARB,
  glGenProgramsNV,
  glGenQueries,
  glGenQueriesARB,
  glGenQueriesEXT,
  glGenQueryResourceTagNV,
  glGenRenderbuffers,
  glGenRenderbuffersEXT,
  glGenRenderbuffersOES,
  glGenSamplers,
  glGenSemaphoresEXT,
  glGenSymbolsEXT,
  glGenTextures,
  glGenTexturesEXT,
  glGenTransformFeedbacks,
  glGenTransformFeedbacksNV,
  glGenVertexArrays,
  glGenVertexArraysAPPLE,
  glGenVertexArraysOES,
  glGenVertexShadersEXT,
  glGenerateMipmap,
  glGenerateMipmapEXT,
  glGenerateMipmapOES,
  glGenerateMultiTexMipmapEXT,
  glGenerateTextureMipmap,
  glGenerateTextureMipmapEXT,
  glGetActiveAtomicCounterBufferiv,
  glGetActiveAttrib,
  glGetActiveAttribARB,
  glGetActiveSubroutineName,
  glGetActiveSubroutineUniformName,
  glGetActiveSubroutineUniformiv,
  glGetActiveUniform,
  glGetActiveUniformARB,
  glGetActiveUniformBlockName,
  glGetActiveUniformBlockiv,
  glGetActiveUniformName,
  glGetActiveUniformsiv,
  glGetActiveVaryingNV,
  glGetArrayObjectfvATI,
  glGetArrayObjectivATI,
  glGetAttachedObjectsARB,
  glGetAttachedShaders,
  glGetAttribLocation,
  glGetAttribLocationARB,
  glGetBooleanIndexedvEXT,
  glGetBooleani_v,
  glGetBooleanv,
  glGetBufferParameteri64v,
  glGetBufferParameteriv,
  glGetBufferParameterivARB,
  glGetBufferParameterui64vNV,
  glGetBufferPointerv,
  glGetBufferPointervARB,
  glGetBufferPointervOES,
  glGetBufferSubData,
  glGetBufferSubDataARB,
  glGetClipPlane,
  glGetClipPlanef,
  glGetClipPlanefOES,
  glGetClipPlanex,
  glGetClipPlanexOES,
  glGetColorTable,
  glGetColorTableEXT,
  glGetColorTableParameterfv,
  glGetColorTableParameterfvEXT,
  glGetColorTableParameterfvSGI,
  glGetColorTableParameteriv,
  glGetColorTableParameterivEXT,
  glGetColorTableParameterivSGI,
  glGetColorTableSGI,
  glGetCombinerInputParameterfvNV,
  glGetCombinerInputParameterivNV,
  glGetCombinerOutputParameterfvNV,
  glGetCombinerOutputParameterivNV,
  glGetCombinerStageParameterfvNV,
  glGetCommandHeaderNV,
  glGetCompressedMultiTexImageEXT,
  glGetCompressedTexImage,
  glGetCompressedTexImageARB,
  glGetCompressedTextureImage,
  glGetCompressedTextureImageEXT,
  glGetCompressedTextureSubImage,
  glGetConvolutionFilter,
  glGetConvolutionFilterEXT,
  glGetConvolutionParameterfv,
  glGetConvolutionParameterfvEXT
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glGenFencesAPPLE ------------------------------------------------------------

glGenFencesAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glGenFencesAPPLE v1 v2 = liftIO $ dyn200 ptr_glGenFencesAPPLE v1 v2

{-# NOINLINE ptr_glGenFencesAPPLE #-}
ptr_glGenFencesAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFencesAPPLE = unsafePerformIO $ getCommand "glGenFencesAPPLE"

-- glGenFencesNV ---------------------------------------------------------------

glGenFencesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glGenFencesNV v1 v2 = liftIO $ dyn200 ptr_glGenFencesNV v1 v2

{-# NOINLINE ptr_glGenFencesNV #-}
ptr_glGenFencesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFencesNV = unsafePerformIO $ getCommand "glGenFencesNV"

-- glGenFragmentShadersATI -----------------------------------------------------

glGenFragmentShadersATI
  :: MonadIO m
  => GLuint -- ^ @range@.
  -> m GLuint
glGenFragmentShadersATI v1 = liftIO $ dyn312 ptr_glGenFragmentShadersATI v1

{-# NOINLINE ptr_glGenFragmentShadersATI #-}
ptr_glGenFragmentShadersATI :: FunPtr (GLuint -> IO GLuint)
ptr_glGenFragmentShadersATI = unsafePerformIO $ getCommand "glGenFragmentShadersATI"

-- glGenFramebuffers -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenFramebuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenFramebuffers.xhtml OpenGL 4.x>.
glGenFramebuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenFramebuffers v1 v2 = liftIO $ dyn200 ptr_glGenFramebuffers v1 v2

{-# NOINLINE ptr_glGenFramebuffers #-}
ptr_glGenFramebuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFramebuffers = unsafePerformIO $ getCommand "glGenFramebuffers"

-- glGenFramebuffersEXT --------------------------------------------------------

-- | This command is an alias for 'glGenFramebuffers'.
glGenFramebuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenFramebuffersEXT v1 v2 = liftIO $ dyn200 ptr_glGenFramebuffersEXT v1 v2

{-# NOINLINE ptr_glGenFramebuffersEXT #-}
ptr_glGenFramebuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFramebuffersEXT = unsafePerformIO $ getCommand "glGenFramebuffersEXT"

-- glGenFramebuffersOES --------------------------------------------------------

glGenFramebuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenFramebuffersOES v1 v2 = liftIO $ dyn200 ptr_glGenFramebuffersOES v1 v2

{-# NOINLINE ptr_glGenFramebuffersOES #-}
ptr_glGenFramebuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFramebuffersOES = unsafePerformIO $ getCommand "glGenFramebuffersOES"

-- glGenLists ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenLists.xml OpenGL 2.x>.
glGenLists
  :: MonadIO m
  => GLsizei -- ^ @range@.
  -> m GLuint -- ^ of type @List@.
glGenLists v1 = liftIO $ dyn311 ptr_glGenLists v1

{-# NOINLINE ptr_glGenLists #-}
ptr_glGenLists :: FunPtr (GLsizei -> IO GLuint)
ptr_glGenLists = unsafePerformIO $ getCommand "glGenLists"

-- glGenNamesAMD ---------------------------------------------------------------

glGenNamesAMD
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @num@.
  -> Ptr GLuint -- ^ @names@ pointing to @num@ elements of type @GLuint@.
  -> m ()
glGenNamesAMD v1 v2 v3 = liftIO $ dyn221 ptr_glGenNamesAMD v1 v2 v3

{-# NOINLINE ptr_glGenNamesAMD #-}
ptr_glGenNamesAMD :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGenNamesAMD = unsafePerformIO $ getCommand "glGenNamesAMD"

-- glGenOcclusionQueriesNV -----------------------------------------------------

glGenOcclusionQueriesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenOcclusionQueriesNV v1 v2 = liftIO $ dyn200 ptr_glGenOcclusionQueriesNV v1 v2

{-# NOINLINE ptr_glGenOcclusionQueriesNV #-}
ptr_glGenOcclusionQueriesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenOcclusionQueriesNV = unsafePerformIO $ getCommand "glGenOcclusionQueriesNV"

-- glGenPathsNV ----------------------------------------------------------------

glGenPathsNV
  :: MonadIO m
  => GLsizei -- ^ @range@.
  -> m GLuint -- ^ of type @Path@.
glGenPathsNV v1 = liftIO $ dyn311 ptr_glGenPathsNV v1

{-# NOINLINE ptr_glGenPathsNV #-}
ptr_glGenPathsNV :: FunPtr (GLsizei -> IO GLuint)
ptr_glGenPathsNV = unsafePerformIO $ getCommand "glGenPathsNV"

-- glGenPerfMonitorsAMD --------------------------------------------------------

glGenPerfMonitorsAMD
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @monitors@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenPerfMonitorsAMD v1 v2 = liftIO $ dyn200 ptr_glGenPerfMonitorsAMD v1 v2

{-# NOINLINE ptr_glGenPerfMonitorsAMD #-}
ptr_glGenPerfMonitorsAMD :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenPerfMonitorsAMD = unsafePerformIO $ getCommand "glGenPerfMonitorsAMD"

-- glGenProgramPipelines -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGenProgramPipelines.xhtml OpenGL 4.x>.
glGenProgramPipelines
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramPipelines v1 v2 = liftIO $ dyn200 ptr_glGenProgramPipelines v1 v2

{-# NOINLINE ptr_glGenProgramPipelines #-}
ptr_glGenProgramPipelines :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramPipelines = unsafePerformIO $ getCommand "glGenProgramPipelines"

-- glGenProgramPipelinesEXT ----------------------------------------------------

glGenProgramPipelinesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramPipelinesEXT v1 v2 = liftIO $ dyn200 ptr_glGenProgramPipelinesEXT v1 v2

{-# NOINLINE ptr_glGenProgramPipelinesEXT #-}
ptr_glGenProgramPipelinesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramPipelinesEXT = unsafePerformIO $ getCommand "glGenProgramPipelinesEXT"

-- glGenProgramsARB ------------------------------------------------------------

glGenProgramsARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramsARB v1 v2 = liftIO $ dyn200 ptr_glGenProgramsARB v1 v2

{-# NOINLINE ptr_glGenProgramsARB #-}
ptr_glGenProgramsARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramsARB = unsafePerformIO $ getCommand "glGenProgramsARB"

-- glGenProgramsNV -------------------------------------------------------------

-- | This command is an alias for 'glGenProgramsARB'.
glGenProgramsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramsNV v1 v2 = liftIO $ dyn200 ptr_glGenProgramsNV v1 v2

{-# NOINLINE ptr_glGenProgramsNV #-}
ptr_glGenProgramsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramsNV = unsafePerformIO $ getCommand "glGenProgramsNV"

-- glGenQueries ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenQueries.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGenQueries.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenQueries.xhtml OpenGL 4.x>.
glGenQueries
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenQueries v1 v2 = liftIO $ dyn200 ptr_glGenQueries v1 v2

{-# NOINLINE ptr_glGenQueries #-}
ptr_glGenQueries :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenQueries = unsafePerformIO $ getCommand "glGenQueries"

-- glGenQueriesARB -------------------------------------------------------------

-- | This command is an alias for 'glGenQueries'.
glGenQueriesARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenQueriesARB v1 v2 = liftIO $ dyn200 ptr_glGenQueriesARB v1 v2

{-# NOINLINE ptr_glGenQueriesARB #-}
ptr_glGenQueriesARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenQueriesARB = unsafePerformIO $ getCommand "glGenQueriesARB"

-- glGenQueriesEXT -------------------------------------------------------------

glGenQueriesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenQueriesEXT v1 v2 = liftIO $ dyn200 ptr_glGenQueriesEXT v1 v2

{-# NOINLINE ptr_glGenQueriesEXT #-}
ptr_glGenQueriesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenQueriesEXT = unsafePerformIO $ getCommand "glGenQueriesEXT"

-- glGenQueryResourceTagNV -----------------------------------------------------

glGenQueryResourceTagNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLint -- ^ @tagIds@ pointing to @n@ elements of type @GLint@.
  -> m ()
glGenQueryResourceTagNV v1 v2 = liftIO $ dyn222 ptr_glGenQueryResourceTagNV v1 v2

{-# NOINLINE ptr_glGenQueryResourceTagNV #-}
ptr_glGenQueryResourceTagNV :: FunPtr (GLsizei -> Ptr GLint -> IO ())
ptr_glGenQueryResourceTagNV = unsafePerformIO $ getCommand "glGenQueryResourceTagNV"

-- glGenRenderbuffers ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenRenderbuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenRenderbuffers.xhtml OpenGL 4.x>.
glGenRenderbuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenRenderbuffers v1 v2 = liftIO $ dyn200 ptr_glGenRenderbuffers v1 v2

{-# NOINLINE ptr_glGenRenderbuffers #-}
ptr_glGenRenderbuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenRenderbuffers = unsafePerformIO $ getCommand "glGenRenderbuffers"

-- glGenRenderbuffersEXT -------------------------------------------------------

-- | This command is an alias for 'glGenRenderbuffers'.
glGenRenderbuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenRenderbuffersEXT v1 v2 = liftIO $ dyn200 ptr_glGenRenderbuffersEXT v1 v2

{-# NOINLINE ptr_glGenRenderbuffersEXT #-}
ptr_glGenRenderbuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenRenderbuffersEXT = unsafePerformIO $ getCommand "glGenRenderbuffersEXT"

-- glGenRenderbuffersOES -------------------------------------------------------

glGenRenderbuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenRenderbuffersOES v1 v2 = liftIO $ dyn200 ptr_glGenRenderbuffersOES v1 v2

{-# NOINLINE ptr_glGenRenderbuffersOES #-}
ptr_glGenRenderbuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenRenderbuffersOES = unsafePerformIO $ getCommand "glGenRenderbuffersOES"

-- glGenSamplers ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenSamplers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenSamplers.xhtml OpenGL 4.x>.
glGenSamplers
  :: MonadIO m
  => GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @samplers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glGenSamplers v1 v2 = liftIO $ dyn200 ptr_glGenSamplers v1 v2

{-# NOINLINE ptr_glGenSamplers #-}
ptr_glGenSamplers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenSamplers = unsafePerformIO $ getCommand "glGenSamplers"

-- glGenSemaphoresEXT ----------------------------------------------------------

glGenSemaphoresEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @semaphores@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenSemaphoresEXT v1 v2 = liftIO $ dyn200 ptr_glGenSemaphoresEXT v1 v2

{-# NOINLINE ptr_glGenSemaphoresEXT #-}
ptr_glGenSemaphoresEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenSemaphoresEXT = unsafePerformIO $ getCommand "glGenSemaphoresEXT"

-- glGenSymbolsEXT -------------------------------------------------------------

glGenSymbolsEXT
  :: MonadIO m
  => GLenum -- ^ @datatype@ of type [DataTypeEXT](Graphics-GL-Groups.html#DataTypeEXT).
  -> GLenum -- ^ @storagetype@ of type [VertexShaderStorageTypeEXT](Graphics-GL-Groups.html#VertexShaderStorageTypeEXT).
  -> GLenum -- ^ @range@ of type [ParameterRangeEXT](Graphics-GL-Groups.html#ParameterRangeEXT).
  -> GLuint -- ^ @components@.
  -> m GLuint
glGenSymbolsEXT v1 v2 v3 v4 = liftIO $ dyn313 ptr_glGenSymbolsEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGenSymbolsEXT #-}
ptr_glGenSymbolsEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO GLuint)
ptr_glGenSymbolsEXT = unsafePerformIO $ getCommand "glGenSymbolsEXT"

-- glGenTextures ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenTextures.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGenTextures.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenTextures.xhtml OpenGL 4.x>.
glGenTextures
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glGenTextures v1 v2 = liftIO $ dyn200 ptr_glGenTextures v1 v2

{-# NOINLINE ptr_glGenTextures #-}
ptr_glGenTextures :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTextures = unsafePerformIO $ getCommand "glGenTextures"

-- glGenTexturesEXT ------------------------------------------------------------

glGenTexturesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glGenTexturesEXT v1 v2 = liftIO $ dyn200 ptr_glGenTexturesEXT v1 v2

{-# NOINLINE ptr_glGenTexturesEXT #-}
ptr_glGenTexturesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTexturesEXT = unsafePerformIO $ getCommand "glGenTexturesEXT"

-- glGenTransformFeedbacks -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGenTransformFeedbacks.xhtml OpenGL 4.x>.
glGenTransformFeedbacks
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenTransformFeedbacks v1 v2 = liftIO $ dyn200 ptr_glGenTransformFeedbacks v1 v2

{-# NOINLINE ptr_glGenTransformFeedbacks #-}
ptr_glGenTransformFeedbacks :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTransformFeedbacks = unsafePerformIO $ getCommand "glGenTransformFeedbacks"

-- glGenTransformFeedbacksNV ---------------------------------------------------

-- | This command is an alias for 'glGenTransformFeedbacks'.
glGenTransformFeedbacksNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenTransformFeedbacksNV v1 v2 = liftIO $ dyn200 ptr_glGenTransformFeedbacksNV v1 v2

{-# NOINLINE ptr_glGenTransformFeedbacksNV #-}
ptr_glGenTransformFeedbacksNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTransformFeedbacksNV = unsafePerformIO $ getCommand "glGenTransformFeedbacksNV"

-- glGenVertexArrays -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenVertexArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenVertexArrays.xhtml OpenGL 4.x>.
glGenVertexArrays
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenVertexArrays v1 v2 = liftIO $ dyn200 ptr_glGenVertexArrays v1 v2

{-# NOINLINE ptr_glGenVertexArrays #-}
ptr_glGenVertexArrays :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenVertexArrays = unsafePerformIO $ getCommand "glGenVertexArrays"

-- glGenVertexArraysAPPLE ------------------------------------------------------

-- | This command is an alias for 'glGenVertexArrays'.
glGenVertexArraysAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenVertexArraysAPPLE v1 v2 = liftIO $ dyn200 ptr_glGenVertexArraysAPPLE v1 v2

{-# NOINLINE ptr_glGenVertexArraysAPPLE #-}
ptr_glGenVertexArraysAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenVertexArraysAPPLE = unsafePerformIO $ getCommand "glGenVertexArraysAPPLE"

-- glGenVertexArraysOES --------------------------------------------------------

-- | This command is an alias for 'glGenVertexArrays'.
glGenVertexArraysOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenVertexArraysOES v1 v2 = liftIO $ dyn200 ptr_glGenVertexArraysOES v1 v2

{-# NOINLINE ptr_glGenVertexArraysOES #-}
ptr_glGenVertexArraysOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenVertexArraysOES = unsafePerformIO $ getCommand "glGenVertexArraysOES"

-- glGenVertexShadersEXT -------------------------------------------------------

glGenVertexShadersEXT
  :: MonadIO m
  => GLuint -- ^ @range@.
  -> m GLuint
glGenVertexShadersEXT v1 = liftIO $ dyn312 ptr_glGenVertexShadersEXT v1

{-# NOINLINE ptr_glGenVertexShadersEXT #-}
ptr_glGenVertexShadersEXT :: FunPtr (GLuint -> IO GLuint)
ptr_glGenVertexShadersEXT = unsafePerformIO $ getCommand "glGenVertexShadersEXT"

-- glGenerateMipmap ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenerateMipmap.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenerateMipmap.xhtml OpenGL 4.x>.
glGenerateMipmap
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glGenerateMipmap v1 = liftIO $ dyn5 ptr_glGenerateMipmap v1

{-# NOINLINE ptr_glGenerateMipmap #-}
ptr_glGenerateMipmap :: FunPtr (GLenum -> IO ())
ptr_glGenerateMipmap = unsafePerformIO $ getCommand "glGenerateMipmap"

-- glGenerateMipmapEXT ---------------------------------------------------------

-- | This command is an alias for 'glGenerateMipmap'.
glGenerateMipmapEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glGenerateMipmapEXT v1 = liftIO $ dyn5 ptr_glGenerateMipmapEXT v1

{-# NOINLINE ptr_glGenerateMipmapEXT #-}
ptr_glGenerateMipmapEXT :: FunPtr (GLenum -> IO ())
ptr_glGenerateMipmapEXT = unsafePerformIO $ getCommand "glGenerateMipmapEXT"

-- glGenerateMipmapOES ---------------------------------------------------------

glGenerateMipmapOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glGenerateMipmapOES v1 = liftIO $ dyn5 ptr_glGenerateMipmapOES v1

{-# NOINLINE ptr_glGenerateMipmapOES #-}
ptr_glGenerateMipmapOES :: FunPtr (GLenum -> IO ())
ptr_glGenerateMipmapOES = unsafePerformIO $ getCommand "glGenerateMipmapOES"

-- glGenerateMultiTexMipmapEXT -------------------------------------------------

glGenerateMultiTexMipmapEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glGenerateMultiTexMipmapEXT v1 v2 = liftIO $ dyn54 ptr_glGenerateMultiTexMipmapEXT v1 v2

{-# NOINLINE ptr_glGenerateMultiTexMipmapEXT #-}
ptr_glGenerateMultiTexMipmapEXT :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glGenerateMultiTexMipmapEXT = unsafePerformIO $ getCommand "glGenerateMultiTexMipmapEXT"

-- glGenerateTextureMipmap -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGenerateMipmap.xhtml OpenGL 4.x>.
glGenerateTextureMipmap
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m ()
glGenerateTextureMipmap v1 = liftIO $ dyn3 ptr_glGenerateTextureMipmap v1

{-# NOINLINE ptr_glGenerateTextureMipmap #-}
ptr_glGenerateTextureMipmap :: FunPtr (GLuint -> IO ())
ptr_glGenerateTextureMipmap = unsafePerformIO $ getCommand "glGenerateTextureMipmap"

-- glGenerateTextureMipmapEXT --------------------------------------------------

glGenerateTextureMipmapEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glGenerateTextureMipmapEXT v1 v2 = liftIO $ dyn18 ptr_glGenerateTextureMipmapEXT v1 v2

{-# NOINLINE ptr_glGenerateTextureMipmapEXT #-}
ptr_glGenerateTextureMipmapEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glGenerateTextureMipmapEXT = unsafePerformIO $ getCommand "glGenerateTextureMipmapEXT"

-- glGetActiveAtomicCounterBufferiv --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveAtomicCounterBufferiv.xhtml OpenGL 4.x>.
glGetActiveAtomicCounterBufferiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @bufferIndex@.
  -> GLenum -- ^ @pname@ of type [AtomicCounterBufferPName](Graphics-GL-Groups.html#AtomicCounterBufferPName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetActiveAtomicCounterBufferiv v1 v2 v3 v4 = liftIO $ dyn314 ptr_glGetActiveAtomicCounterBufferiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetActiveAtomicCounterBufferiv #-}
ptr_glGetActiveAtomicCounterBufferiv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveAtomicCounterBufferiv = unsafePerformIO $ getCommand "glGetActiveAtomicCounterBufferiv"

-- glGetActiveAttrib -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetActiveAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveAttrib.xhtml OpenGL 4.x>.
glGetActiveAttrib
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type [AttributeType](Graphics-GL-Groups.html#AttributeType).
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveAttrib v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn315 ptr_glGetActiveAttrib v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveAttrib #-}
ptr_glGetActiveAttrib :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetActiveAttrib = unsafePerformIO $ getCommand "glGetActiveAttrib"

-- glGetActiveAttribARB --------------------------------------------------------

-- | This command is an alias for 'glGetActiveAttrib'.
glGetActiveAttribARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @maxLength@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type [AttributeType](Graphics-GL-Groups.html#AttributeType).
  -> Ptr GLcharARB -- ^ @name@ pointing to @maxLength@ elements of type @GLcharARB@.
  -> m ()
glGetActiveAttribARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn316 ptr_glGetActiveAttribARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveAttribARB #-}
ptr_glGetActiveAttribARB :: FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ())
ptr_glGetActiveAttribARB = unsafePerformIO $ getCommand "glGetActiveAttribARB"

-- glGetActiveSubroutineName ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineName.xhtml OpenGL 4.x>.
glGetActiveSubroutineName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveSubroutineName v1 v2 v3 v4 v5 v6 = liftIO $ dyn317 ptr_glGetActiveSubroutineName v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetActiveSubroutineName #-}
ptr_glGetActiveSubroutineName :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveSubroutineName = unsafePerformIO $ getCommand "glGetActiveSubroutineName"

-- glGetActiveSubroutineUniformName --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineUniformName.xhtml OpenGL 4.x>.
glGetActiveSubroutineUniformName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveSubroutineUniformName v1 v2 v3 v4 v5 v6 = liftIO $ dyn317 ptr_glGetActiveSubroutineUniformName v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetActiveSubroutineUniformName #-}
ptr_glGetActiveSubroutineUniformName :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveSubroutineUniformName = unsafePerformIO $ getCommand "glGetActiveSubroutineUniformName"

-- glGetActiveSubroutineUniformiv ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineUniform.xhtml OpenGL 4.x>.
glGetActiveSubroutineUniformiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [SubroutineParameterName](Graphics-GL-Groups.html#SubroutineParameterName).
  -> Ptr GLint -- ^ @values@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetActiveSubroutineUniformiv v1 v2 v3 v4 v5 = liftIO $ dyn318 ptr_glGetActiveSubroutineUniformiv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveSubroutineUniformiv #-}
ptr_glGetActiveSubroutineUniformiv :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveSubroutineUniformiv = unsafePerformIO $ getCommand "glGetActiveSubroutineUniformiv"

-- glGetActiveUniform ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetActiveUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniform.xhtml OpenGL 4.x>.
glGetActiveUniform
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type [UniformType](Graphics-GL-Groups.html#UniformType).
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveUniform v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn315 ptr_glGetActiveUniform v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveUniform #-}
ptr_glGetActiveUniform :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetActiveUniform = unsafePerformIO $ getCommand "glGetActiveUniform"

-- glGetActiveUniformARB -------------------------------------------------------

-- | This command is an alias for 'glGetActiveUniform'.
glGetActiveUniformARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @maxLength@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type [UniformType](Graphics-GL-Groups.html#UniformType).
  -> Ptr GLcharARB -- ^ @name@ pointing to @maxLength@ elements of type @GLcharARB@.
  -> m ()
glGetActiveUniformARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn316 ptr_glGetActiveUniformARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveUniformARB #-}
ptr_glGetActiveUniformARB :: FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ())
ptr_glGetActiveUniformARB = unsafePerformIO $ getCommand "glGetActiveUniformARB"

-- glGetActiveUniformBlockName -------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformBlockName.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformBlockName.xhtml OpenGL 4.x>.
glGetActiveUniformBlockName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformBlockIndex@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @uniformBlockName@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveUniformBlockName v1 v2 v3 v4 v5 = liftIO $ dyn319 ptr_glGetActiveUniformBlockName v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveUniformBlockName #-}
ptr_glGetActiveUniformBlockName :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveUniformBlockName = unsafePerformIO $ getCommand "glGetActiveUniformBlockName"

-- glGetActiveUniformBlockiv ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformBlock.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformBlock.xhtml OpenGL 4.x>.
glGetActiveUniformBlockiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformBlockIndex@.
  -> GLenum -- ^ @pname@ of type [UniformBlockPName](Graphics-GL-Groups.html#UniformBlockPName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(program,uniformBlockIndex,pname)@ elements of type @GLint@.
  -> m ()
glGetActiveUniformBlockiv v1 v2 v3 v4 = liftIO $ dyn314 ptr_glGetActiveUniformBlockiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetActiveUniformBlockiv #-}
ptr_glGetActiveUniformBlockiv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveUniformBlockiv = unsafePerformIO $ getCommand "glGetActiveUniformBlockiv"

-- glGetActiveUniformName ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformName.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformName.xhtml OpenGL 4.x>.
glGetActiveUniformName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformIndex@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @uniformName@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveUniformName v1 v2 v3 v4 v5 = liftIO $ dyn319 ptr_glGetActiveUniformName v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveUniformName #-}
ptr_glGetActiveUniformName :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveUniformName = unsafePerformIO $ getCommand "glGetActiveUniformName"

-- glGetActiveUniformsiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformsiv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformsiv.xhtml OpenGL 4.x>.
glGetActiveUniformsiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @uniformCount@.
  -> Ptr GLuint -- ^ @uniformIndices@ pointing to @uniformCount@ elements of type @GLuint@.
  -> GLenum -- ^ @pname@ of type [UniformPName](Graphics-GL-Groups.html#UniformPName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(uniformCount,pname)@ elements of type @GLint@.
  -> m ()
glGetActiveUniformsiv v1 v2 v3 v4 v5 = liftIO $ dyn320 ptr_glGetActiveUniformsiv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveUniformsiv #-}
ptr_glGetActiveUniformsiv :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveUniformsiv = unsafePerformIO $ getCommand "glGetActiveUniformsiv"

-- glGetActiveVaryingNV --------------------------------------------------------

glGetActiveVaryingNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLsizei -- ^ @size@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(program,index,bufSize)@ elements of type @GLchar@.
  -> m ()
glGetActiveVaryingNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn321 ptr_glGetActiveVaryingNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveVaryingNV #-}
ptr_glGetActiveVaryingNV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetActiveVaryingNV = unsafePerformIO $ getCommand "glGetActiveVaryingNV"

-- glGetArrayObjectfvATI -------------------------------------------------------

glGetArrayObjectfvATI
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetArrayObjectfvATI v1 v2 v3 = liftIO $ dyn139 ptr_glGetArrayObjectfvATI v1 v2 v3

{-# NOINLINE ptr_glGetArrayObjectfvATI #-}
ptr_glGetArrayObjectfvATI :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetArrayObjectfvATI = unsafePerformIO $ getCommand "glGetArrayObjectfvATI"

-- glGetArrayObjectivATI -------------------------------------------------------

glGetArrayObjectivATI
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetArrayObjectivATI v1 v2 v3 = liftIO $ dyn140 ptr_glGetArrayObjectivATI v1 v2 v3

{-# NOINLINE ptr_glGetArrayObjectivATI #-}
ptr_glGetArrayObjectivATI :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetArrayObjectivATI = unsafePerformIO $ getCommand "glGetArrayObjectivATI"

-- glGetAttachedObjectsARB -----------------------------------------------------

glGetAttachedObjectsARB
  :: MonadIO m
  => GLhandleARB -- ^ @containerObj@ of type @handleARB@.
  -> GLsizei -- ^ @maxCount@.
  -> Ptr GLsizei -- ^ @count@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLhandleARB -- ^ @obj@ pointing to @maxCount@ elements of type @handleARB@.
  -> m ()
glGetAttachedObjectsARB v1 v2 v3 v4 = liftIO $ dyn322 ptr_glGetAttachedObjectsARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetAttachedObjectsARB #-}
ptr_glGetAttachedObjectsARB :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> IO ())
ptr_glGetAttachedObjectsARB = unsafePerformIO $ getCommand "glGetAttachedObjectsARB"

-- glGetAttachedShaders --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetAttachedShaders.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetAttachedShaders.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetAttachedShaders.xhtml OpenGL 4.x>.
glGetAttachedShaders
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @maxCount@.
  -> Ptr GLsizei -- ^ @count@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLuint -- ^ @shaders@ pointing to @maxCount@ elements of type @GLuint@.
  -> m ()
glGetAttachedShaders v1 v2 v3 v4 = liftIO $ dyn323 ptr_glGetAttachedShaders v1 v2 v3 v4

{-# NOINLINE ptr_glGetAttachedShaders #-}
ptr_glGetAttachedShaders :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())
ptr_glGetAttachedShaders = unsafePerformIO $ getCommand "glGetAttachedShaders"

-- glGetAttribLocation ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetAttribLocation.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetAttribLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetAttribLocation.xhtml OpenGL 4.x>.
glGetAttribLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetAttribLocation v1 v2 = liftIO $ dyn324 ptr_glGetAttribLocation v1 v2

{-# NOINLINE ptr_glGetAttribLocation #-}
ptr_glGetAttribLocation :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetAttribLocation = unsafePerformIO $ getCommand "glGetAttribLocation"

-- glGetAttribLocationARB ------------------------------------------------------

-- | This command is an alias for 'glGetAttribLocation'.
glGetAttribLocationARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> Ptr GLcharARB -- ^ @name@.
  -> m GLint
glGetAttribLocationARB v1 v2 = liftIO $ dyn325 ptr_glGetAttribLocationARB v1 v2

{-# NOINLINE ptr_glGetAttribLocationARB #-}
ptr_glGetAttribLocationARB :: FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint)
ptr_glGetAttribLocationARB = unsafePerformIO $ getCommand "glGetAttribLocationARB"

-- glGetBooleanIndexedvEXT -----------------------------------------------------

-- | This command is an alias for 'glGetBooleani_v'.
glGetBooleanIndexedvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetBooleanIndexedvEXT v1 v2 v3 = liftIO $ dyn326 ptr_glGetBooleanIndexedvEXT v1 v2 v3

{-# NOINLINE ptr_glGetBooleanIndexedvEXT #-}
ptr_glGetBooleanIndexedvEXT :: FunPtr (GLenum -> GLuint -> Ptr GLboolean -> IO ())
ptr_glGetBooleanIndexedvEXT = unsafePerformIO $ getCommand "glGetBooleanIndexedvEXT"

-- glGetBooleani_v -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetBooleani_v
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetBooleani_v v1 v2 v3 = liftIO $ dyn326 ptr_glGetBooleani_v v1 v2 v3

{-# NOINLINE ptr_glGetBooleani_v #-}
ptr_glGetBooleani_v :: FunPtr (GLenum -> GLuint -> Ptr GLboolean -> IO ())
ptr_glGetBooleani_v = unsafePerformIO $ getCommand "glGetBooleani_v"

-- glGetBooleanv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGet.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetBooleanv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPName](Graphics-GL-Groups.html#GetPName).
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetBooleanv v1 v2 = liftIO $ dyn327 ptr_glGetBooleanv v1 v2

{-# NOINLINE ptr_glGetBooleanv #-}
ptr_glGetBooleanv :: FunPtr (GLenum -> Ptr GLboolean -> IO ())
ptr_glGetBooleanv = unsafePerformIO $ getCommand "glGetBooleanv"

-- glGetBufferParameteri64v ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetBufferParameteri64v
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @pname@ of type [BufferPNameARB](Graphics-GL-Groups.html#BufferPNameARB).
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64@.
  -> m ()
glGetBufferParameteri64v v1 v2 v3 = liftIO $ dyn328 ptr_glGetBufferParameteri64v v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameteri64v #-}
ptr_glGetBufferParameteri64v :: FunPtr (GLenum -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetBufferParameteri64v = unsafePerformIO $ getCommand "glGetBufferParameteri64v"

-- glGetBufferParameteriv ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferParameteriv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetBufferParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @pname@ of type [BufferPNameARB](Graphics-GL-Groups.html#BufferPNameARB).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetBufferParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glGetBufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameteriv #-}
ptr_glGetBufferParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetBufferParameteriv = unsafePerformIO $ getCommand "glGetBufferParameteriv"

-- glGetBufferParameterivARB ---------------------------------------------------

-- | This command is an alias for 'glGetBufferParameteriv'.
glGetBufferParameterivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @pname@ of type [BufferPNameARB](Graphics-GL-Groups.html#BufferPNameARB).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetBufferParameterivARB v1 v2 v3 = liftIO $ dyn140 ptr_glGetBufferParameterivARB v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameterivARB #-}
ptr_glGetBufferParameterivARB :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetBufferParameterivARB = unsafePerformIO $ getCommand "glGetBufferParameterivARB"

-- glGetBufferParameterui64vNV -------------------------------------------------

glGetBufferParameterui64vNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetBufferParameterui64vNV v1 v2 v3 = liftIO $ dyn329 ptr_glGetBufferParameterui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameterui64vNV #-}
ptr_glGetBufferParameterui64vNV :: FunPtr (GLenum -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetBufferParameterui64vNV = unsafePerformIO $ getCommand "glGetBufferParameterui64vNV"

-- glGetBufferPointerv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferPointerv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferPointerv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferPointerv.xhtml OpenGL 4.x>.
glGetBufferPointerv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @pname@ of type [BufferPointerNameARB](Graphics-GL-Groups.html#BufferPointerNameARB).
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetBufferPointerv v1 v2 v3 = liftIO $ dyn330 ptr_glGetBufferPointerv v1 v2 v3

{-# NOINLINE ptr_glGetBufferPointerv #-}
ptr_glGetBufferPointerv :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetBufferPointerv = unsafePerformIO $ getCommand "glGetBufferPointerv"

-- glGetBufferPointervARB ------------------------------------------------------

-- | This command is an alias for 'glGetBufferPointerv'.
glGetBufferPointervARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @pname@ of type [BufferPointerNameARB](Graphics-GL-Groups.html#BufferPointerNameARB).
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetBufferPointervARB v1 v2 v3 = liftIO $ dyn330 ptr_glGetBufferPointervARB v1 v2 v3

{-# NOINLINE ptr_glGetBufferPointervARB #-}
ptr_glGetBufferPointervARB :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetBufferPointervARB = unsafePerformIO $ getCommand "glGetBufferPointervARB"

-- glGetBufferPointervOES ------------------------------------------------------

-- | This command is an alias for 'glGetBufferPointerv'.
glGetBufferPointervOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @pname@ of type [BufferPointerNameARB](Graphics-GL-Groups.html#BufferPointerNameARB).
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glGetBufferPointervOES v1 v2 v3 = liftIO $ dyn330 ptr_glGetBufferPointervOES v1 v2 v3

{-# NOINLINE ptr_glGetBufferPointervOES #-}
ptr_glGetBufferPointervOES :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetBufferPointervOES = unsafePerformIO $ getCommand "glGetBufferPointervOES"

-- glGetBufferSubData ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferSubData.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferSubData.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferSubData.xhtml OpenGL 4.x>.
glGetBufferSubData
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> m ()
glGetBufferSubData v1 v2 v3 v4 = liftIO $ dyn70 ptr_glGetBufferSubData v1 v2 v3 v4

{-# NOINLINE ptr_glGetBufferSubData #-}
ptr_glGetBufferSubData :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glGetBufferSubData = unsafePerformIO $ getCommand "glGetBufferSubData"

-- glGetBufferSubDataARB -------------------------------------------------------

-- | This command is an alias for 'glGetBufferSubData'.
glGetBufferSubDataARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLintptrARB -- ^ @offset@ of type @BufferOffsetARB@.
  -> GLsizeiptrARB -- ^ @size@ of type @BufferSizeARB@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> m ()
glGetBufferSubDataARB v1 v2 v3 v4 = liftIO $ dyn71 ptr_glGetBufferSubDataARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetBufferSubDataARB #-}
ptr_glGetBufferSubDataARB :: FunPtr (GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr a -> IO ())
ptr_glGetBufferSubDataARB = unsafePerformIO $ getCommand "glGetBufferSubDataARB"

-- glGetClipPlane --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetClipPlane.xml OpenGL 2.x>.
glGetClipPlane
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLdouble -- ^ @equation@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetClipPlane v1 v2 = liftIO $ dyn100 ptr_glGetClipPlane v1 v2

{-# NOINLINE ptr_glGetClipPlane #-}
ptr_glGetClipPlane :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glGetClipPlane = unsafePerformIO $ getCommand "glGetClipPlane"

-- glGetClipPlanef -------------------------------------------------------------

glGetClipPlanef
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfloat -- ^ @equation@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetClipPlanef v1 v2 = liftIO $ dyn101 ptr_glGetClipPlanef v1 v2

{-# NOINLINE ptr_glGetClipPlanef #-}
ptr_glGetClipPlanef :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetClipPlanef = unsafePerformIO $ getCommand "glGetClipPlanef"

-- glGetClipPlanefOES ----------------------------------------------------------

glGetClipPlanefOES
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfloat -- ^ @equation@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetClipPlanefOES v1 v2 = liftIO $ dyn101 ptr_glGetClipPlanefOES v1 v2

{-# NOINLINE ptr_glGetClipPlanefOES #-}
ptr_glGetClipPlanefOES :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetClipPlanefOES = unsafePerformIO $ getCommand "glGetClipPlanefOES"

-- glGetClipPlanex -------------------------------------------------------------

glGetClipPlanex
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glGetClipPlanex v1 v2 = liftIO $ dyn102 ptr_glGetClipPlanex v1 v2

{-# NOINLINE ptr_glGetClipPlanex #-}
ptr_glGetClipPlanex :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glGetClipPlanex = unsafePerformIO $ getCommand "glGetClipPlanex"

-- glGetClipPlanexOES ----------------------------------------------------------

glGetClipPlanexOES
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glGetClipPlanexOES v1 v2 = liftIO $ dyn102 ptr_glGetClipPlanexOES v1 v2

{-# NOINLINE ptr_glGetClipPlanexOES #-}
ptr_glGetClipPlanexOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glGetClipPlanexOES = unsafePerformIO $ getCommand "glGetClipPlanexOES"

-- glGetColorTable -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTable.xml OpenGL 2.x>.
glGetColorTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetColorTable v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetColorTable v1 v2 v3 v4

{-# NOINLINE ptr_glGetColorTable #-}
ptr_glGetColorTable :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetColorTable = unsafePerformIO $ getCommand "glGetColorTable"

-- glGetColorTableEXT ----------------------------------------------------------

-- | This command is an alias for 'glGetColorTable'.
glGetColorTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetColorTableEXT v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetColorTableEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetColorTableEXT #-}
ptr_glGetColorTableEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetColorTableEXT = unsafePerformIO $ getCommand "glGetColorTableEXT"

-- glGetColorTableParameterfv --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml OpenGL 2.x>.
glGetColorTableParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetColorTableParameterfv v1 v2 v3 = liftIO $ dyn139 ptr_glGetColorTableParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterfv #-}
ptr_glGetColorTableParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetColorTableParameterfv = unsafePerformIO $ getCommand "glGetColorTableParameterfv"

-- glGetColorTableParameterfvEXT -----------------------------------------------

-- | This command is an alias for 'glGetColorTableParameterfv'.
glGetColorTableParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetColorTableParameterfvEXT v1 v2 v3 = liftIO $ dyn139 ptr_glGetColorTableParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterfvEXT #-}
ptr_glGetColorTableParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetColorTableParameterfvEXT = unsafePerformIO $ getCommand "glGetColorTableParameterfvEXT"

-- glGetColorTableParameterfvSGI -----------------------------------------------

glGetColorTableParameterfvSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetColorTableParameterfvSGI v1 v2 v3 = liftIO $ dyn139 ptr_glGetColorTableParameterfvSGI v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterfvSGI #-}
ptr_glGetColorTableParameterfvSGI :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetColorTableParameterfvSGI = unsafePerformIO $ getCommand "glGetColorTableParameterfvSGI"

-- glGetColorTableParameteriv --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml OpenGL 2.x>.
glGetColorTableParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetColorTableParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glGetColorTableParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameteriv #-}
ptr_glGetColorTableParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetColorTableParameteriv = unsafePerformIO $ getCommand "glGetColorTableParameteriv"

-- glGetColorTableParameterivEXT -----------------------------------------------

-- | This command is an alias for 'glGetColorTableParameteriv'.
glGetColorTableParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetColorTableParameterivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glGetColorTableParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterivEXT #-}
ptr_glGetColorTableParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetColorTableParameterivEXT = unsafePerformIO $ getCommand "glGetColorTableParameterivEXT"

-- glGetColorTableParameterivSGI -----------------------------------------------

glGetColorTableParameterivSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetColorTableParameterivSGI v1 v2 v3 = liftIO $ dyn140 ptr_glGetColorTableParameterivSGI v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterivSGI #-}
ptr_glGetColorTableParameterivSGI :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetColorTableParameterivSGI = unsafePerformIO $ getCommand "glGetColorTableParameterivSGI"

-- glGetColorTableSGI ----------------------------------------------------------

glGetColorTableSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetColorTableSGI v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetColorTableSGI v1 v2 v3 v4

{-# NOINLINE ptr_glGetColorTableSGI #-}
ptr_glGetColorTableSGI :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetColorTableSGI = unsafePerformIO $ getCommand "glGetColorTableSGI"

-- glGetCombinerInputParameterfvNV ---------------------------------------------

glGetCombinerInputParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @portion@ of type [CombinerPortionNV](Graphics-GL-Groups.html#CombinerPortionNV).
  -> GLenum -- ^ @variable@ of type [CombinerVariableNV](Graphics-GL-Groups.html#CombinerVariableNV).
  -> GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetCombinerInputParameterfvNV v1 v2 v3 v4 v5 = liftIO $ dyn332 ptr_glGetCombinerInputParameterfvNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetCombinerInputParameterfvNV #-}
ptr_glGetCombinerInputParameterfvNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetCombinerInputParameterfvNV = unsafePerformIO $ getCommand "glGetCombinerInputParameterfvNV"

-- glGetCombinerInputParameterivNV ---------------------------------------------

glGetCombinerInputParameterivNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @portion@ of type [CombinerPortionNV](Graphics-GL-Groups.html#CombinerPortionNV).
  -> GLenum -- ^ @variable@ of type [CombinerVariableNV](Graphics-GL-Groups.html#CombinerVariableNV).
  -> GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetCombinerInputParameterivNV v1 v2 v3 v4 v5 = liftIO $ dyn333 ptr_glGetCombinerInputParameterivNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetCombinerInputParameterivNV #-}
ptr_glGetCombinerInputParameterivNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetCombinerInputParameterivNV = unsafePerformIO $ getCommand "glGetCombinerInputParameterivNV"

-- glGetCombinerOutputParameterfvNV --------------------------------------------

glGetCombinerOutputParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @portion@ of type [CombinerPortionNV](Graphics-GL-Groups.html#CombinerPortionNV).
  -> GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetCombinerOutputParameterfvNV v1 v2 v3 v4 = liftIO $ dyn334 ptr_glGetCombinerOutputParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetCombinerOutputParameterfvNV #-}
ptr_glGetCombinerOutputParameterfvNV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetCombinerOutputParameterfvNV = unsafePerformIO $ getCommand "glGetCombinerOutputParameterfvNV"

-- glGetCombinerOutputParameterivNV --------------------------------------------

glGetCombinerOutputParameterivNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @portion@ of type [CombinerPortionNV](Graphics-GL-Groups.html#CombinerPortionNV).
  -> GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetCombinerOutputParameterivNV v1 v2 v3 v4 = liftIO $ dyn335 ptr_glGetCombinerOutputParameterivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetCombinerOutputParameterivNV #-}
ptr_glGetCombinerOutputParameterivNV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetCombinerOutputParameterivNV = unsafePerformIO $ getCommand "glGetCombinerOutputParameterivNV"

-- glGetCombinerStageParameterfvNV ---------------------------------------------

glGetCombinerStageParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type [CombinerStageNV](Graphics-GL-Groups.html#CombinerStageNV).
  -> GLenum -- ^ @pname@ of type [CombinerParameterNV](Graphics-GL-Groups.html#CombinerParameterNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetCombinerStageParameterfvNV v1 v2 v3 = liftIO $ dyn139 ptr_glGetCombinerStageParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glGetCombinerStageParameterfvNV #-}
ptr_glGetCombinerStageParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetCombinerStageParameterfvNV = unsafePerformIO $ getCommand "glGetCombinerStageParameterfvNV"

-- glGetCommandHeaderNV --------------------------------------------------------

glGetCommandHeaderNV
  :: MonadIO m
  => GLenum -- ^ @tokenID@.
  -> GLuint -- ^ @size@.
  -> m GLuint
glGetCommandHeaderNV v1 v2 = liftIO $ dyn336 ptr_glGetCommandHeaderNV v1 v2

{-# NOINLINE ptr_glGetCommandHeaderNV #-}
ptr_glGetCommandHeaderNV :: FunPtr (GLenum -> GLuint -> IO GLuint)
ptr_glGetCommandHeaderNV = unsafePerformIO $ getCommand "glGetCommandHeaderNV"

-- glGetCompressedMultiTexImageEXT ---------------------------------------------

glGetCompressedMultiTexImageEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @lod@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,lod)@ elements of type @a@.
  -> m ()
glGetCompressedMultiTexImageEXT v1 v2 v3 v4 = liftIO $ dyn337 ptr_glGetCompressedMultiTexImageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetCompressedMultiTexImageEXT #-}
ptr_glGetCompressedMultiTexImageEXT :: FunPtr (GLenum -> GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedMultiTexImageEXT = unsafePerformIO $ getCommand "glGetCompressedMultiTexImageEXT"

-- glGetCompressedTexImage -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetCompressedTexImage.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetCompressedTexImage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml OpenGL 4.x>.
glGetCompressedTexImage
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,level)@ elements of type @CompressedTextureARB@.
  -> m ()
glGetCompressedTexImage v1 v2 v3 = liftIO $ dyn338 ptr_glGetCompressedTexImage v1 v2 v3

{-# NOINLINE ptr_glGetCompressedTexImage #-}
ptr_glGetCompressedTexImage :: FunPtr (GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedTexImage = unsafePerformIO $ getCommand "glGetCompressedTexImage"

-- glGetCompressedTexImageARB --------------------------------------------------

-- | This command is an alias for 'glGetCompressedTexImage'.
glGetCompressedTexImageARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,level)@ elements of type @CompressedTextureARB@.
  -> m ()
glGetCompressedTexImageARB v1 v2 v3 = liftIO $ dyn338 ptr_glGetCompressedTexImageARB v1 v2 v3

{-# NOINLINE ptr_glGetCompressedTexImageARB #-}
ptr_glGetCompressedTexImageARB :: FunPtr (GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedTexImageARB = unsafePerformIO $ getCommand "glGetCompressedTexImageARB"

-- glGetCompressedTextureImage -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml OpenGL 4.x>.
glGetCompressedTextureImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetCompressedTextureImage v1 v2 v3 v4 = liftIO $ dyn339 ptr_glGetCompressedTextureImage v1 v2 v3 v4

{-# NOINLINE ptr_glGetCompressedTextureImage #-}
ptr_glGetCompressedTextureImage :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glGetCompressedTextureImage = unsafePerformIO $ getCommand "glGetCompressedTextureImage"

-- glGetCompressedTextureImageEXT ----------------------------------------------

glGetCompressedTextureImageEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @lod@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,lod)@ elements of type @a@.
  -> m ()
glGetCompressedTextureImageEXT v1 v2 v3 v4 = liftIO $ dyn340 ptr_glGetCompressedTextureImageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetCompressedTextureImageEXT #-}
ptr_glGetCompressedTextureImageEXT :: FunPtr (GLuint -> GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedTextureImageEXT = unsafePerformIO $ getCommand "glGetCompressedTextureImageEXT"

-- glGetCompressedTextureSubImage ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetCompressedTextureSubImage.xhtml OpenGL 4.x>.
glGetCompressedTextureSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetCompressedTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn341 ptr_glGetCompressedTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glGetCompressedTextureSubImage #-}
ptr_glGetCompressedTextureSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glGetCompressedTextureSubImage = unsafePerformIO $ getCommand "glGetCompressedTextureSubImage"

-- glGetConvolutionFilter ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionFilter.xml OpenGL 2.x>.
glGetConvolutionFilter
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetConvolutionFilter v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetConvolutionFilter v1 v2 v3 v4

{-# NOINLINE ptr_glGetConvolutionFilter #-}
ptr_glGetConvolutionFilter :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetConvolutionFilter = unsafePerformIO $ getCommand "glGetConvolutionFilter"

-- glGetConvolutionFilterEXT ---------------------------------------------------

glGetConvolutionFilterEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetConvolutionFilterEXT v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetConvolutionFilterEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetConvolutionFilterEXT #-}
ptr_glGetConvolutionFilterEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetConvolutionFilterEXT = unsafePerformIO $ getCommand "glGetConvolutionFilterEXT"

-- glGetConvolutionParameterfv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionParameter.xml OpenGL 2.x>.
glGetConvolutionParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTarget](Graphics-GL-Groups.html#ConvolutionTarget).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetConvolutionParameterfv v1 v2 v3 = liftIO $ dyn139 ptr_glGetConvolutionParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetConvolutionParameterfv #-}
ptr_glGetConvolutionParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetConvolutionParameterfv = unsafePerformIO $ getCommand "glGetConvolutionParameterfv"

-- glGetConvolutionParameterfvEXT ----------------------------------------------

glGetConvolutionParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetConvolutionParameterfvEXT v1 v2 v3 = liftIO $ dyn139 ptr_glGetConvolutionParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetConvolutionParameterfvEXT #-}
ptr_glGetConvolutionParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetConvolutionParameterfvEXT = unsafePerformIO $ getCommand "glGetConvolutionParameterfvEXT"

