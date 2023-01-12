{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F12
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

module Graphics.GL.Functions.F12 (
  glGetProgramEnvParameterIivNV,
  glGetProgramEnvParameterIuivNV,
  glGetProgramEnvParameterdvARB,
  glGetProgramEnvParameterfvARB,
  glGetProgramInfoLog,
  glGetProgramInterfaceiv,
  glGetProgramLocalParameterIivNV,
  glGetProgramLocalParameterIuivNV,
  glGetProgramLocalParameterdvARB,
  glGetProgramLocalParameterfvARB,
  glGetProgramNamedParameterdvNV,
  glGetProgramNamedParameterfvNV,
  glGetProgramParameterdvNV,
  glGetProgramParameterfvNV,
  glGetProgramPipelineInfoLog,
  glGetProgramPipelineInfoLogEXT,
  glGetProgramPipelineiv,
  glGetProgramPipelineivEXT,
  glGetProgramResourceIndex,
  glGetProgramResourceLocation,
  glGetProgramResourceLocationIndex,
  glGetProgramResourceLocationIndexEXT,
  glGetProgramResourceName,
  glGetProgramResourcefvNV,
  glGetProgramResourceiv,
  glGetProgramStageiv,
  glGetProgramStringARB,
  glGetProgramStringNV,
  glGetProgramSubroutineParameteruivNV,
  glGetProgramiv,
  glGetProgramivARB,
  glGetProgramivNV,
  glGetQueryBufferObjecti64v,
  glGetQueryBufferObjectiv,
  glGetQueryBufferObjectui64v,
  glGetQueryBufferObjectuiv,
  glGetQueryIndexediv,
  glGetQueryObjecti64v,
  glGetQueryObjecti64vEXT,
  glGetQueryObjectiv,
  glGetQueryObjectivARB,
  glGetQueryObjectivEXT,
  glGetQueryObjectui64v,
  glGetQueryObjectui64vEXT,
  glGetQueryObjectuiv,
  glGetQueryObjectuivARB,
  glGetQueryObjectuivEXT,
  glGetQueryiv,
  glGetQueryivARB,
  glGetQueryivEXT,
  glGetRenderbufferParameteriv,
  glGetRenderbufferParameterivEXT,
  glGetRenderbufferParameterivOES,
  glGetSamplerParameterIiv,
  glGetSamplerParameterIivEXT,
  glGetSamplerParameterIivOES,
  glGetSamplerParameterIuiv,
  glGetSamplerParameterIuivEXT,
  glGetSamplerParameterIuivOES,
  glGetSamplerParameterfv,
  glGetSamplerParameteriv,
  glGetSemaphoreParameterui64vEXT,
  glGetSeparableFilter,
  glGetSeparableFilterEXT,
  glGetShaderInfoLog,
  glGetShaderPrecisionFormat,
  glGetShaderSource,
  glGetShaderSourceARB,
  glGetShaderiv,
  glGetShadingRateImagePaletteNV,
  glGetShadingRateSampleLocationivNV,
  glGetSharpenTexFuncSGIS,
  glGetStageIndexNV,
  glGetString,
  glGetStringi,
  glGetSubroutineIndex,
  glGetSubroutineUniformLocation,
  glGetSynciv,
  glGetSyncivAPPLE,
  glGetTexBumpParameterfvATI,
  glGetTexBumpParameterivATI,
  glGetTexEnvfv,
  glGetTexEnviv,
  glGetTexEnvxv,
  glGetTexEnvxvOES,
  glGetTexFilterFuncSGIS,
  glGetTexGendv,
  glGetTexGenfv,
  glGetTexGenfvOES,
  glGetTexGeniv,
  glGetTexGenivOES,
  glGetTexGenxvOES,
  glGetTexImage,
  glGetTexLevelParameterfv,
  glGetTexLevelParameteriv,
  glGetTexLevelParameterxvOES,
  glGetTexParameterIiv,
  glGetTexParameterIivEXT,
  glGetTexParameterIivOES,
  glGetTexParameterIuiv
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glGetProgramEnvParameterIivNV -----------------------------------------------

glGetProgramEnvParameterIivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetProgramEnvParameterIivNV v1 v2 v3 = liftIO $ dyn357 ptr_glGetProgramEnvParameterIivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterIivNV #-}
ptr_glGetProgramEnvParameterIivNV :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetProgramEnvParameterIivNV = unsafePerformIO $ getCommand "glGetProgramEnvParameterIivNV"

-- glGetProgramEnvParameterIuivNV ----------------------------------------------

glGetProgramEnvParameterIuivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glGetProgramEnvParameterIuivNV v1 v2 v3 = liftIO $ dyn221 ptr_glGetProgramEnvParameterIuivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterIuivNV #-}
ptr_glGetProgramEnvParameterIuivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetProgramEnvParameterIuivNV = unsafePerformIO $ getCommand "glGetProgramEnvParameterIuivNV"

-- glGetProgramEnvParameterdvARB -----------------------------------------------

glGetProgramEnvParameterdvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramEnvParameterdvARB v1 v2 v3 = liftIO $ dyn344 ptr_glGetProgramEnvParameterdvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterdvARB #-}
ptr_glGetProgramEnvParameterdvARB :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetProgramEnvParameterdvARB = unsafePerformIO $ getCommand "glGetProgramEnvParameterdvARB"

-- glGetProgramEnvParameterfvARB -----------------------------------------------

glGetProgramEnvParameterfvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramEnvParameterfvARB v1 v2 v3 = liftIO $ dyn278 ptr_glGetProgramEnvParameterfvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterfvARB #-}
ptr_glGetProgramEnvParameterfvARB :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetProgramEnvParameterfvARB = unsafePerformIO $ getCommand "glGetProgramEnvParameterfvARB"

-- glGetProgramInfoLog ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetProgramInfoLog.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetProgramInfoLog.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetProgramInfoLog.xhtml OpenGL 4.x>.
glGetProgramInfoLog
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramInfoLog v1 v2 v3 v4 = liftIO $ dyn345 ptr_glGetProgramInfoLog v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramInfoLog #-}
ptr_glGetProgramInfoLog :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramInfoLog = unsafePerformIO $ getCommand "glGetProgramInfoLog"

-- glGetProgramInterfaceiv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramInterface.xhtml OpenGL 4.x>.
glGetProgramInterfaceiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> GLenum -- ^ @pname@ of type [ProgramInterfacePName](Graphics-GL-Groups.html#ProgramInterfacePName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetProgramInterfaceiv v1 v2 v3 v4 = liftIO $ dyn379 ptr_glGetProgramInterfaceiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramInterfaceiv #-}
ptr_glGetProgramInterfaceiv :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramInterfaceiv = unsafePerformIO $ getCommand "glGetProgramInterfaceiv"

-- glGetProgramLocalParameterIivNV ---------------------------------------------

glGetProgramLocalParameterIivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetProgramLocalParameterIivNV v1 v2 v3 = liftIO $ dyn357 ptr_glGetProgramLocalParameterIivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterIivNV #-}
ptr_glGetProgramLocalParameterIivNV :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetProgramLocalParameterIivNV = unsafePerformIO $ getCommand "glGetProgramLocalParameterIivNV"

-- glGetProgramLocalParameterIuivNV --------------------------------------------

glGetProgramLocalParameterIuivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glGetProgramLocalParameterIuivNV v1 v2 v3 = liftIO $ dyn221 ptr_glGetProgramLocalParameterIuivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterIuivNV #-}
ptr_glGetProgramLocalParameterIuivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetProgramLocalParameterIuivNV = unsafePerformIO $ getCommand "glGetProgramLocalParameterIuivNV"

-- glGetProgramLocalParameterdvARB ---------------------------------------------

glGetProgramLocalParameterdvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramLocalParameterdvARB v1 v2 v3 = liftIO $ dyn344 ptr_glGetProgramLocalParameterdvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterdvARB #-}
ptr_glGetProgramLocalParameterdvARB :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetProgramLocalParameterdvARB = unsafePerformIO $ getCommand "glGetProgramLocalParameterdvARB"

-- glGetProgramLocalParameterfvARB ---------------------------------------------

glGetProgramLocalParameterfvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramLocalParameterfvARB v1 v2 v3 = liftIO $ dyn278 ptr_glGetProgramLocalParameterfvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterfvARB #-}
ptr_glGetProgramLocalParameterfvARB :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetProgramLocalParameterfvARB = unsafePerformIO $ getCommand "glGetProgramLocalParameterfvARB"

-- glGetProgramNamedParameterdvNV ----------------------------------------------

glGetProgramNamedParameterdvNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramNamedParameterdvNV v1 v2 v3 v4 = liftIO $ dyn410 ptr_glGetProgramNamedParameterdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramNamedParameterdvNV #-}
ptr_glGetProgramNamedParameterdvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ())
ptr_glGetProgramNamedParameterdvNV = unsafePerformIO $ getCommand "glGetProgramNamedParameterdvNV"

-- glGetProgramNamedParameterfvNV ----------------------------------------------

glGetProgramNamedParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramNamedParameterfvNV v1 v2 v3 v4 = liftIO $ dyn411 ptr_glGetProgramNamedParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramNamedParameterfvNV #-}
ptr_glGetProgramNamedParameterfvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glGetProgramNamedParameterfvNV = unsafePerformIO $ getCommand "glGetProgramNamedParameterfvNV"

-- glGetProgramParameterdvNV ---------------------------------------------------

glGetProgramParameterdvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramParameterdvNV v1 v2 v3 v4 = liftIO $ dyn412 ptr_glGetProgramParameterdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramParameterdvNV #-}
ptr_glGetProgramParameterdvNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetProgramParameterdvNV = unsafePerformIO $ getCommand "glGetProgramParameterdvNV"

-- glGetProgramParameterfvNV ---------------------------------------------------

glGetProgramParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramParameterfvNV v1 v2 v3 v4 = liftIO $ dyn365 ptr_glGetProgramParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramParameterfvNV #-}
ptr_glGetProgramParameterfvNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetProgramParameterfvNV = unsafePerformIO $ getCommand "glGetProgramParameterfvNV"

-- glGetProgramPipelineInfoLog -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramPipelineInfoLog.xhtml OpenGL 4.x>.
glGetProgramPipelineInfoLog
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramPipelineInfoLog v1 v2 v3 v4 = liftIO $ dyn345 ptr_glGetProgramPipelineInfoLog v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramPipelineInfoLog #-}
ptr_glGetProgramPipelineInfoLog :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramPipelineInfoLog = unsafePerformIO $ getCommand "glGetProgramPipelineInfoLog"

-- glGetProgramPipelineInfoLogEXT ----------------------------------------------

glGetProgramPipelineInfoLogEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramPipelineInfoLogEXT v1 v2 v3 v4 = liftIO $ dyn345 ptr_glGetProgramPipelineInfoLogEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramPipelineInfoLogEXT #-}
ptr_glGetProgramPipelineInfoLogEXT :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramPipelineInfoLogEXT = unsafePerformIO $ getCommand "glGetProgramPipelineInfoLogEXT"

-- glGetProgramPipelineiv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramPipeline.xhtml OpenGL 4.x>.
glGetProgramPipelineiv
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLenum -- ^ @pname@ of type [PipelineParameterName](Graphics-GL-Groups.html#PipelineParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetProgramPipelineiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetProgramPipelineiv v1 v2 v3

{-# NOINLINE ptr_glGetProgramPipelineiv #-}
ptr_glGetProgramPipelineiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramPipelineiv = unsafePerformIO $ getCommand "glGetProgramPipelineiv"

-- glGetProgramPipelineivEXT ---------------------------------------------------

glGetProgramPipelineivEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLenum -- ^ @pname@ of type [PipelineParameterName](Graphics-GL-Groups.html#PipelineParameterName).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetProgramPipelineivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetProgramPipelineivEXT v1 v2 v3

{-# NOINLINE ptr_glGetProgramPipelineivEXT #-}
ptr_glGetProgramPipelineivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramPipelineivEXT = unsafePerformIO $ getCommand "glGetProgramPipelineivEXT"

-- glGetProgramResourceIndex ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceIndex.xhtml OpenGL 4.x>.
glGetProgramResourceIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLuint
glGetProgramResourceIndex v1 v2 v3 = liftIO $ dyn413 ptr_glGetProgramResourceIndex v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceIndex #-}
ptr_glGetProgramResourceIndex :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLuint)
ptr_glGetProgramResourceIndex = unsafePerformIO $ getCommand "glGetProgramResourceIndex"

-- glGetProgramResourceLocation ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceLocation.xhtml OpenGL 4.x>.
glGetProgramResourceLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetProgramResourceLocation v1 v2 v3 = liftIO $ dyn414 ptr_glGetProgramResourceLocation v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceLocation #-}
ptr_glGetProgramResourceLocation :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetProgramResourceLocation = unsafePerformIO $ getCommand "glGetProgramResourceLocation"

-- glGetProgramResourceLocationIndex -------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceLocationIndex.xhtml OpenGL 4.x>.
glGetProgramResourceLocationIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetProgramResourceLocationIndex v1 v2 v3 = liftIO $ dyn414 ptr_glGetProgramResourceLocationIndex v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceLocationIndex #-}
ptr_glGetProgramResourceLocationIndex :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetProgramResourceLocationIndex = unsafePerformIO $ getCommand "glGetProgramResourceLocationIndex"

-- glGetProgramResourceLocationIndexEXT ----------------------------------------

glGetProgramResourceLocationIndexEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetProgramResourceLocationIndexEXT v1 v2 v3 = liftIO $ dyn414 ptr_glGetProgramResourceLocationIndexEXT v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceLocationIndexEXT #-}
ptr_glGetProgramResourceLocationIndexEXT :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetProgramResourceLocationIndexEXT = unsafePerformIO $ getCommand "glGetProgramResourceLocationIndexEXT"

-- glGetProgramResourceName ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceName.xhtml OpenGL 4.x>.
glGetProgramResourceName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramResourceName v1 v2 v3 v4 v5 v6 = liftIO $ dyn317 ptr_glGetProgramResourceName v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetProgramResourceName #-}
ptr_glGetProgramResourceName :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramResourceName = unsafePerformIO $ getCommand "glGetProgramResourceName"

-- glGetProgramResourcefvNV ----------------------------------------------------

glGetProgramResourcefvNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @propCount@.
  -> Ptr GLenum -- ^ @props@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLfloat -- ^ @params@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glGetProgramResourcefvNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn415 ptr_glGetProgramResourcefvNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetProgramResourcefvNV #-}
ptr_glGetProgramResourcefvNV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetProgramResourcefvNV = unsafePerformIO $ getCommand "glGetProgramResourcefvNV"

-- glGetProgramResourceiv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResource.xhtml OpenGL 4.x>.
glGetProgramResourceiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@ of type [ProgramInterface](Graphics-GL-Groups.html#ProgramInterface).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @propCount@.
  -> Ptr GLenum -- ^ @props@ pointing to @propCount@ elements of type [ProgramResourceProperty](Graphics-GL-Groups.html#ProgramResourceProperty).
  -> GLsizei -- ^ @count@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @params@ pointing to @count@ elements of type @GLint@.
  -> m ()
glGetProgramResourceiv v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn416 ptr_glGetProgramResourceiv v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetProgramResourceiv #-}
ptr_glGetProgramResourceiv :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glGetProgramResourceiv = unsafePerformIO $ getCommand "glGetProgramResourceiv"

-- glGetProgramStageiv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramStage.xhtml OpenGL 4.x>.
glGetProgramStageiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLenum -- ^ @pname@ of type [ProgramStagePName](Graphics-GL-Groups.html#ProgramStagePName).
  -> Ptr GLint -- ^ @values@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetProgramStageiv v1 v2 v3 v4 = liftIO $ dyn379 ptr_glGetProgramStageiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramStageiv #-}
ptr_glGetProgramStageiv :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramStageiv = unsafePerformIO $ getCommand "glGetProgramStageiv"

-- glGetProgramStringARB -------------------------------------------------------

glGetProgramStringARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLenum -- ^ @pname@ of type [ProgramStringProperty](Graphics-GL-Groups.html#ProgramStringProperty).
  -> Ptr a -- ^ @string@ pointing to @COMPSIZE(target,pname)@ elements of type @a@.
  -> m ()
glGetProgramStringARB v1 v2 v3 = liftIO $ dyn250 ptr_glGetProgramStringARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramStringARB #-}
ptr_glGetProgramStringARB :: FunPtr (GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetProgramStringARB = unsafePerformIO $ getCommand "glGetProgramStringARB"

-- glGetProgramStringNV --------------------------------------------------------

glGetProgramStringNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLubyte -- ^ @program@ pointing to @COMPSIZE(id,pname)@ elements of type @ProgramCharacterNV@.
  -> m ()
glGetProgramStringNV v1 v2 v3 = liftIO $ dyn417 ptr_glGetProgramStringNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramStringNV #-}
ptr_glGetProgramStringNV :: FunPtr (GLuint -> GLenum -> Ptr GLubyte -> IO ())
ptr_glGetProgramStringNV = unsafePerformIO $ getCommand "glGetProgramStringNV"

-- glGetProgramSubroutineParameteruivNV ----------------------------------------

glGetProgramSubroutineParameteruivNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(target)@ elements of type @GLuint@.
  -> m ()
glGetProgramSubroutineParameteruivNV v1 v2 v3 = liftIO $ dyn221 ptr_glGetProgramSubroutineParameteruivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramSubroutineParameteruivNV #-}
ptr_glGetProgramSubroutineParameteruivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetProgramSubroutineParameteruivNV = unsafePerformIO $ getCommand "glGetProgramSubroutineParameteruivNV"

-- glGetProgramiv --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetProgram.xhtml OpenGL 4.x>.
glGetProgramiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@ of type [ProgramPropertyARB](Graphics-GL-Groups.html#ProgramPropertyARB).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetProgramiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetProgramiv v1 v2 v3

{-# NOINLINE ptr_glGetProgramiv #-}
ptr_glGetProgramiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramiv = unsafePerformIO $ getCommand "glGetProgramiv"

-- glGetProgramivARB -----------------------------------------------------------

glGetProgramivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLenum -- ^ @pname@ of type [ProgramPropertyARB](Graphics-GL-Groups.html#ProgramPropertyARB).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetProgramivARB v1 v2 v3 = liftIO $ dyn140 ptr_glGetProgramivARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramivARB #-}
ptr_glGetProgramivARB :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramivARB = unsafePerformIO $ getCommand "glGetProgramivARB"

-- glGetProgramivNV ------------------------------------------------------------

glGetProgramivNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetProgramivNV v1 v2 v3 = liftIO $ dyn348 ptr_glGetProgramivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramivNV #-}
ptr_glGetProgramivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramivNV = unsafePerformIO $ getCommand "glGetProgramivNV"

-- glGetQueryBufferObjecti64v --------------------------------------------------

glGetQueryBufferObjecti64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjecti64v v1 v2 v3 v4 = liftIO $ dyn418 ptr_glGetQueryBufferObjecti64v v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjecti64v #-}
ptr_glGetQueryBufferObjecti64v :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjecti64v = unsafePerformIO $ getCommand "glGetQueryBufferObjecti64v"

-- glGetQueryBufferObjectiv ----------------------------------------------------

glGetQueryBufferObjectiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjectiv v1 v2 v3 v4 = liftIO $ dyn418 ptr_glGetQueryBufferObjectiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjectiv #-}
ptr_glGetQueryBufferObjectiv :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjectiv = unsafePerformIO $ getCommand "glGetQueryBufferObjectiv"

-- glGetQueryBufferObjectui64v -------------------------------------------------

glGetQueryBufferObjectui64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjectui64v v1 v2 v3 v4 = liftIO $ dyn418 ptr_glGetQueryBufferObjectui64v v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjectui64v #-}
ptr_glGetQueryBufferObjectui64v :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjectui64v = unsafePerformIO $ getCommand "glGetQueryBufferObjectui64v"

-- glGetQueryBufferObjectuiv ---------------------------------------------------

glGetQueryBufferObjectuiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjectuiv v1 v2 v3 v4 = liftIO $ dyn418 ptr_glGetQueryBufferObjectuiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjectuiv #-}
ptr_glGetQueryBufferObjectuiv :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjectuiv = unsafePerformIO $ getCommand "glGetQueryBufferObjectuiv"

-- glGetQueryIndexediv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetQueryIndexed.xhtml OpenGL 4.x>.
glGetQueryIndexediv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [QueryParameterName](Graphics-GL-Groups.html#QueryParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryIndexediv v1 v2 v3 v4 = liftIO $ dyn366 ptr_glGetQueryIndexediv v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryIndexediv #-}
ptr_glGetQueryIndexediv :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryIndexediv = unsafePerformIO $ getCommand "glGetQueryIndexediv"

-- glGetQueryObjecti64v --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjecti64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64@.
  -> m ()
glGetQueryObjecti64v v1 v2 v3 = liftIO $ dyn375 ptr_glGetQueryObjecti64v v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjecti64v #-}
ptr_glGetQueryObjecti64v :: FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetQueryObjecti64v = unsafePerformIO $ getCommand "glGetQueryObjecti64v"

-- glGetQueryObjecti64vEXT -----------------------------------------------------

-- | This command is an alias for 'glGetQueryObjecti64v'.
glGetQueryObjecti64vEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64@.
  -> m ()
glGetQueryObjecti64vEXT v1 v2 v3 = liftIO $ dyn375 ptr_glGetQueryObjecti64vEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjecti64vEXT #-}
ptr_glGetQueryObjecti64vEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetQueryObjecti64vEXT = unsafePerformIO $ getCommand "glGetQueryObjecti64vEXT"

-- glGetQueryObjectiv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetQueryObject.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjectiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryObjectiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetQueryObjectiv v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectiv #-}
ptr_glGetQueryObjectiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryObjectiv = unsafePerformIO $ getCommand "glGetQueryObjectiv"

-- glGetQueryObjectivARB -------------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectiv'.
glGetQueryObjectivARB
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryObjectivARB v1 v2 v3 = liftIO $ dyn348 ptr_glGetQueryObjectivARB v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectivARB #-}
ptr_glGetQueryObjectivARB :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryObjectivARB = unsafePerformIO $ getCommand "glGetQueryObjectivARB"

-- glGetQueryObjectivEXT -------------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectiv'.
glGetQueryObjectivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryObjectivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetQueryObjectivEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectivEXT #-}
ptr_glGetQueryObjectivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryObjectivEXT = unsafePerformIO $ getCommand "glGetQueryObjectivEXT"

-- glGetQueryObjectui64v -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjectui64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLuint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64@.
  -> m ()
glGetQueryObjectui64v v1 v2 v3 = liftIO $ dyn419 ptr_glGetQueryObjectui64v v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectui64v #-}
ptr_glGetQueryObjectui64v :: FunPtr (GLuint -> GLenum -> Ptr GLuint64 -> IO ())
ptr_glGetQueryObjectui64v = unsafePerformIO $ getCommand "glGetQueryObjectui64v"

-- glGetQueryObjectui64vEXT ----------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectui64v'.
glGetQueryObjectui64vEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLuint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64@.
  -> m ()
glGetQueryObjectui64vEXT v1 v2 v3 = liftIO $ dyn419 ptr_glGetQueryObjectui64vEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectui64vEXT #-}
ptr_glGetQueryObjectui64vEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint64 -> IO ())
ptr_glGetQueryObjectui64vEXT = unsafePerformIO $ getCommand "glGetQueryObjectui64vEXT"

-- glGetQueryObjectuiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetQueryObject.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjectuiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetQueryObjectuiv v1 v2 v3 = liftIO $ dyn392 ptr_glGetQueryObjectuiv v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectuiv #-}
ptr_glGetQueryObjectuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetQueryObjectuiv = unsafePerformIO $ getCommand "glGetQueryObjectuiv"

-- glGetQueryObjectuivARB ------------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectuiv'.
glGetQueryObjectuivARB
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetQueryObjectuivARB v1 v2 v3 = liftIO $ dyn392 ptr_glGetQueryObjectuivARB v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectuivARB #-}
ptr_glGetQueryObjectuivARB :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetQueryObjectuivARB = unsafePerformIO $ getCommand "glGetQueryObjectuivARB"

-- glGetQueryObjectuivEXT ------------------------------------------------------

glGetQueryObjectuivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [QueryObjectParameterName](Graphics-GL-Groups.html#QueryObjectParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetQueryObjectuivEXT v1 v2 v3 = liftIO $ dyn392 ptr_glGetQueryObjectuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectuivEXT #-}
ptr_glGetQueryObjectuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetQueryObjectuivEXT = unsafePerformIO $ getCommand "glGetQueryObjectuivEXT"

-- glGetQueryiv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetQueryiv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryiv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryiv.xhtml OpenGL 4.x>.
glGetQueryiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLenum -- ^ @pname@ of type [QueryParameterName](Graphics-GL-Groups.html#QueryParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryiv v1 v2 v3 = liftIO $ dyn140 ptr_glGetQueryiv v1 v2 v3

{-# NOINLINE ptr_glGetQueryiv #-}
ptr_glGetQueryiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryiv = unsafePerformIO $ getCommand "glGetQueryiv"

-- glGetQueryivARB -------------------------------------------------------------

-- | This command is an alias for 'glGetQueryiv'.
glGetQueryivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLenum -- ^ @pname@ of type [QueryParameterName](Graphics-GL-Groups.html#QueryParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryivARB v1 v2 v3 = liftIO $ dyn140 ptr_glGetQueryivARB v1 v2 v3

{-# NOINLINE ptr_glGetQueryivARB #-}
ptr_glGetQueryivARB :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryivARB = unsafePerformIO $ getCommand "glGetQueryivARB"

-- glGetQueryivEXT -------------------------------------------------------------

glGetQueryivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLenum -- ^ @pname@ of type [QueryParameterName](Graphics-GL-Groups.html#QueryParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glGetQueryivEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryivEXT #-}
ptr_glGetQueryivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryivEXT = unsafePerformIO $ getCommand "glGetQueryivEXT"

-- glGetRenderbufferParameteriv ------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetRenderbufferParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetRenderbufferParameter.xhtml OpenGL 4.x>.
glGetRenderbufferParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLenum -- ^ @pname@ of type [RenderbufferParameterName](Graphics-GL-Groups.html#RenderbufferParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetRenderbufferParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glGetRenderbufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetRenderbufferParameteriv #-}
ptr_glGetRenderbufferParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetRenderbufferParameteriv = unsafePerformIO $ getCommand "glGetRenderbufferParameteriv"

-- glGetRenderbufferParameterivEXT ---------------------------------------------

-- | This command is an alias for 'glGetRenderbufferParameteriv'.
glGetRenderbufferParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLenum -- ^ @pname@ of type [RenderbufferParameterName](Graphics-GL-Groups.html#RenderbufferParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetRenderbufferParameterivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glGetRenderbufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetRenderbufferParameterivEXT #-}
ptr_glGetRenderbufferParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetRenderbufferParameterivEXT = unsafePerformIO $ getCommand "glGetRenderbufferParameterivEXT"

-- glGetRenderbufferParameterivOES ---------------------------------------------

glGetRenderbufferParameterivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLenum -- ^ @pname@ of type [RenderbufferParameterName](Graphics-GL-Groups.html#RenderbufferParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetRenderbufferParameterivOES v1 v2 v3 = liftIO $ dyn140 ptr_glGetRenderbufferParameterivOES v1 v2 v3

{-# NOINLINE ptr_glGetRenderbufferParameterivOES #-}
ptr_glGetRenderbufferParameterivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetRenderbufferParameterivOES = unsafePerformIO $ getCommand "glGetRenderbufferParameterivOES"

-- glGetSamplerParameterIiv ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameterIiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameterIiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetSamplerParameterIiv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIiv #-}
ptr_glGetSamplerParameterIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameterIiv = unsafePerformIO $ getCommand "glGetSamplerParameterIiv"

-- glGetSamplerParameterIivEXT -------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIiv'.
glGetSamplerParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameterIivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetSamplerParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIivEXT #-}
ptr_glGetSamplerParameterIivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameterIivEXT = unsafePerformIO $ getCommand "glGetSamplerParameterIivEXT"

-- glGetSamplerParameterIivOES -------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIiv'.
glGetSamplerParameterIivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameterIivOES v1 v2 v3 = liftIO $ dyn348 ptr_glGetSamplerParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIivOES #-}
ptr_glGetSamplerParameterIivOES :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameterIivOES = unsafePerformIO $ getCommand "glGetSamplerParameterIivOES"

-- glGetSamplerParameterIuiv ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameterIuiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetSamplerParameterIuiv v1 v2 v3 = liftIO $ dyn392 ptr_glGetSamplerParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIuiv #-}
ptr_glGetSamplerParameterIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetSamplerParameterIuiv = unsafePerformIO $ getCommand "glGetSamplerParameterIuiv"

-- glGetSamplerParameterIuivEXT ------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIuiv'.
glGetSamplerParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetSamplerParameterIuivEXT v1 v2 v3 = liftIO $ dyn392 ptr_glGetSamplerParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIuivEXT #-}
ptr_glGetSamplerParameterIuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetSamplerParameterIuivEXT = unsafePerformIO $ getCommand "glGetSamplerParameterIuivEXT"

-- glGetSamplerParameterIuivOES ------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIuiv'.
glGetSamplerParameterIuivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetSamplerParameterIuivOES v1 v2 v3 = liftIO $ dyn392 ptr_glGetSamplerParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIuivOES #-}
ptr_glGetSamplerParameterIuivOES :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetSamplerParameterIuivOES = unsafePerformIO $ getCommand "glGetSamplerParameterIuivOES"

-- glGetSamplerParameterfv -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameterfv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterF](Graphics-GL-Groups.html#SamplerParameterF).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetSamplerParameterfv v1 v2 v3 = liftIO $ dyn364 ptr_glGetSamplerParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterfv #-}
ptr_glGetSamplerParameterfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetSamplerParameterfv = unsafePerformIO $ getCommand "glGetSamplerParameterfv"

-- glGetSamplerParameteriv -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameteriv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameteriv v1 v2 v3 = liftIO $ dyn348 ptr_glGetSamplerParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameteriv #-}
ptr_glGetSamplerParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameteriv = unsafePerformIO $ getCommand "glGetSamplerParameteriv"

-- glGetSemaphoreParameterui64vEXT ---------------------------------------------

glGetSemaphoreParameterui64vEXT
  :: MonadIO m
  => GLuint -- ^ @semaphore@.
  -> GLenum -- ^ @pname@ of type [SemaphoreParameterName](Graphics-GL-Groups.html#SemaphoreParameterName).
  -> Ptr GLuint64 -- ^ @params@.
  -> m ()
glGetSemaphoreParameterui64vEXT v1 v2 v3 = liftIO $ dyn419 ptr_glGetSemaphoreParameterui64vEXT v1 v2 v3

{-# NOINLINE ptr_glGetSemaphoreParameterui64vEXT #-}
ptr_glGetSemaphoreParameterui64vEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint64 -> IO ())
ptr_glGetSemaphoreParameterui64vEXT = unsafePerformIO $ getCommand "glGetSemaphoreParameterui64vEXT"

-- glGetSeparableFilter --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetSeparableFilter.xml OpenGL 2.x>.
glGetSeparableFilter
  :: MonadIO m
  => GLenum -- ^ @target@ of type [SeparableTargetEXT](Graphics-GL-Groups.html#SeparableTargetEXT).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @row@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> Ptr b -- ^ @column@ pointing to @COMPSIZE(target,format,type)@ elements of type @b@.
  -> Ptr c -- ^ @span@ pointing to @COMPSIZE(target,format,type)@ elements of type @c@.
  -> m ()
glGetSeparableFilter v1 v2 v3 v4 v5 v6 = liftIO $ dyn420 ptr_glGetSeparableFilter v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetSeparableFilter #-}
ptr_glGetSeparableFilter :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> Ptr b -> Ptr c -> IO ())
ptr_glGetSeparableFilter = unsafePerformIO $ getCommand "glGetSeparableFilter"

-- glGetSeparableFilterEXT -----------------------------------------------------

glGetSeparableFilterEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [SeparableTargetEXT](Graphics-GL-Groups.html#SeparableTargetEXT).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @row@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> Ptr b -- ^ @column@ pointing to @COMPSIZE(target,format,type)@ elements of type @b@.
  -> Ptr c -- ^ @span@ pointing to @COMPSIZE(target,format,type)@ elements of type @c@.
  -> m ()
glGetSeparableFilterEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn420 ptr_glGetSeparableFilterEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetSeparableFilterEXT #-}
ptr_glGetSeparableFilterEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> Ptr b -> Ptr c -> IO ())
ptr_glGetSeparableFilterEXT = unsafePerformIO $ getCommand "glGetSeparableFilterEXT"

-- glGetShaderInfoLog ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetShaderInfoLog.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetShaderInfoLog.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetShaderInfoLog.xhtml OpenGL 4.x>.
glGetShaderInfoLog
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetShaderInfoLog v1 v2 v3 v4 = liftIO $ dyn345 ptr_glGetShaderInfoLog v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderInfoLog #-}
ptr_glGetShaderInfoLog :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetShaderInfoLog = unsafePerformIO $ getCommand "glGetShaderInfoLog"

-- glGetShaderPrecisionFormat --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetShaderPrecisionFormat.xhtml OpenGL 4.x>.
glGetShaderPrecisionFormat
  :: MonadIO m
  => GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLenum -- ^ @precisiontype@ of type [PrecisionType](Graphics-GL-Groups.html#PrecisionType).
  -> Ptr GLint -- ^ @range@ pointing to @2@ elements of type @GLint@.
  -> Ptr GLint -- ^ @precision@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetShaderPrecisionFormat v1 v2 v3 v4 = liftIO $ dyn421 ptr_glGetShaderPrecisionFormat v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderPrecisionFormat #-}
ptr_glGetShaderPrecisionFormat :: FunPtr (GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ())
ptr_glGetShaderPrecisionFormat = unsafePerformIO $ getCommand "glGetShaderPrecisionFormat"

-- glGetShaderSource -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetShaderSource.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetShaderSource.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetShaderSource.xhtml OpenGL 4.x>.
glGetShaderSource
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @source@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetShaderSource v1 v2 v3 v4 = liftIO $ dyn345 ptr_glGetShaderSource v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderSource #-}
ptr_glGetShaderSource :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetShaderSource = unsafePerformIO $ getCommand "glGetShaderSource"

-- glGetShaderSourceARB --------------------------------------------------------

-- | This command is an alias for 'glGetShaderSource'.
glGetShaderSourceARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> GLsizei -- ^ @maxLength@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLcharARB -- ^ @source@ pointing to @maxLength@ elements of type @GLcharARB@.
  -> m ()
glGetShaderSourceARB v1 v2 v3 v4 = liftIO $ dyn353 ptr_glGetShaderSourceARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderSourceARB #-}
ptr_glGetShaderSourceARB :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ())
ptr_glGetShaderSourceARB = unsafePerformIO $ getCommand "glGetShaderSourceARB"

-- glGetShaderiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetShader.xhtml OpenGL 4.x>.
glGetShaderiv
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLenum -- ^ @pname@ of type [ShaderParameterName](Graphics-GL-Groups.html#ShaderParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetShaderiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetShaderiv v1 v2 v3

{-# NOINLINE ptr_glGetShaderiv #-}
ptr_glGetShaderiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetShaderiv = unsafePerformIO $ getCommand "glGetShaderiv"

-- glGetShadingRateImagePaletteNV ----------------------------------------------

glGetShadingRateImagePaletteNV
  :: MonadIO m
  => GLuint -- ^ @viewport@.
  -> GLuint -- ^ @entry@.
  -> Ptr GLenum -- ^ @rate@ pointing to @1@ element of type @GLenum@.
  -> m ()
glGetShadingRateImagePaletteNV v1 v2 v3 = liftIO $ dyn422 ptr_glGetShadingRateImagePaletteNV v1 v2 v3

{-# NOINLINE ptr_glGetShadingRateImagePaletteNV #-}
ptr_glGetShadingRateImagePaletteNV :: FunPtr (GLuint -> GLuint -> Ptr GLenum -> IO ())
ptr_glGetShadingRateImagePaletteNV = unsafePerformIO $ getCommand "glGetShadingRateImagePaletteNV"

-- glGetShadingRateSampleLocationivNV ------------------------------------------

glGetShadingRateSampleLocationivNV
  :: MonadIO m
  => GLenum -- ^ @rate@.
  -> GLuint -- ^ @samples@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @location@ pointing to @3@ elements of type @GLint@.
  -> m ()
glGetShadingRateSampleLocationivNV v1 v2 v3 v4 = liftIO $ dyn423 ptr_glGetShadingRateSampleLocationivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetShadingRateSampleLocationivNV #-}
ptr_glGetShadingRateSampleLocationivNV :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLint -> IO ())
ptr_glGetShadingRateSampleLocationivNV = unsafePerformIO $ getCommand "glGetShadingRateSampleLocationivNV"

-- glGetSharpenTexFuncSGIS -----------------------------------------------------

glGetSharpenTexFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target)@ elements of type @GLfloat@.
  -> m ()
glGetSharpenTexFuncSGIS v1 v2 = liftIO $ dyn101 ptr_glGetSharpenTexFuncSGIS v1 v2

{-# NOINLINE ptr_glGetSharpenTexFuncSGIS #-}
ptr_glGetSharpenTexFuncSGIS :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetSharpenTexFuncSGIS = unsafePerformIO $ getCommand "glGetSharpenTexFuncSGIS"

-- glGetStageIndexNV -----------------------------------------------------------

glGetStageIndexNV
  :: MonadIO m
  => GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> m GLushort
glGetStageIndexNV v1 = liftIO $ dyn424 ptr_glGetStageIndexNV v1

{-# NOINLINE ptr_glGetStageIndexNV #-}
ptr_glGetStageIndexNV :: FunPtr (GLenum -> IO GLushort)
ptr_glGetStageIndexNV = unsafePerformIO $ getCommand "glGetStageIndexNV"

-- glGetString -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetString.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetString.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetString.xhtml OpenGL 4.x>.
glGetString
  :: MonadIO m
  => GLenum -- ^ @name@ of type [StringName](Graphics-GL-Groups.html#StringName).
  -> m (Ptr GLubyte) -- ^ pointing to elements of type @String@.
glGetString v1 = liftIO $ dyn425 ptr_glGetString v1

{-# NOINLINE ptr_glGetString #-}
ptr_glGetString :: FunPtr (GLenum -> IO (Ptr GLubyte))
ptr_glGetString = unsafePerformIO $ getCommand "glGetString"

-- glGetStringi ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetString.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetString.xhtml OpenGL 4.x>.
glGetStringi
  :: MonadIO m
  => GLenum -- ^ @name@ of type [StringName](Graphics-GL-Groups.html#StringName).
  -> GLuint -- ^ @index@.
  -> m (Ptr GLubyte) -- ^ pointing to elements of type @String@.
glGetStringi v1 v2 = liftIO $ dyn426 ptr_glGetStringi v1 v2

{-# NOINLINE ptr_glGetStringi #-}
ptr_glGetStringi :: FunPtr (GLenum -> GLuint -> IO (Ptr GLubyte))
ptr_glGetStringi = unsafePerformIO $ getCommand "glGetStringi"

-- glGetSubroutineIndex --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetSubroutineIndex.xhtml OpenGL 4.x>.
glGetSubroutineIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> Ptr GLchar -- ^ @name@.
  -> m GLuint
glGetSubroutineIndex v1 v2 v3 = liftIO $ dyn413 ptr_glGetSubroutineIndex v1 v2 v3

{-# NOINLINE ptr_glGetSubroutineIndex #-}
ptr_glGetSubroutineIndex :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLuint)
ptr_glGetSubroutineIndex = unsafePerformIO $ getCommand "glGetSubroutineIndex"

-- glGetSubroutineUniformLocation ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetSubroutineUniformLocation.xhtml OpenGL 4.x>.
glGetSubroutineUniformLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetSubroutineUniformLocation v1 v2 v3 = liftIO $ dyn414 ptr_glGetSubroutineUniformLocation v1 v2 v3

{-# NOINLINE ptr_glGetSubroutineUniformLocation #-}
ptr_glGetSubroutineUniformLocation :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetSubroutineUniformLocation = unsafePerformIO $ getCommand "glGetSubroutineUniformLocation"

-- glGetSynciv -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSync.xhtml OpenGL 4.x>.
glGetSynciv
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> GLenum -- ^ @pname@ of type [SyncParameterName](Graphics-GL-Groups.html#SyncParameterName).
  -> GLsizei -- ^ @count@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @values@ pointing to @count@ elements of type @GLint@.
  -> m ()
glGetSynciv v1 v2 v3 v4 v5 = liftIO $ dyn427 ptr_glGetSynciv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetSynciv #-}
ptr_glGetSynciv :: FunPtr (GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glGetSynciv = unsafePerformIO $ getCommand "glGetSynciv"

-- glGetSyncivAPPLE ------------------------------------------------------------

-- | This command is an alias for 'glGetSynciv'.
glGetSyncivAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> GLenum -- ^ @pname@ of type [SyncParameterName](Graphics-GL-Groups.html#SyncParameterName).
  -> GLsizei -- ^ @count@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLint -- ^ @values@ pointing to @count@ elements of type @GLint@.
  -> m ()
glGetSyncivAPPLE v1 v2 v3 v4 v5 = liftIO $ dyn427 ptr_glGetSyncivAPPLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetSyncivAPPLE #-}
ptr_glGetSyncivAPPLE :: FunPtr (GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glGetSyncivAPPLE = unsafePerformIO $ getCommand "glGetSyncivAPPLE"

-- glGetTexBumpParameterfvATI --------------------------------------------------

glGetTexBumpParameterfvATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetTexBumpParameterATI](Graphics-GL-Groups.html#GetTexBumpParameterATI).
  -> Ptr GLfloat -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexBumpParameterfvATI v1 v2 = liftIO $ dyn101 ptr_glGetTexBumpParameterfvATI v1 v2

{-# NOINLINE ptr_glGetTexBumpParameterfvATI #-}
ptr_glGetTexBumpParameterfvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexBumpParameterfvATI = unsafePerformIO $ getCommand "glGetTexBumpParameterfvATI"

-- glGetTexBumpParameterivATI --------------------------------------------------

glGetTexBumpParameterivATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetTexBumpParameterATI](Graphics-GL-Groups.html#GetTexBumpParameterATI).
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexBumpParameterivATI v1 v2 = liftIO $ dyn143 ptr_glGetTexBumpParameterivATI v1 v2

{-# NOINLINE ptr_glGetTexBumpParameterivATI #-}
ptr_glGetTexBumpParameterivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glGetTexBumpParameterivATI = unsafePerformIO $ getCommand "glGetTexBumpParameterivATI"

-- glGetTexEnvfv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml OpenGL 2.x>.
glGetTexEnvfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexEnvfv v1 v2 v3 = liftIO $ dyn139 ptr_glGetTexEnvfv v1 v2 v3

{-# NOINLINE ptr_glGetTexEnvfv #-}
ptr_glGetTexEnvfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexEnvfv = unsafePerformIO $ getCommand "glGetTexEnvfv"

-- glGetTexEnviv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml OpenGL 2.x>.
glGetTexEnviv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexEnviv v1 v2 v3 = liftIO $ dyn140 ptr_glGetTexEnviv v1 v2 v3

{-# NOINLINE ptr_glGetTexEnviv #-}
ptr_glGetTexEnviv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexEnviv = unsafePerformIO $ getCommand "glGetTexEnviv"

-- glGetTexEnvxv ---------------------------------------------------------------

glGetTexEnvxv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexEnvxv v1 v2 v3 = liftIO $ dyn170 ptr_glGetTexEnvxv v1 v2 v3

{-# NOINLINE ptr_glGetTexEnvxv #-}
ptr_glGetTexEnvxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexEnvxv = unsafePerformIO $ getCommand "glGetTexEnvxv"

-- glGetTexEnvxvOES ------------------------------------------------------------

glGetTexEnvxvOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexEnvxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glGetTexEnvxvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexEnvxvOES #-}
ptr_glGetTexEnvxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexEnvxvOES = unsafePerformIO $ getCommand "glGetTexEnvxvOES"

-- glGetTexFilterFuncSGIS ------------------------------------------------------

glGetTexFilterFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @filter@ of type [TextureFilterSGIS](Graphics-GL-Groups.html#TextureFilterSGIS).
  -> Ptr GLfloat -- ^ @weights@ pointing to @COMPSIZE(target,filter)@ elements of type @GLfloat@.
  -> m ()
glGetTexFilterFuncSGIS v1 v2 v3 = liftIO $ dyn139 ptr_glGetTexFilterFuncSGIS v1 v2 v3

{-# NOINLINE ptr_glGetTexFilterFuncSGIS #-}
ptr_glGetTexFilterFuncSGIS :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexFilterFuncSGIS = unsafePerformIO $ getCommand "glGetTexFilterFuncSGIS"

-- glGetTexGendv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml OpenGL 2.x>.
glGetTexGendv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetTexGendv v1 v2 v3 = liftIO $ dyn368 ptr_glGetTexGendv v1 v2 v3

{-# NOINLINE ptr_glGetTexGendv #-}
ptr_glGetTexGendv :: FunPtr (GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetTexGendv = unsafePerformIO $ getCommand "glGetTexGendv"

-- glGetTexGenfv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml OpenGL 2.x>.
glGetTexGenfv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexGenfv v1 v2 v3 = liftIO $ dyn139 ptr_glGetTexGenfv v1 v2 v3

{-# NOINLINE ptr_glGetTexGenfv #-}
ptr_glGetTexGenfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexGenfv = unsafePerformIO $ getCommand "glGetTexGenfv"

-- glGetTexGenfvOES ------------------------------------------------------------

glGetTexGenfvOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexGenfvOES v1 v2 v3 = liftIO $ dyn139 ptr_glGetTexGenfvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexGenfvOES #-}
ptr_glGetTexGenfvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexGenfvOES = unsafePerformIO $ getCommand "glGetTexGenfvOES"

-- glGetTexGeniv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml OpenGL 2.x>.
glGetTexGeniv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexGeniv v1 v2 v3 = liftIO $ dyn140 ptr_glGetTexGeniv v1 v2 v3

{-# NOINLINE ptr_glGetTexGeniv #-}
ptr_glGetTexGeniv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexGeniv = unsafePerformIO $ getCommand "glGetTexGeniv"

-- glGetTexGenivOES ------------------------------------------------------------

glGetTexGenivOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexGenivOES v1 v2 v3 = liftIO $ dyn140 ptr_glGetTexGenivOES v1 v2 v3

{-# NOINLINE ptr_glGetTexGenivOES #-}
ptr_glGetTexGenivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexGenivOES = unsafePerformIO $ getCommand "glGetTexGenivOES"

-- glGetTexGenxvOES ------------------------------------------------------------

glGetTexGenxvOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexGenxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glGetTexGenxvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexGenxvOES #-}
ptr_glGetTexGenxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexGenxvOES = unsafePerformIO $ getCommand "glGetTexGenxvOES"

-- glGetTexImage ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexImage.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexImage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml OpenGL 4.x>.
glGetTexImage
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(target,level,format,type)@ elements of type @a@.
  -> m ()
glGetTexImage v1 v2 v3 v4 v5 = liftIO $ dyn428 ptr_glGetTexImage v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetTexImage #-}
ptr_glGetTexImage :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetTexImage = unsafePerformIO $ getCommand "glGetTexImage"

-- glGetTexLevelParameterfv ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexLevelParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexLevelParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTexLevelParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexLevelParameterfv v1 v2 v3 v4 = liftIO $ dyn429 ptr_glGetTexLevelParameterfv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTexLevelParameterfv #-}
ptr_glGetTexLevelParameterfv :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexLevelParameterfv = unsafePerformIO $ getCommand "glGetTexLevelParameterfv"

-- glGetTexLevelParameteriv ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexLevelParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexLevelParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTexLevelParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexLevelParameteriv v1 v2 v3 v4 = liftIO $ dyn430 ptr_glGetTexLevelParameteriv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTexLevelParameteriv #-}
ptr_glGetTexLevelParameteriv :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexLevelParameteriv = unsafePerformIO $ getCommand "glGetTexLevelParameteriv"

-- glGetTexLevelParameterxvOES -------------------------------------------------

glGetTexLevelParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexLevelParameterxvOES v1 v2 v3 v4 = liftIO $ dyn431 ptr_glGetTexLevelParameterxvOES v1 v2 v3 v4

{-# NOINLINE ptr_glGetTexLevelParameterxvOES #-}
ptr_glGetTexLevelParameterxvOES :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexLevelParameterxvOES = unsafePerformIO $ getCommand "glGetTexLevelParameterxvOES"

-- glGetTexParameterIiv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameterIiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameterIiv v1 v2 v3 = liftIO $ dyn140 ptr_glGetTexParameterIiv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIiv #-}
ptr_glGetTexParameterIiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameterIiv = unsafePerformIO $ getCommand "glGetTexParameterIiv"

-- glGetTexParameterIivEXT -----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIiv'.
glGetTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameterIivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glGetTexParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIivEXT #-}
ptr_glGetTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameterIivEXT = unsafePerformIO $ getCommand "glGetTexParameterIivEXT"

-- glGetTexParameterIivOES -----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIiv'.
glGetTexParameterIivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameterIivOES v1 v2 v3 = liftIO $ dyn140 ptr_glGetTexParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIivOES #-}
ptr_glGetTexParameterIivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameterIivOES = unsafePerformIO $ getCommand "glGetTexParameterIivOES"

-- glGetTexParameterIuiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameterIuiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTexParameterIuiv v1 v2 v3 = liftIO $ dyn432 ptr_glGetTexParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIuiv #-}
ptr_glGetTexParameterIuiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTexParameterIuiv = unsafePerformIO $ getCommand "glGetTexParameterIuiv"

