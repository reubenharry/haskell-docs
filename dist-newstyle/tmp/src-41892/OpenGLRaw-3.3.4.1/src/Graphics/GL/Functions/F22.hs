{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F22
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

module Graphics.GL.Functions.F22 (
  glProgramUniform4iv,
  glProgramUniform4ivEXT,
  glProgramUniform4ui,
  glProgramUniform4ui64ARB,
  glProgramUniform4ui64NV,
  glProgramUniform4ui64vARB,
  glProgramUniform4ui64vNV,
  glProgramUniform4uiEXT,
  glProgramUniform4uiv,
  glProgramUniform4uivEXT,
  glProgramUniformHandleui64ARB,
  glProgramUniformHandleui64IMG,
  glProgramUniformHandleui64NV,
  glProgramUniformHandleui64vARB,
  glProgramUniformHandleui64vIMG,
  glProgramUniformHandleui64vNV,
  glProgramUniformMatrix2dv,
  glProgramUniformMatrix2dvEXT,
  glProgramUniformMatrix2fv,
  glProgramUniformMatrix2fvEXT,
  glProgramUniformMatrix2x3dv,
  glProgramUniformMatrix2x3dvEXT,
  glProgramUniformMatrix2x3fv,
  glProgramUniformMatrix2x3fvEXT,
  glProgramUniformMatrix2x4dv,
  glProgramUniformMatrix2x4dvEXT,
  glProgramUniformMatrix2x4fv,
  glProgramUniformMatrix2x4fvEXT,
  glProgramUniformMatrix3dv,
  glProgramUniformMatrix3dvEXT,
  glProgramUniformMatrix3fv,
  glProgramUniformMatrix3fvEXT,
  glProgramUniformMatrix3x2dv,
  glProgramUniformMatrix3x2dvEXT,
  glProgramUniformMatrix3x2fv,
  glProgramUniformMatrix3x2fvEXT,
  glProgramUniformMatrix3x4dv,
  glProgramUniformMatrix3x4dvEXT,
  glProgramUniformMatrix3x4fv,
  glProgramUniformMatrix3x4fvEXT,
  glProgramUniformMatrix4dv,
  glProgramUniformMatrix4dvEXT,
  glProgramUniformMatrix4fv,
  glProgramUniformMatrix4fvEXT,
  glProgramUniformMatrix4x2dv,
  glProgramUniformMatrix4x2dvEXT,
  glProgramUniformMatrix4x2fv,
  glProgramUniformMatrix4x2fvEXT,
  glProgramUniformMatrix4x3dv,
  glProgramUniformMatrix4x3dvEXT,
  glProgramUniformMatrix4x3fv,
  glProgramUniformMatrix4x3fvEXT,
  glProgramUniformui64NV,
  glProgramUniformui64vNV,
  glProgramVertexLimitNV,
  glProvokingVertex,
  glProvokingVertexEXT,
  glPushAttrib,
  glPushClientAttrib,
  glPushClientAttribDefaultEXT,
  glPushDebugGroup,
  glPushDebugGroupKHR,
  glPushGroupMarkerEXT,
  glPushMatrix,
  glPushName,
  glQueryCounter,
  glQueryCounterEXT,
  glQueryMatrixxOES,
  glQueryObjectParameteruiAMD,
  glQueryResourceNV,
  glQueryResourceTagNV,
  glRasterPos2d,
  glRasterPos2dv,
  glRasterPos2f,
  glRasterPos2fv,
  glRasterPos2i,
  glRasterPos2iv,
  glRasterPos2s,
  glRasterPos2sv,
  glRasterPos2xOES,
  glRasterPos2xvOES,
  glRasterPos3d,
  glRasterPos3dv,
  glRasterPos3f,
  glRasterPos3fv,
  glRasterPos3i,
  glRasterPos3iv,
  glRasterPos3s,
  glRasterPos3sv,
  glRasterPos3xOES,
  glRasterPos3xvOES,
  glRasterPos4d,
  glRasterPos4dv,
  glRasterPos4f,
  glRasterPos4fv,
  glRasterPos4i,
  glRasterPos4iv,
  glRasterPos4s,
  glRasterPos4sv,
  glRasterPos4xOES
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glProgramUniform4iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glProgramUniform4iv v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform4iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4iv #-}
ptr_glProgramUniform4iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform4iv = unsafePerformIO $ getCommand "glProgramUniform4iv"

-- glProgramUniform4ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4iv'.
glProgramUniform4ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glProgramUniform4ivEXT v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform4ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4ivEXT #-}
ptr_glProgramUniform4ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform4ivEXT = unsafePerformIO $ getCommand "glProgramUniform4ivEXT"

-- glProgramUniform4ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glProgramUniform4ui v1 v2 v3 v4 v5 v6 = liftIO $ dyn701 ptr_glProgramUniform4ui v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4ui #-}
ptr_glProgramUniform4ui :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform4ui = unsafePerformIO $ getCommand "glProgramUniform4ui"

-- glProgramUniform4ui64ARB ----------------------------------------------------

glProgramUniform4ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> GLuint64 -- ^ @w@.
  -> m ()
glProgramUniform4ui64ARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn702 ptr_glProgramUniform4ui64ARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4ui64ARB #-}
ptr_glProgramUniform4ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glProgramUniform4ui64ARB = unsafePerformIO $ getCommand "glProgramUniform4ui64ARB"

-- glProgramUniform4ui64NV -----------------------------------------------------

glProgramUniform4ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> GLuint64EXT -- ^ @w@.
  -> m ()
glProgramUniform4ui64NV v1 v2 v3 v4 v5 v6 = liftIO $ dyn703 ptr_glProgramUniform4ui64NV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4ui64NV #-}
ptr_glProgramUniform4ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glProgramUniform4ui64NV = unsafePerformIO $ getCommand "glProgramUniform4ui64NV"

-- glProgramUniform4ui64vARB ---------------------------------------------------

glProgramUniform4ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*4@ elements of type @GLuint64@.
  -> m ()
glProgramUniform4ui64vARB v1 v2 v3 v4 = liftIO $ dyn482 ptr_glProgramUniform4ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4ui64vARB #-}
ptr_glProgramUniform4ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform4ui64vARB = unsafePerformIO $ getCommand "glProgramUniform4ui64vARB"

-- glProgramUniform4ui64vNV ----------------------------------------------------

glProgramUniform4ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform4ui64vNV v1 v2 v3 v4 = liftIO $ dyn679 ptr_glProgramUniform4ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4ui64vNV #-}
ptr_glProgramUniform4ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform4ui64vNV = unsafePerformIO $ getCommand "glProgramUniform4ui64vNV"

-- glProgramUniform4uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4ui'.
glProgramUniform4uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glProgramUniform4uiEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn701 ptr_glProgramUniform4uiEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4uiEXT #-}
ptr_glProgramUniform4uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform4uiEXT = unsafePerformIO $ getCommand "glProgramUniform4uiEXT"

-- glProgramUniform4uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glProgramUniform4uiv v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform4uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4uiv #-}
ptr_glProgramUniform4uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform4uiv = unsafePerformIO $ getCommand "glProgramUniform4uiv"

-- glProgramUniform4uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform4uiv'.
glProgramUniform4uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glProgramUniform4uivEXT v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform4uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4uivEXT #-}
ptr_glProgramUniform4uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform4uivEXT = unsafePerformIO $ getCommand "glProgramUniform4uivEXT"

-- glProgramUniformHandleui64ARB -----------------------------------------------

glProgramUniformHandleui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glProgramUniformHandleui64ARB v1 v2 v3 = liftIO $ dyn677 ptr_glProgramUniformHandleui64ARB v1 v2 v3

{-# NOINLINE ptr_glProgramUniformHandleui64ARB #-}
ptr_glProgramUniformHandleui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
ptr_glProgramUniformHandleui64ARB = unsafePerformIO $ getCommand "glProgramUniformHandleui64ARB"

-- glProgramUniformHandleui64IMG -----------------------------------------------

-- | This command is an alias for 'glProgramUniformHandleui64ARB'.
glProgramUniformHandleui64IMG
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glProgramUniformHandleui64IMG v1 v2 v3 = liftIO $ dyn677 ptr_glProgramUniformHandleui64IMG v1 v2 v3

{-# NOINLINE ptr_glProgramUniformHandleui64IMG #-}
ptr_glProgramUniformHandleui64IMG :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
ptr_glProgramUniformHandleui64IMG = unsafePerformIO $ getCommand "glProgramUniformHandleui64IMG"

-- glProgramUniformHandleui64NV ------------------------------------------------

glProgramUniformHandleui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glProgramUniformHandleui64NV v1 v2 v3 = liftIO $ dyn677 ptr_glProgramUniformHandleui64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniformHandleui64NV #-}
ptr_glProgramUniformHandleui64NV :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
ptr_glProgramUniformHandleui64NV = unsafePerformIO $ getCommand "glProgramUniformHandleui64NV"

-- glProgramUniformHandleui64vARB ----------------------------------------------

glProgramUniformHandleui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @values@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glProgramUniformHandleui64vARB v1 v2 v3 v4 = liftIO $ dyn482 ptr_glProgramUniformHandleui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniformHandleui64vARB #-}
ptr_glProgramUniformHandleui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniformHandleui64vARB = unsafePerformIO $ getCommand "glProgramUniformHandleui64vARB"

-- glProgramUniformHandleui64vIMG ----------------------------------------------

-- | This command is an alias for 'glProgramUniformHandleui64vARB'.
glProgramUniformHandleui64vIMG
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @values@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glProgramUniformHandleui64vIMG v1 v2 v3 v4 = liftIO $ dyn482 ptr_glProgramUniformHandleui64vIMG v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniformHandleui64vIMG #-}
ptr_glProgramUniformHandleui64vIMG :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniformHandleui64vIMG = unsafePerformIO $ getCommand "glProgramUniformHandleui64vIMG"

-- glProgramUniformHandleui64vNV -----------------------------------------------

glProgramUniformHandleui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @values@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glProgramUniformHandleui64vNV v1 v2 v3 v4 = liftIO $ dyn482 ptr_glProgramUniformHandleui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniformHandleui64vNV #-}
ptr_glProgramUniformHandleui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniformHandleui64vNV = unsafePerformIO $ getCommand "glProgramUniformHandleui64vNV"

-- glProgramUniformMatrix2dv ---------------------------------------------------

glProgramUniformMatrix2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix2dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2dv #-}
ptr_glProgramUniformMatrix2dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2dv = unsafePerformIO $ getCommand "glProgramUniformMatrix2dv"

-- glProgramUniformMatrix2dvEXT ------------------------------------------------

glProgramUniformMatrix2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix2dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2dvEXT #-}
ptr_glProgramUniformMatrix2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2dvEXT"

-- glProgramUniformMatrix2fv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix2fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2fv #-}
ptr_glProgramUniformMatrix2fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2fv = unsafePerformIO $ getCommand "glProgramUniformMatrix2fv"

-- glProgramUniformMatrix2fvEXT ------------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix2fv'.
glProgramUniformMatrix2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix2fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2fvEXT #-}
ptr_glProgramUniformMatrix2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2fvEXT"

-- glProgramUniformMatrix2x3dv -------------------------------------------------

glProgramUniformMatrix2x3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x3dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix2x3dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3dv #-}
ptr_glProgramUniformMatrix2x3dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x3dv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3dv"

-- glProgramUniformMatrix2x3dvEXT ----------------------------------------------

glProgramUniformMatrix2x3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x3dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix2x3dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3dvEXT #-}
ptr_glProgramUniformMatrix2x3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x3dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3dvEXT"

-- glProgramUniformMatrix2x3fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix2x3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x3fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix2x3fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3fv #-}
ptr_glProgramUniformMatrix2x3fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x3fv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3fv"

-- glProgramUniformMatrix2x3fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix2x3fv'.
glProgramUniformMatrix2x3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x3fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix2x3fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3fvEXT #-}
ptr_glProgramUniformMatrix2x3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x3fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3fvEXT"

-- glProgramUniformMatrix2x4dv -------------------------------------------------

glProgramUniformMatrix2x4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x4dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix2x4dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4dv #-}
ptr_glProgramUniformMatrix2x4dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x4dv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4dv"

-- glProgramUniformMatrix2x4dvEXT ----------------------------------------------

glProgramUniformMatrix2x4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x4dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix2x4dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4dvEXT #-}
ptr_glProgramUniformMatrix2x4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x4dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4dvEXT"

-- glProgramUniformMatrix2x4fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix2x4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x4fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix2x4fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4fv #-}
ptr_glProgramUniformMatrix2x4fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x4fv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4fv"

-- glProgramUniformMatrix2x4fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix2x4fv'.
glProgramUniformMatrix2x4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix2x4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4fvEXT #-}
ptr_glProgramUniformMatrix2x4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x4fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4fvEXT"

-- glProgramUniformMatrix3dv ---------------------------------------------------

glProgramUniformMatrix3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*9@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix3dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3dv #-}
ptr_glProgramUniformMatrix3dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3dv = unsafePerformIO $ getCommand "glProgramUniformMatrix3dv"

-- glProgramUniformMatrix3dvEXT ------------------------------------------------

glProgramUniformMatrix3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*9@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix3dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3dvEXT #-}
ptr_glProgramUniformMatrix3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3dvEXT"

-- glProgramUniformMatrix3fv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*9@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix3fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3fv #-}
ptr_glProgramUniformMatrix3fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3fv = unsafePerformIO $ getCommand "glProgramUniformMatrix3fv"

-- glProgramUniformMatrix3fvEXT ------------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix3fv'.
glProgramUniformMatrix3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*9@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix3fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3fvEXT #-}
ptr_glProgramUniformMatrix3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3fvEXT"

-- glProgramUniformMatrix3x2dv -------------------------------------------------

glProgramUniformMatrix3x2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x2dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix3x2dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2dv #-}
ptr_glProgramUniformMatrix3x2dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x2dv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2dv"

-- glProgramUniformMatrix3x2dvEXT ----------------------------------------------

glProgramUniformMatrix3x2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x2dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix3x2dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2dvEXT #-}
ptr_glProgramUniformMatrix3x2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x2dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2dvEXT"

-- glProgramUniformMatrix3x2fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix3x2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x2fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix3x2fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2fv #-}
ptr_glProgramUniformMatrix3x2fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x2fv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2fv"

-- glProgramUniformMatrix3x2fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix3x2fv'.
glProgramUniformMatrix3x2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x2fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix3x2fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2fvEXT #-}
ptr_glProgramUniformMatrix3x2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x2fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2fvEXT"

-- glProgramUniformMatrix3x4dv -------------------------------------------------

glProgramUniformMatrix3x4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x4dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix3x4dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4dv #-}
ptr_glProgramUniformMatrix3x4dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x4dv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4dv"

-- glProgramUniformMatrix3x4dvEXT ----------------------------------------------

glProgramUniformMatrix3x4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x4dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix3x4dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4dvEXT #-}
ptr_glProgramUniformMatrix3x4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x4dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4dvEXT"

-- glProgramUniformMatrix3x4fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix3x4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x4fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix3x4fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4fv #-}
ptr_glProgramUniformMatrix3x4fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x4fv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4fv"

-- glProgramUniformMatrix3x4fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix3x4fv'.
glProgramUniformMatrix3x4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix3x4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4fvEXT #-}
ptr_glProgramUniformMatrix3x4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x4fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4fvEXT"

-- glProgramUniformMatrix4dv ---------------------------------------------------

glProgramUniformMatrix4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*16@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix4dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4dv #-}
ptr_glProgramUniformMatrix4dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4dv = unsafePerformIO $ getCommand "glProgramUniformMatrix4dv"

-- glProgramUniformMatrix4dvEXT ------------------------------------------------

glProgramUniformMatrix4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*16@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix4dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4dvEXT #-}
ptr_glProgramUniformMatrix4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4dvEXT"

-- glProgramUniformMatrix4fv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*16@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix4fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4fv #-}
ptr_glProgramUniformMatrix4fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4fv = unsafePerformIO $ getCommand "glProgramUniformMatrix4fv"

-- glProgramUniformMatrix4fvEXT ------------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix4fv'.
glProgramUniformMatrix4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*16@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4fvEXT #-}
ptr_glProgramUniformMatrix4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4fvEXT"

-- glProgramUniformMatrix4x2dv -------------------------------------------------

glProgramUniformMatrix4x2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x2dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix4x2dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2dv #-}
ptr_glProgramUniformMatrix4x2dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x2dv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2dv"

-- glProgramUniformMatrix4x2dvEXT ----------------------------------------------

glProgramUniformMatrix4x2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x2dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix4x2dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2dvEXT #-}
ptr_glProgramUniformMatrix4x2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x2dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2dvEXT"

-- glProgramUniformMatrix4x2fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix4x2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x2fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix4x2fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2fv #-}
ptr_glProgramUniformMatrix4x2fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x2fv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2fv"

-- glProgramUniformMatrix4x2fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix4x2fv'.
glProgramUniformMatrix4x2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x2fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix4x2fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2fvEXT #-}
ptr_glProgramUniformMatrix4x2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x2fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2fvEXT"

-- glProgramUniformMatrix4x3dv -------------------------------------------------

glProgramUniformMatrix4x3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x3dv v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix4x3dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3dv #-}
ptr_glProgramUniformMatrix4x3dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x3dv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3dv"

-- glProgramUniformMatrix4x3dvEXT ----------------------------------------------

glProgramUniformMatrix4x3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x3dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn704 ptr_glProgramUniformMatrix4x3dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3dvEXT #-}
ptr_glProgramUniformMatrix4x3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x3dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3dvEXT"

-- glProgramUniformMatrix4x3fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix4x3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x3fv v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix4x3fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3fv #-}
ptr_glProgramUniformMatrix4x3fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x3fv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3fv"

-- glProgramUniformMatrix4x3fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix4x3fv'.
glProgramUniformMatrix4x3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x3fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glProgramUniformMatrix4x3fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3fvEXT #-}
ptr_glProgramUniformMatrix4x3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x3fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3fvEXT"

-- glProgramUniformui64NV ------------------------------------------------------

glProgramUniformui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @value@.
  -> m ()
glProgramUniformui64NV v1 v2 v3 = liftIO $ dyn678 ptr_glProgramUniformui64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniformui64NV #-}
ptr_glProgramUniformui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> IO ())
ptr_glProgramUniformui64NV = unsafePerformIO $ getCommand "glProgramUniformui64NV"

-- glProgramUniformui64vNV -----------------------------------------------------

glProgramUniformui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniformui64vNV v1 v2 v3 v4 = liftIO $ dyn679 ptr_glProgramUniformui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniformui64vNV #-}
ptr_glProgramUniformui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniformui64vNV = unsafePerformIO $ getCommand "glProgramUniformui64vNV"

-- glProgramVertexLimitNV ------------------------------------------------------

glProgramVertexLimitNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLint -- ^ @limit@.
  -> m ()
glProgramVertexLimitNV v1 v2 = liftIO $ dyn58 ptr_glProgramVertexLimitNV v1 v2

{-# NOINLINE ptr_glProgramVertexLimitNV #-}
ptr_glProgramVertexLimitNV :: FunPtr (GLenum -> GLint -> IO ())
ptr_glProgramVertexLimitNV = unsafePerformIO $ getCommand "glProgramVertexLimitNV"

-- glProvokingVertex -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glProvokingVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glProvokingVertex.xhtml OpenGL 4.x>.
glProvokingVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [VertexProvokingMode](Graphics-GL-Groups.html#VertexProvokingMode).
  -> m ()
glProvokingVertex v1 = liftIO $ dyn5 ptr_glProvokingVertex v1

{-# NOINLINE ptr_glProvokingVertex #-}
ptr_glProvokingVertex :: FunPtr (GLenum -> IO ())
ptr_glProvokingVertex = unsafePerformIO $ getCommand "glProvokingVertex"

-- glProvokingVertexEXT --------------------------------------------------------

-- | This command is an alias for 'glProvokingVertex'.
glProvokingVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [VertexProvokingMode](Graphics-GL-Groups.html#VertexProvokingMode).
  -> m ()
glProvokingVertexEXT v1 = liftIO $ dyn5 ptr_glProvokingVertexEXT v1

{-# NOINLINE ptr_glProvokingVertexEXT #-}
ptr_glProvokingVertexEXT :: FunPtr (GLenum -> IO ())
ptr_glProvokingVertexEXT = unsafePerformIO $ getCommand "glProvokingVertexEXT"

-- glPushAttrib ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushAttrib.xml OpenGL 2.x>.
glPushAttrib
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [AttribMask](Graphics-GL-Groups.html#AttribMask).
  -> m ()
glPushAttrib v1 = liftIO $ dyn75 ptr_glPushAttrib v1

{-# NOINLINE ptr_glPushAttrib #-}
ptr_glPushAttrib :: FunPtr (GLbitfield -> IO ())
ptr_glPushAttrib = unsafePerformIO $ getCommand "glPushAttrib"

-- glPushClientAttrib ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushClientAttrib.xml OpenGL 2.x>.
glPushClientAttrib
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [ClientAttribMask](Graphics-GL-Groups.html#ClientAttribMask).
  -> m ()
glPushClientAttrib v1 = liftIO $ dyn75 ptr_glPushClientAttrib v1

{-# NOINLINE ptr_glPushClientAttrib #-}
ptr_glPushClientAttrib :: FunPtr (GLbitfield -> IO ())
ptr_glPushClientAttrib = unsafePerformIO $ getCommand "glPushClientAttrib"

-- glPushClientAttribDefaultEXT ------------------------------------------------

glPushClientAttribDefaultEXT
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [ClientAttribMask](Graphics-GL-Groups.html#ClientAttribMask).
  -> m ()
glPushClientAttribDefaultEXT v1 = liftIO $ dyn75 ptr_glPushClientAttribDefaultEXT v1

{-# NOINLINE ptr_glPushClientAttribDefaultEXT #-}
ptr_glPushClientAttribDefaultEXT :: FunPtr (GLbitfield -> IO ())
ptr_glPushClientAttribDefaultEXT = unsafePerformIO $ getCommand "glPushClientAttribDefaultEXT"

-- glPushDebugGroup ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPushDebugGroup.xhtml OpenGL 4.x>.
glPushDebugGroup
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @message@ pointing to @COMPSIZE(message,length)@ elements of type @GLchar@.
  -> m ()
glPushDebugGroup v1 v2 v3 v4 = liftIO $ dyn511 ptr_glPushDebugGroup v1 v2 v3 v4

{-# NOINLINE ptr_glPushDebugGroup #-}
ptr_glPushDebugGroup :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glPushDebugGroup = unsafePerformIO $ getCommand "glPushDebugGroup"

-- glPushDebugGroupKHR ---------------------------------------------------------

-- | This command is an alias for 'glPushDebugGroup'.
glPushDebugGroupKHR
  :: MonadIO m
  => GLenum -- ^ @source@ of type [DebugSource](Graphics-GL-Groups.html#DebugSource).
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @message@.
  -> m ()
glPushDebugGroupKHR v1 v2 v3 v4 = liftIO $ dyn511 ptr_glPushDebugGroupKHR v1 v2 v3 v4

{-# NOINLINE ptr_glPushDebugGroupKHR #-}
ptr_glPushDebugGroupKHR :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glPushDebugGroupKHR = unsafePerformIO $ getCommand "glPushDebugGroupKHR"

-- glPushGroupMarkerEXT --------------------------------------------------------

glPushGroupMarkerEXT
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @marker@.
  -> m ()
glPushGroupMarkerEXT v1 v2 = liftIO $ dyn495 ptr_glPushGroupMarkerEXT v1 v2

{-# NOINLINE ptr_glPushGroupMarkerEXT #-}
ptr_glPushGroupMarkerEXT :: FunPtr (GLsizei -> Ptr GLchar -> IO ())
ptr_glPushGroupMarkerEXT = unsafePerformIO $ getCommand "glPushGroupMarkerEXT"

-- glPushMatrix ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushMatrix.xml OpenGL 2.x>.
glPushMatrix
  :: MonadIO m
  => m ()
glPushMatrix = liftIO $ dyn11 ptr_glPushMatrix

{-# NOINLINE ptr_glPushMatrix #-}
ptr_glPushMatrix :: FunPtr (IO ())
ptr_glPushMatrix = unsafePerformIO $ getCommand "glPushMatrix"

-- glPushName ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushName.xml OpenGL 2.x>.
glPushName
  :: MonadIO m
  => GLuint -- ^ @name@ of type @SelectName@.
  -> m ()
glPushName v1 = liftIO $ dyn3 ptr_glPushName v1

{-# NOINLINE ptr_glPushName #-}
ptr_glPushName :: FunPtr (GLuint -> IO ())
ptr_glPushName = unsafePerformIO $ getCommand "glPushName"

-- glQueryCounter --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glQueryCounter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glQueryCounter.xhtml OpenGL 4.x>.
glQueryCounter
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @target@ of type [QueryCounterTarget](Graphics-GL-Groups.html#QueryCounterTarget).
  -> m ()
glQueryCounter v1 v2 = liftIO $ dyn18 ptr_glQueryCounter v1 v2

{-# NOINLINE ptr_glQueryCounter #-}
ptr_glQueryCounter :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glQueryCounter = unsafePerformIO $ getCommand "glQueryCounter"

-- glQueryCounterEXT -----------------------------------------------------------

-- | This command is an alias for 'glQueryCounter'.
glQueryCounterEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @target@ of type [QueryCounterTarget](Graphics-GL-Groups.html#QueryCounterTarget).
  -> m ()
glQueryCounterEXT v1 v2 = liftIO $ dyn18 ptr_glQueryCounterEXT v1 v2

{-# NOINLINE ptr_glQueryCounterEXT #-}
ptr_glQueryCounterEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glQueryCounterEXT = unsafePerformIO $ getCommand "glQueryCounterEXT"

-- glQueryMatrixxOES -----------------------------------------------------------

glQueryMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @mantissa@ pointing to @16@ elements of type @GLfixed@.
  -> Ptr GLint -- ^ @exponent@ pointing to @16@ elements of type @GLint@.
  -> m GLbitfield
glQueryMatrixxOES v1 v2 = liftIO $ dyn706 ptr_glQueryMatrixxOES v1 v2

{-# NOINLINE ptr_glQueryMatrixxOES #-}
ptr_glQueryMatrixxOES :: FunPtr (Ptr GLfixed -> Ptr GLint -> IO GLbitfield)
ptr_glQueryMatrixxOES = unsafePerformIO $ getCommand "glQueryMatrixxOES"

-- glQueryObjectParameteruiAMD -------------------------------------------------

glQueryObjectParameteruiAMD
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> GLuint -- ^ @param@ of type [OcclusionQueryEventMaskAMD](Graphics-GL-Groups.html#OcclusionQueryEventMaskAMD).
  -> m ()
glQueryObjectParameteruiAMD v1 v2 v3 v4 = liftIO $ dyn707 ptr_glQueryObjectParameteruiAMD v1 v2 v3 v4

{-# NOINLINE ptr_glQueryObjectParameteruiAMD #-}
ptr_glQueryObjectParameteruiAMD :: FunPtr (GLenum -> GLuint -> GLenum -> GLuint -> IO ())
ptr_glQueryObjectParameteruiAMD = unsafePerformIO $ getCommand "glQueryObjectParameteruiAMD"

-- glQueryResourceNV -----------------------------------------------------------

glQueryResourceNV
  :: MonadIO m
  => GLenum -- ^ @queryType@.
  -> GLint -- ^ @tagId@.
  -> GLuint -- ^ @count@.
  -> Ptr GLint -- ^ @buffer@ pointing to @count@ elements of type @GLint@.
  -> m GLint
glQueryResourceNV v1 v2 v3 v4 = liftIO $ dyn708 ptr_glQueryResourceNV v1 v2 v3 v4

{-# NOINLINE ptr_glQueryResourceNV #-}
ptr_glQueryResourceNV :: FunPtr (GLenum -> GLint -> GLuint -> Ptr GLint -> IO GLint)
ptr_glQueryResourceNV = unsafePerformIO $ getCommand "glQueryResourceNV"

-- glQueryResourceTagNV --------------------------------------------------------

glQueryResourceTagNV
  :: MonadIO m
  => GLint -- ^ @tagId@.
  -> Ptr GLchar -- ^ @tagString@.
  -> m ()
glQueryResourceTagNV v1 v2 = liftIO $ dyn220 ptr_glQueryResourceTagNV v1 v2

{-# NOINLINE ptr_glQueryResourceTagNV #-}
ptr_glQueryResourceTagNV :: FunPtr (GLint -> Ptr GLchar -> IO ())
ptr_glQueryResourceTagNV = unsafePerformIO $ getCommand "glQueryResourceTagNV"

-- glRasterPos2d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2dv'.
glRasterPos2d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glRasterPos2d v1 v2 = liftIO $ dyn225 ptr_glRasterPos2d v1 v2

{-# NOINLINE ptr_glRasterPos2d #-}
ptr_glRasterPos2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glRasterPos2d = unsafePerformIO $ getCommand "glRasterPos2d"

-- glRasterPos2dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glRasterPos2dv v1 = liftIO $ dyn42 ptr_glRasterPos2dv v1

{-# NOINLINE ptr_glRasterPos2dv #-}
ptr_glRasterPos2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glRasterPos2dv = unsafePerformIO $ getCommand "glRasterPos2dv"

-- glRasterPos2f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2fv'.
glRasterPos2f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glRasterPos2f v1 v2 = liftIO $ dyn230 ptr_glRasterPos2f v1 v2

{-# NOINLINE ptr_glRasterPos2f #-}
ptr_glRasterPos2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glRasterPos2f = unsafePerformIO $ getCommand "glRasterPos2f"

-- glRasterPos2fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glRasterPos2fv v1 = liftIO $ dyn44 ptr_glRasterPos2fv v1

{-# NOINLINE ptr_glRasterPos2fv #-}
ptr_glRasterPos2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glRasterPos2fv = unsafePerformIO $ getCommand "glRasterPos2fv"

-- glRasterPos2i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2iv'.
glRasterPos2i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glRasterPos2i v1 v2 = liftIO $ dyn277 ptr_glRasterPos2i v1 v2

{-# NOINLINE ptr_glRasterPos2i #-}
ptr_glRasterPos2i :: FunPtr (GLint -> GLint -> IO ())
ptr_glRasterPos2i = unsafePerformIO $ getCommand "glRasterPos2i"

-- glRasterPos2iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glRasterPos2iv v1 = liftIO $ dyn46 ptr_glRasterPos2iv v1

{-# NOINLINE ptr_glRasterPos2iv #-}
ptr_glRasterPos2iv :: FunPtr (Ptr GLint -> IO ())
ptr_glRasterPos2iv = unsafePerformIO $ getCommand "glRasterPos2iv"

-- glRasterPos2s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2sv'.
glRasterPos2s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glRasterPos2s v1 v2 = liftIO $ dyn709 ptr_glRasterPos2s v1 v2

{-# NOINLINE ptr_glRasterPos2s #-}
ptr_glRasterPos2s :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glRasterPos2s = unsafePerformIO $ getCommand "glRasterPos2s"

-- glRasterPos2sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glRasterPos2sv v1 = liftIO $ dyn48 ptr_glRasterPos2sv v1

{-# NOINLINE ptr_glRasterPos2sv #-}
ptr_glRasterPos2sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glRasterPos2sv = unsafePerformIO $ getCommand "glRasterPos2sv"

-- glRasterPos2xOES ------------------------------------------------------------

glRasterPos2xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> m ()
glRasterPos2xOES v1 v2 = liftIO $ dyn232 ptr_glRasterPos2xOES v1 v2

{-# NOINLINE ptr_glRasterPos2xOES #-}
ptr_glRasterPos2xOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glRasterPos2xOES = unsafePerformIO $ getCommand "glRasterPos2xOES"

-- glRasterPos2xvOES -----------------------------------------------------------

glRasterPos2xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glRasterPos2xvOES v1 = liftIO $ dyn114 ptr_glRasterPos2xvOES v1

{-# NOINLINE ptr_glRasterPos2xvOES #-}
ptr_glRasterPos2xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glRasterPos2xvOES = unsafePerformIO $ getCommand "glRasterPos2xvOES"

-- glRasterPos3d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3dv'.
glRasterPos3d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glRasterPos3d v1 v2 v3 = liftIO $ dyn41 ptr_glRasterPos3d v1 v2 v3

{-# NOINLINE ptr_glRasterPos3d #-}
ptr_glRasterPos3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRasterPos3d = unsafePerformIO $ getCommand "glRasterPos3d"

-- glRasterPos3dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glRasterPos3dv v1 = liftIO $ dyn42 ptr_glRasterPos3dv v1

{-# NOINLINE ptr_glRasterPos3dv #-}
ptr_glRasterPos3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glRasterPos3dv = unsafePerformIO $ getCommand "glRasterPos3dv"

-- glRasterPos3f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3fv'.
glRasterPos3f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glRasterPos3f v1 v2 v3 = liftIO $ dyn43 ptr_glRasterPos3f v1 v2 v3

{-# NOINLINE ptr_glRasterPos3f #-}
ptr_glRasterPos3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRasterPos3f = unsafePerformIO $ getCommand "glRasterPos3f"

-- glRasterPos3fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glRasterPos3fv v1 = liftIO $ dyn44 ptr_glRasterPos3fv v1

{-# NOINLINE ptr_glRasterPos3fv #-}
ptr_glRasterPos3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glRasterPos3fv = unsafePerformIO $ getCommand "glRasterPos3fv"

-- glRasterPos3i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3iv'.
glRasterPos3i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glRasterPos3i v1 v2 v3 = liftIO $ dyn45 ptr_glRasterPos3i v1 v2 v3

{-# NOINLINE ptr_glRasterPos3i #-}
ptr_glRasterPos3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glRasterPos3i = unsafePerformIO $ getCommand "glRasterPos3i"

-- glRasterPos3iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glRasterPos3iv v1 = liftIO $ dyn46 ptr_glRasterPos3iv v1

{-# NOINLINE ptr_glRasterPos3iv #-}
ptr_glRasterPos3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glRasterPos3iv = unsafePerformIO $ getCommand "glRasterPos3iv"

-- glRasterPos3s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3sv'.
glRasterPos3s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glRasterPos3s v1 v2 v3 = liftIO $ dyn47 ptr_glRasterPos3s v1 v2 v3

{-# NOINLINE ptr_glRasterPos3s #-}
ptr_glRasterPos3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glRasterPos3s = unsafePerformIO $ getCommand "glRasterPos3s"

-- glRasterPos3sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glRasterPos3sv v1 = liftIO $ dyn48 ptr_glRasterPos3sv v1

{-# NOINLINE ptr_glRasterPos3sv #-}
ptr_glRasterPos3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glRasterPos3sv = unsafePerformIO $ getCommand "glRasterPos3sv"

-- glRasterPos3xOES ------------------------------------------------------------

glRasterPos3xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glRasterPos3xOES v1 v2 v3 = liftIO $ dyn113 ptr_glRasterPos3xOES v1 v2 v3

{-# NOINLINE ptr_glRasterPos3xOES #-}
ptr_glRasterPos3xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRasterPos3xOES = unsafePerformIO $ getCommand "glRasterPos3xOES"

-- glRasterPos3xvOES -----------------------------------------------------------

glRasterPos3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glRasterPos3xvOES v1 = liftIO $ dyn114 ptr_glRasterPos3xvOES v1

{-# NOINLINE ptr_glRasterPos3xvOES #-}
ptr_glRasterPos3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glRasterPos3xvOES = unsafePerformIO $ getCommand "glRasterPos3xvOES"

-- glRasterPos4d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4dv'.
glRasterPos4d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> GLdouble -- ^ @w@ of type @CoordD@.
  -> m ()
glRasterPos4d v1 v2 v3 v4 = liftIO $ dyn116 ptr_glRasterPos4d v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4d #-}
ptr_glRasterPos4d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRasterPos4d = unsafePerformIO $ getCommand "glRasterPos4d"

-- glRasterPos4dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glRasterPos4dv v1 = liftIO $ dyn42 ptr_glRasterPos4dv v1

{-# NOINLINE ptr_glRasterPos4dv #-}
ptr_glRasterPos4dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glRasterPos4dv = unsafePerformIO $ getCommand "glRasterPos4dv"

-- glRasterPos4f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4fv'.
glRasterPos4f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> GLfloat -- ^ @w@ of type @CoordF@.
  -> m ()
glRasterPos4f v1 v2 v3 v4 = liftIO $ dyn52 ptr_glRasterPos4f v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4f #-}
ptr_glRasterPos4f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRasterPos4f = unsafePerformIO $ getCommand "glRasterPos4f"

-- glRasterPos4fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glRasterPos4fv v1 = liftIO $ dyn44 ptr_glRasterPos4fv v1

{-# NOINLINE ptr_glRasterPos4fv #-}
ptr_glRasterPos4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glRasterPos4fv = unsafePerformIO $ getCommand "glRasterPos4fv"

-- glRasterPos4i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4iv'.
glRasterPos4i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> GLint -- ^ @w@ of type @CoordI@.
  -> m ()
glRasterPos4i v1 v2 v3 v4 = liftIO $ dyn82 ptr_glRasterPos4i v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4i #-}
ptr_glRasterPos4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glRasterPos4i = unsafePerformIO $ getCommand "glRasterPos4i"

-- glRasterPos4iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glRasterPos4iv v1 = liftIO $ dyn46 ptr_glRasterPos4iv v1

{-# NOINLINE ptr_glRasterPos4iv #-}
ptr_glRasterPos4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glRasterPos4iv = unsafePerformIO $ getCommand "glRasterPos4iv"

-- glRasterPos4s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4sv'.
glRasterPos4s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> GLshort -- ^ @w@ of type @CoordS@.
  -> m ()
glRasterPos4s v1 v2 v3 v4 = liftIO $ dyn120 ptr_glRasterPos4s v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4s #-}
ptr_glRasterPos4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glRasterPos4s = unsafePerformIO $ getCommand "glRasterPos4s"

-- glRasterPos4sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glRasterPos4sv v1 = liftIO $ dyn48 ptr_glRasterPos4sv v1

{-# NOINLINE ptr_glRasterPos4sv #-}
ptr_glRasterPos4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glRasterPos4sv = unsafePerformIO $ getCommand "glRasterPos4sv"

-- glRasterPos4xOES ------------------------------------------------------------

glRasterPos4xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> GLfixed -- ^ @w@.
  -> m ()
glRasterPos4xOES v1 v2 v3 v4 = liftIO $ dyn53 ptr_glRasterPos4xOES v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4xOES #-}
ptr_glRasterPos4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRasterPos4xOES = unsafePerformIO $ getCommand "glRasterPos4xOES"

