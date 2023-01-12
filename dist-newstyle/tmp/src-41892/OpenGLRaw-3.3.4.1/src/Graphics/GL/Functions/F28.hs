{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F28
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

module Graphics.GL.Functions.F28 (
  glUniform2iv,
  glUniform2ivARB,
  glUniform2ui,
  glUniform2ui64ARB,
  glUniform2ui64NV,
  glUniform2ui64vARB,
  glUniform2ui64vNV,
  glUniform2uiEXT,
  glUniform2uiv,
  glUniform2uivEXT,
  glUniform3d,
  glUniform3dv,
  glUniform3f,
  glUniform3fARB,
  glUniform3fv,
  glUniform3fvARB,
  glUniform3i,
  glUniform3i64ARB,
  glUniform3i64NV,
  glUniform3i64vARB,
  glUniform3i64vNV,
  glUniform3iARB,
  glUniform3iv,
  glUniform3ivARB,
  glUniform3ui,
  glUniform3ui64ARB,
  glUniform3ui64NV,
  glUniform3ui64vARB,
  glUniform3ui64vNV,
  glUniform3uiEXT,
  glUniform3uiv,
  glUniform3uivEXT,
  glUniform4d,
  glUniform4dv,
  glUniform4f,
  glUniform4fARB,
  glUniform4fv,
  glUniform4fvARB,
  glUniform4i,
  glUniform4i64ARB,
  glUniform4i64NV,
  glUniform4i64vARB,
  glUniform4i64vNV,
  glUniform4iARB,
  glUniform4iv,
  glUniform4ivARB,
  glUniform4ui,
  glUniform4ui64ARB,
  glUniform4ui64NV,
  glUniform4ui64vARB,
  glUniform4ui64vNV,
  glUniform4uiEXT,
  glUniform4uiv,
  glUniform4uivEXT,
  glUniformBlockBinding,
  glUniformBufferEXT,
  glUniformHandleui64ARB,
  glUniformHandleui64IMG,
  glUniformHandleui64NV,
  glUniformHandleui64vARB,
  glUniformHandleui64vIMG,
  glUniformHandleui64vNV,
  glUniformMatrix2dv,
  glUniformMatrix2fv,
  glUniformMatrix2fvARB,
  glUniformMatrix2x3dv,
  glUniformMatrix2x3fv,
  glUniformMatrix2x3fvNV,
  glUniformMatrix2x4dv,
  glUniformMatrix2x4fv,
  glUniformMatrix2x4fvNV,
  glUniformMatrix3dv,
  glUniformMatrix3fv,
  glUniformMatrix3fvARB,
  glUniformMatrix3x2dv,
  glUniformMatrix3x2fv,
  glUniformMatrix3x2fvNV,
  glUniformMatrix3x4dv,
  glUniformMatrix3x4fv,
  glUniformMatrix3x4fvNV,
  glUniformMatrix4dv,
  glUniformMatrix4fv,
  glUniformMatrix4fvARB,
  glUniformMatrix4x2dv,
  glUniformMatrix4x2fv,
  glUniformMatrix4x2fvNV,
  glUniformMatrix4x3dv,
  glUniformMatrix4x3fv,
  glUniformMatrix4x3fvNV,
  glUniformSubroutinesuiv,
  glUniformui64NV,
  glUniformui64vNV,
  glUnlockArraysEXT,
  glUnmapBuffer,
  glUnmapBufferARB,
  glUnmapBufferOES,
  glUnmapNamedBuffer,
  glUnmapNamedBufferEXT,
  glUnmapObjectBufferATI,
  glUnmapTexture2DINTEL
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glUniform2iv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2iv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*2@ elements of type @GLint@.
  -> m ()
glUniform2iv v1 v2 v3 = liftIO $ dyn841 ptr_glUniform2iv v1 v2 v3

{-# NOINLINE ptr_glUniform2iv #-}
ptr_glUniform2iv :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform2iv = unsafePerformIO $ getCommand "glUniform2iv"

-- glUniform2ivARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform2iv'.
glUniform2ivARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*2@ elements of type @GLint@.
  -> m ()
glUniform2ivARB v1 v2 v3 = liftIO $ dyn841 ptr_glUniform2ivARB v1 v2 v3

{-# NOINLINE ptr_glUniform2ivARB #-}
ptr_glUniform2ivARB :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform2ivARB = unsafePerformIO $ getCommand "glUniform2ivARB"

-- glUniform2ui ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2ui
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glUniform2ui v1 v2 v3 = liftIO $ dyn850 ptr_glUniform2ui v1 v2 v3

{-# NOINLINE ptr_glUniform2ui #-}
ptr_glUniform2ui :: FunPtr (GLint -> GLuint -> GLuint -> IO ())
ptr_glUniform2ui = unsafePerformIO $ getCommand "glUniform2ui"

-- glUniform2ui64ARB -----------------------------------------------------------

glUniform2ui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> m ()
glUniform2ui64ARB v1 v2 v3 = liftIO $ dyn851 ptr_glUniform2ui64ARB v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64ARB #-}
ptr_glUniform2ui64ARB :: FunPtr (GLint -> GLuint64 -> GLuint64 -> IO ())
ptr_glUniform2ui64ARB = unsafePerformIO $ getCommand "glUniform2ui64ARB"

-- glUniform2ui64NV ------------------------------------------------------------

glUniform2ui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> m ()
glUniform2ui64NV v1 v2 v3 = liftIO $ dyn852 ptr_glUniform2ui64NV v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64NV #-}
ptr_glUniform2ui64NV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glUniform2ui64NV = unsafePerformIO $ getCommand "glUniform2ui64NV"

-- glUniform2ui64vARB ----------------------------------------------------------

glUniform2ui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*2@ elements of type @GLuint64@.
  -> m ()
glUniform2ui64vARB v1 v2 v3 = liftIO $ dyn845 ptr_glUniform2ui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64vARB #-}
ptr_glUniform2ui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniform2ui64vARB = unsafePerformIO $ getCommand "glUniform2ui64vARB"

-- glUniform2ui64vNV -----------------------------------------------------------

glUniform2ui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*2@ elements of type @GLuint64EXT@.
  -> m ()
glUniform2ui64vNV v1 v2 v3 = liftIO $ dyn846 ptr_glUniform2ui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64vNV #-}
ptr_glUniform2ui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniform2ui64vNV = unsafePerformIO $ getCommand "glUniform2ui64vNV"

-- glUniform2uiEXT -------------------------------------------------------------

-- | This command is an alias for 'glUniform2ui'.
glUniform2uiEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glUniform2uiEXT v1 v2 v3 = liftIO $ dyn850 ptr_glUniform2uiEXT v1 v2 v3

{-# NOINLINE ptr_glUniform2uiEXT #-}
ptr_glUniform2uiEXT :: FunPtr (GLint -> GLuint -> GLuint -> IO ())
ptr_glUniform2uiEXT = unsafePerformIO $ getCommand "glUniform2uiEXT"

-- glUniform2uiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2uiv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*2@ elements of type @GLuint@.
  -> m ()
glUniform2uiv v1 v2 v3 = liftIO $ dyn847 ptr_glUniform2uiv v1 v2 v3

{-# NOINLINE ptr_glUniform2uiv #-}
ptr_glUniform2uiv :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform2uiv = unsafePerformIO $ getCommand "glUniform2uiv"

-- glUniform2uivEXT ------------------------------------------------------------

-- | This command is an alias for 'glUniform2uiv'.
glUniform2uivEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*2@ elements of type @GLuint@.
  -> m ()
glUniform2uivEXT v1 v2 v3 = liftIO $ dyn847 ptr_glUniform2uivEXT v1 v2 v3

{-# NOINLINE ptr_glUniform2uivEXT #-}
ptr_glUniform2uivEXT :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform2uivEXT = unsafePerformIO $ getCommand "glUniform2uivEXT"

-- glUniform3d -----------------------------------------------------------------

glUniform3d
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glUniform3d v1 v2 v3 v4 = liftIO $ dyn853 ptr_glUniform3d v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3d #-}
ptr_glUniform3d :: FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glUniform3d = unsafePerformIO $ getCommand "glUniform3d"

-- glUniform3dv ----------------------------------------------------------------

glUniform3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*3@ elements of type @GLdouble@.
  -> m ()
glUniform3dv v1 v2 v3 = liftIO $ dyn834 ptr_glUniform3dv v1 v2 v3

{-# NOINLINE ptr_glUniform3dv #-}
ptr_glUniform3dv :: FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glUniform3dv = unsafePerformIO $ getCommand "glUniform3dv"

-- glUniform3f -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3f
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glUniform3f v1 v2 v3 v4 = liftIO $ dyn854 ptr_glUniform3f v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3f #-}
ptr_glUniform3f :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform3f = unsafePerformIO $ getCommand "glUniform3f"

-- glUniform3fARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform3f'.
glUniform3fARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glUniform3fARB v1 v2 v3 v4 = liftIO $ dyn854 ptr_glUniform3fARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3fARB #-}
ptr_glUniform3fARB :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform3fARB = unsafePerformIO $ getCommand "glUniform3fARB"

-- glUniform3fv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glUniform3fv v1 v2 v3 = liftIO $ dyn836 ptr_glUniform3fv v1 v2 v3

{-# NOINLINE ptr_glUniform3fv #-}
ptr_glUniform3fv :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform3fv = unsafePerformIO $ getCommand "glUniform3fv"

-- glUniform3fvARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform3fv'.
glUniform3fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glUniform3fvARB v1 v2 v3 = liftIO $ dyn836 ptr_glUniform3fvARB v1 v2 v3

{-# NOINLINE ptr_glUniform3fvARB #-}
ptr_glUniform3fvARB :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform3fvARB = unsafePerformIO $ getCommand "glUniform3fvARB"

-- glUniform3i -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3i
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glUniform3i v1 v2 v3 v4 = liftIO $ dyn82 ptr_glUniform3i v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3i #-}
ptr_glUniform3i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform3i = unsafePerformIO $ getCommand "glUniform3i"

-- glUniform3i64ARB ------------------------------------------------------------

glUniform3i64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> m ()
glUniform3i64ARB v1 v2 v3 v4 = liftIO $ dyn855 ptr_glUniform3i64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3i64ARB #-}
ptr_glUniform3i64ARB :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glUniform3i64ARB = unsafePerformIO $ getCommand "glUniform3i64ARB"

-- glUniform3i64NV -------------------------------------------------------------

glUniform3i64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> m ()
glUniform3i64NV v1 v2 v3 v4 = liftIO $ dyn856 ptr_glUniform3i64NV v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3i64NV #-}
ptr_glUniform3i64NV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glUniform3i64NV = unsafePerformIO $ getCommand "glUniform3i64NV"

-- glUniform3i64vARB -----------------------------------------------------------

glUniform3i64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*3@ elements of type @GLint64@.
  -> m ()
glUniform3i64vARB v1 v2 v3 = liftIO $ dyn839 ptr_glUniform3i64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform3i64vARB #-}
ptr_glUniform3i64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glUniform3i64vARB = unsafePerformIO $ getCommand "glUniform3i64vARB"

-- glUniform3i64vNV ------------------------------------------------------------

glUniform3i64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLint64EXT@.
  -> m ()
glUniform3i64vNV v1 v2 v3 = liftIO $ dyn840 ptr_glUniform3i64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform3i64vNV #-}
ptr_glUniform3i64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glUniform3i64vNV = unsafePerformIO $ getCommand "glUniform3i64vNV"

-- glUniform3iARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform3i'.
glUniform3iARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glUniform3iARB v1 v2 v3 v4 = liftIO $ dyn82 ptr_glUniform3iARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3iARB #-}
ptr_glUniform3iARB :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform3iARB = unsafePerformIO $ getCommand "glUniform3iARB"

-- glUniform3iv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3iv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*3@ elements of type @GLint@.
  -> m ()
glUniform3iv v1 v2 v3 = liftIO $ dyn841 ptr_glUniform3iv v1 v2 v3

{-# NOINLINE ptr_glUniform3iv #-}
ptr_glUniform3iv :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform3iv = unsafePerformIO $ getCommand "glUniform3iv"

-- glUniform3ivARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform3iv'.
glUniform3ivARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*3@ elements of type @GLint@.
  -> m ()
glUniform3ivARB v1 v2 v3 = liftIO $ dyn841 ptr_glUniform3ivARB v1 v2 v3

{-# NOINLINE ptr_glUniform3ivARB #-}
ptr_glUniform3ivARB :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform3ivARB = unsafePerformIO $ getCommand "glUniform3ivARB"

-- glUniform3ui ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3ui
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glUniform3ui v1 v2 v3 v4 = liftIO $ dyn857 ptr_glUniform3ui v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3ui #-}
ptr_glUniform3ui :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform3ui = unsafePerformIO $ getCommand "glUniform3ui"

-- glUniform3ui64ARB -----------------------------------------------------------

glUniform3ui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> m ()
glUniform3ui64ARB v1 v2 v3 v4 = liftIO $ dyn858 ptr_glUniform3ui64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3ui64ARB #-}
ptr_glUniform3ui64ARB :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glUniform3ui64ARB = unsafePerformIO $ getCommand "glUniform3ui64ARB"

-- glUniform3ui64NV ------------------------------------------------------------

glUniform3ui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> m ()
glUniform3ui64NV v1 v2 v3 v4 = liftIO $ dyn859 ptr_glUniform3ui64NV v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3ui64NV #-}
ptr_glUniform3ui64NV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glUniform3ui64NV = unsafePerformIO $ getCommand "glUniform3ui64NV"

-- glUniform3ui64vARB ----------------------------------------------------------

glUniform3ui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*3@ elements of type @GLuint64@.
  -> m ()
glUniform3ui64vARB v1 v2 v3 = liftIO $ dyn845 ptr_glUniform3ui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform3ui64vARB #-}
ptr_glUniform3ui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniform3ui64vARB = unsafePerformIO $ getCommand "glUniform3ui64vARB"

-- glUniform3ui64vNV -----------------------------------------------------------

glUniform3ui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLuint64EXT@.
  -> m ()
glUniform3ui64vNV v1 v2 v3 = liftIO $ dyn846 ptr_glUniform3ui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform3ui64vNV #-}
ptr_glUniform3ui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniform3ui64vNV = unsafePerformIO $ getCommand "glUniform3ui64vNV"

-- glUniform3uiEXT -------------------------------------------------------------

-- | This command is an alias for 'glUniform3ui'.
glUniform3uiEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glUniform3uiEXT v1 v2 v3 v4 = liftIO $ dyn857 ptr_glUniform3uiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3uiEXT #-}
ptr_glUniform3uiEXT :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform3uiEXT = unsafePerformIO $ getCommand "glUniform3uiEXT"

-- glUniform3uiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3uiv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*3@ elements of type @GLuint@.
  -> m ()
glUniform3uiv v1 v2 v3 = liftIO $ dyn847 ptr_glUniform3uiv v1 v2 v3

{-# NOINLINE ptr_glUniform3uiv #-}
ptr_glUniform3uiv :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform3uiv = unsafePerformIO $ getCommand "glUniform3uiv"

-- glUniform3uivEXT ------------------------------------------------------------

-- | This command is an alias for 'glUniform3uiv'.
glUniform3uivEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*3@ elements of type @GLuint@.
  -> m ()
glUniform3uivEXT v1 v2 v3 = liftIO $ dyn847 ptr_glUniform3uivEXT v1 v2 v3

{-# NOINLINE ptr_glUniform3uivEXT #-}
ptr_glUniform3uivEXT :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform3uivEXT = unsafePerformIO $ getCommand "glUniform3uivEXT"

-- glUniform4d -----------------------------------------------------------------

glUniform4d
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glUniform4d v1 v2 v3 v4 v5 = liftIO $ dyn860 ptr_glUniform4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4d #-}
ptr_glUniform4d :: FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glUniform4d = unsafePerformIO $ getCommand "glUniform4d"

-- glUniform4dv ----------------------------------------------------------------

glUniform4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glUniform4dv v1 v2 v3 = liftIO $ dyn834 ptr_glUniform4dv v1 v2 v3

{-# NOINLINE ptr_glUniform4dv #-}
ptr_glUniform4dv :: FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glUniform4dv = unsafePerformIO $ getCommand "glUniform4dv"

-- glUniform4f -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4f
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glUniform4f v1 v2 v3 v4 v5 = liftIO $ dyn861 ptr_glUniform4f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4f #-}
ptr_glUniform4f :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform4f = unsafePerformIO $ getCommand "glUniform4f"

-- glUniform4fARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform4f'.
glUniform4fARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glUniform4fARB v1 v2 v3 v4 v5 = liftIO $ dyn861 ptr_glUniform4fARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4fARB #-}
ptr_glUniform4fARB :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform4fARB = unsafePerformIO $ getCommand "glUniform4fARB"

-- glUniform4fv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniform4fv v1 v2 v3 = liftIO $ dyn836 ptr_glUniform4fv v1 v2 v3

{-# NOINLINE ptr_glUniform4fv #-}
ptr_glUniform4fv :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform4fv = unsafePerformIO $ getCommand "glUniform4fv"

-- glUniform4fvARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform4fv'.
glUniform4fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniform4fvARB v1 v2 v3 = liftIO $ dyn836 ptr_glUniform4fvARB v1 v2 v3

{-# NOINLINE ptr_glUniform4fvARB #-}
ptr_glUniform4fvARB :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform4fvARB = unsafePerformIO $ getCommand "glUniform4fvARB"

-- glUniform4i -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4i
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glUniform4i v1 v2 v3 v4 v5 = liftIO $ dyn261 ptr_glUniform4i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4i #-}
ptr_glUniform4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform4i = unsafePerformIO $ getCommand "glUniform4i"

-- glUniform4i64ARB ------------------------------------------------------------

glUniform4i64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> GLint64 -- ^ @w@.
  -> m ()
glUniform4i64ARB v1 v2 v3 v4 v5 = liftIO $ dyn862 ptr_glUniform4i64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4i64ARB #-}
ptr_glUniform4i64ARB :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glUniform4i64ARB = unsafePerformIO $ getCommand "glUniform4i64ARB"

-- glUniform4i64NV -------------------------------------------------------------

glUniform4i64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> GLint64EXT -- ^ @w@.
  -> m ()
glUniform4i64NV v1 v2 v3 v4 v5 = liftIO $ dyn863 ptr_glUniform4i64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4i64NV #-}
ptr_glUniform4i64NV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glUniform4i64NV = unsafePerformIO $ getCommand "glUniform4i64NV"

-- glUniform4i64vARB -----------------------------------------------------------

glUniform4i64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*4@ elements of type @GLint64@.
  -> m ()
glUniform4i64vARB v1 v2 v3 = liftIO $ dyn839 ptr_glUniform4i64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform4i64vARB #-}
ptr_glUniform4i64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glUniform4i64vARB = unsafePerformIO $ getCommand "glUniform4i64vARB"

-- glUniform4i64vNV ------------------------------------------------------------

glUniform4i64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLint64EXT@.
  -> m ()
glUniform4i64vNV v1 v2 v3 = liftIO $ dyn840 ptr_glUniform4i64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform4i64vNV #-}
ptr_glUniform4i64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glUniform4i64vNV = unsafePerformIO $ getCommand "glUniform4i64vNV"

-- glUniform4iARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform4i'.
glUniform4iARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glUniform4iARB v1 v2 v3 v4 v5 = liftIO $ dyn261 ptr_glUniform4iARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4iARB #-}
ptr_glUniform4iARB :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform4iARB = unsafePerformIO $ getCommand "glUniform4iARB"

-- glUniform4iv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4iv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glUniform4iv v1 v2 v3 = liftIO $ dyn841 ptr_glUniform4iv v1 v2 v3

{-# NOINLINE ptr_glUniform4iv #-}
ptr_glUniform4iv :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform4iv = unsafePerformIO $ getCommand "glUniform4iv"

-- glUniform4ivARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform4iv'.
glUniform4ivARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glUniform4ivARB v1 v2 v3 = liftIO $ dyn841 ptr_glUniform4ivARB v1 v2 v3

{-# NOINLINE ptr_glUniform4ivARB #-}
ptr_glUniform4ivARB :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform4ivARB = unsafePerformIO $ getCommand "glUniform4ivARB"

-- glUniform4ui ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4ui
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glUniform4ui v1 v2 v3 v4 v5 = liftIO $ dyn864 ptr_glUniform4ui v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4ui #-}
ptr_glUniform4ui :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform4ui = unsafePerformIO $ getCommand "glUniform4ui"

-- glUniform4ui64ARB -----------------------------------------------------------

glUniform4ui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> GLuint64 -- ^ @w@.
  -> m ()
glUniform4ui64ARB v1 v2 v3 v4 v5 = liftIO $ dyn865 ptr_glUniform4ui64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4ui64ARB #-}
ptr_glUniform4ui64ARB :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glUniform4ui64ARB = unsafePerformIO $ getCommand "glUniform4ui64ARB"

-- glUniform4ui64NV ------------------------------------------------------------

glUniform4ui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> GLuint64EXT -- ^ @w@.
  -> m ()
glUniform4ui64NV v1 v2 v3 v4 v5 = liftIO $ dyn866 ptr_glUniform4ui64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4ui64NV #-}
ptr_glUniform4ui64NV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glUniform4ui64NV = unsafePerformIO $ getCommand "glUniform4ui64NV"

-- glUniform4ui64vARB ----------------------------------------------------------

glUniform4ui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*4@ elements of type @GLuint64@.
  -> m ()
glUniform4ui64vARB v1 v2 v3 = liftIO $ dyn845 ptr_glUniform4ui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform4ui64vARB #-}
ptr_glUniform4ui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniform4ui64vARB = unsafePerformIO $ getCommand "glUniform4ui64vARB"

-- glUniform4ui64vNV -----------------------------------------------------------

glUniform4ui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLuint64EXT@.
  -> m ()
glUniform4ui64vNV v1 v2 v3 = liftIO $ dyn846 ptr_glUniform4ui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform4ui64vNV #-}
ptr_glUniform4ui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniform4ui64vNV = unsafePerformIO $ getCommand "glUniform4ui64vNV"

-- glUniform4uiEXT -------------------------------------------------------------

-- | This command is an alias for 'glUniform4ui'.
glUniform4uiEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glUniform4uiEXT v1 v2 v3 v4 v5 = liftIO $ dyn864 ptr_glUniform4uiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4uiEXT #-}
ptr_glUniform4uiEXT :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform4uiEXT = unsafePerformIO $ getCommand "glUniform4uiEXT"

-- glUniform4uiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4uiv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glUniform4uiv v1 v2 v3 = liftIO $ dyn847 ptr_glUniform4uiv v1 v2 v3

{-# NOINLINE ptr_glUniform4uiv #-}
ptr_glUniform4uiv :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform4uiv = unsafePerformIO $ getCommand "glUniform4uiv"

-- glUniform4uivEXT ------------------------------------------------------------

-- | This command is an alias for 'glUniform4uiv'.
glUniform4uivEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glUniform4uivEXT v1 v2 v3 = liftIO $ dyn847 ptr_glUniform4uivEXT v1 v2 v3

{-# NOINLINE ptr_glUniform4uivEXT #-}
ptr_glUniform4uivEXT :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform4uivEXT = unsafePerformIO $ getCommand "glUniform4uivEXT"

-- glUniformBlockBinding -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniformBlockBinding.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniformBlockBinding.xhtml OpenGL 4.x>.
glUniformBlockBinding
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformBlockIndex@.
  -> GLuint -- ^ @uniformBlockBinding@.
  -> m ()
glUniformBlockBinding v1 v2 v3 = liftIO $ dyn109 ptr_glUniformBlockBinding v1 v2 v3

{-# NOINLINE ptr_glUniformBlockBinding #-}
ptr_glUniformBlockBinding :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniformBlockBinding = unsafePerformIO $ getCommand "glUniformBlockBinding"

-- glUniformBufferEXT ----------------------------------------------------------

glUniformBufferEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glUniformBufferEXT v1 v2 v3 = liftIO $ dyn676 ptr_glUniformBufferEXT v1 v2 v3

{-# NOINLINE ptr_glUniformBufferEXT #-}
ptr_glUniformBufferEXT :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
ptr_glUniformBufferEXT = unsafePerformIO $ getCommand "glUniformBufferEXT"

-- glUniformHandleui64ARB ------------------------------------------------------

glUniformHandleui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glUniformHandleui64ARB v1 v2 = liftIO $ dyn843 ptr_glUniformHandleui64ARB v1 v2

{-# NOINLINE ptr_glUniformHandleui64ARB #-}
ptr_glUniformHandleui64ARB :: FunPtr (GLint -> GLuint64 -> IO ())
ptr_glUniformHandleui64ARB = unsafePerformIO $ getCommand "glUniformHandleui64ARB"

-- glUniformHandleui64IMG ------------------------------------------------------

-- | This command is an alias for 'glUniformHandleui64ARB'.
glUniformHandleui64IMG
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glUniformHandleui64IMG v1 v2 = liftIO $ dyn843 ptr_glUniformHandleui64IMG v1 v2

{-# NOINLINE ptr_glUniformHandleui64IMG #-}
ptr_glUniformHandleui64IMG :: FunPtr (GLint -> GLuint64 -> IO ())
ptr_glUniformHandleui64IMG = unsafePerformIO $ getCommand "glUniformHandleui64IMG"

-- glUniformHandleui64NV -------------------------------------------------------

glUniformHandleui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glUniformHandleui64NV v1 v2 = liftIO $ dyn843 ptr_glUniformHandleui64NV v1 v2

{-# NOINLINE ptr_glUniformHandleui64NV #-}
ptr_glUniformHandleui64NV :: FunPtr (GLint -> GLuint64 -> IO ())
ptr_glUniformHandleui64NV = unsafePerformIO $ getCommand "glUniformHandleui64NV"

-- glUniformHandleui64vARB -----------------------------------------------------

glUniformHandleui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glUniformHandleui64vARB v1 v2 v3 = liftIO $ dyn845 ptr_glUniformHandleui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniformHandleui64vARB #-}
ptr_glUniformHandleui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniformHandleui64vARB = unsafePerformIO $ getCommand "glUniformHandleui64vARB"

-- glUniformHandleui64vIMG -----------------------------------------------------

-- | This command is an alias for 'glUniformHandleui64vARB'.
glUniformHandleui64vIMG
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glUniformHandleui64vIMG v1 v2 v3 = liftIO $ dyn845 ptr_glUniformHandleui64vIMG v1 v2 v3

{-# NOINLINE ptr_glUniformHandleui64vIMG #-}
ptr_glUniformHandleui64vIMG :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniformHandleui64vIMG = unsafePerformIO $ getCommand "glUniformHandleui64vIMG"

-- glUniformHandleui64vNV ------------------------------------------------------

glUniformHandleui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glUniformHandleui64vNV v1 v2 v3 = liftIO $ dyn845 ptr_glUniformHandleui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniformHandleui64vNV #-}
ptr_glUniformHandleui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniformHandleui64vNV = unsafePerformIO $ getCommand "glUniformHandleui64vNV"

-- glUniformMatrix2dv ----------------------------------------------------------

glUniformMatrix2dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix2dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix2dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2dv #-}
ptr_glUniformMatrix2dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix2dv = unsafePerformIO $ getCommand "glUniformMatrix2dv"

-- glUniformMatrix2fv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix2fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix2fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2fv #-}
ptr_glUniformMatrix2fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2fv = unsafePerformIO $ getCommand "glUniformMatrix2fv"

-- glUniformMatrix2fvARB -------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix2fv'.
glUniformMatrix2fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2fvARB v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix2fvARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2fvARB #-}
ptr_glUniformMatrix2fvARB :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2fvARB = unsafePerformIO $ getCommand "glUniformMatrix2fvARB"

-- glUniformMatrix2x3dv --------------------------------------------------------

glUniformMatrix2x3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix2x3dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix2x3dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x3dv #-}
ptr_glUniformMatrix2x3dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix2x3dv = unsafePerformIO $ getCommand "glUniformMatrix2x3dv"

-- glUniformMatrix2x3fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix2x3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x3fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix2x3fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x3fv #-}
ptr_glUniformMatrix2x3fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x3fv = unsafePerformIO $ getCommand "glUniformMatrix2x3fv"

-- glUniformMatrix2x3fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix2x3fv'.
glUniformMatrix2x3fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x3fvNV v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix2x3fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x3fvNV #-}
ptr_glUniformMatrix2x3fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x3fvNV = unsafePerformIO $ getCommand "glUniformMatrix2x3fvNV"

-- glUniformMatrix2x4dv --------------------------------------------------------

glUniformMatrix2x4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix2x4dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix2x4dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x4dv #-}
ptr_glUniformMatrix2x4dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix2x4dv = unsafePerformIO $ getCommand "glUniformMatrix2x4dv"

-- glUniformMatrix2x4fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix2x4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x4fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix2x4fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x4fv #-}
ptr_glUniformMatrix2x4fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x4fv = unsafePerformIO $ getCommand "glUniformMatrix2x4fv"

-- glUniformMatrix2x4fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix2x4fv'.
glUniformMatrix2x4fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x4fvNV v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix2x4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x4fvNV #-}
ptr_glUniformMatrix2x4fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x4fvNV = unsafePerformIO $ getCommand "glUniformMatrix2x4fvNV"

-- glUniformMatrix3dv ----------------------------------------------------------

glUniformMatrix3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*9@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix3dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix3dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3dv #-}
ptr_glUniformMatrix3dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix3dv = unsafePerformIO $ getCommand "glUniformMatrix3dv"

-- glUniformMatrix3fv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*9@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix3fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3fv #-}
ptr_glUniformMatrix3fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3fv = unsafePerformIO $ getCommand "glUniformMatrix3fv"

-- glUniformMatrix3fvARB -------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix3fv'.
glUniformMatrix3fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*9@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3fvARB v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix3fvARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3fvARB #-}
ptr_glUniformMatrix3fvARB :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3fvARB = unsafePerformIO $ getCommand "glUniformMatrix3fvARB"

-- glUniformMatrix3x2dv --------------------------------------------------------

glUniformMatrix3x2dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix3x2dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix3x2dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x2dv #-}
ptr_glUniformMatrix3x2dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix3x2dv = unsafePerformIO $ getCommand "glUniformMatrix3x2dv"

-- glUniformMatrix3x2fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix3x2fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x2fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix3x2fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x2fv #-}
ptr_glUniformMatrix3x2fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x2fv = unsafePerformIO $ getCommand "glUniformMatrix3x2fv"

-- glUniformMatrix3x2fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix3x2fv'.
glUniformMatrix3x2fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x2fvNV v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix3x2fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x2fvNV #-}
ptr_glUniformMatrix3x2fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x2fvNV = unsafePerformIO $ getCommand "glUniformMatrix3x2fvNV"

-- glUniformMatrix3x4dv --------------------------------------------------------

glUniformMatrix3x4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix3x4dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix3x4dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x4dv #-}
ptr_glUniformMatrix3x4dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix3x4dv = unsafePerformIO $ getCommand "glUniformMatrix3x4dv"

-- glUniformMatrix3x4fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix3x4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x4fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix3x4fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x4fv #-}
ptr_glUniformMatrix3x4fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x4fv = unsafePerformIO $ getCommand "glUniformMatrix3x4fv"

-- glUniformMatrix3x4fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix3x4fv'.
glUniformMatrix3x4fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x4fvNV v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix3x4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x4fvNV #-}
ptr_glUniformMatrix3x4fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x4fvNV = unsafePerformIO $ getCommand "glUniformMatrix3x4fvNV"

-- glUniformMatrix4dv ----------------------------------------------------------

glUniformMatrix4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*16@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix4dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix4dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4dv #-}
ptr_glUniformMatrix4dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix4dv = unsafePerformIO $ getCommand "glUniformMatrix4dv"

-- glUniformMatrix4fv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*16@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix4fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4fv #-}
ptr_glUniformMatrix4fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4fv = unsafePerformIO $ getCommand "glUniformMatrix4fv"

-- glUniformMatrix4fvARB -------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix4fv'.
glUniformMatrix4fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*16@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4fvARB v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix4fvARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4fvARB #-}
ptr_glUniformMatrix4fvARB :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4fvARB = unsafePerformIO $ getCommand "glUniformMatrix4fvARB"

-- glUniformMatrix4x2dv --------------------------------------------------------

glUniformMatrix4x2dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix4x2dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix4x2dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x2dv #-}
ptr_glUniformMatrix4x2dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix4x2dv = unsafePerformIO $ getCommand "glUniformMatrix4x2dv"

-- glUniformMatrix4x2fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix4x2fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x2fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix4x2fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x2fv #-}
ptr_glUniformMatrix4x2fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x2fv = unsafePerformIO $ getCommand "glUniformMatrix4x2fv"

-- glUniformMatrix4x2fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix4x2fv'.
glUniformMatrix4x2fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x2fvNV v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix4x2fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x2fvNV #-}
ptr_glUniformMatrix4x2fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x2fvNV = unsafePerformIO $ getCommand "glUniformMatrix4x2fvNV"

-- glUniformMatrix4x3dv --------------------------------------------------------

glUniformMatrix4x3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix4x3dv v1 v2 v3 v4 = liftIO $ dyn867 ptr_glUniformMatrix4x3dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x3dv #-}
ptr_glUniformMatrix4x3dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix4x3dv = unsafePerformIO $ getCommand "glUniformMatrix4x3dv"

-- glUniformMatrix4x3fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix4x3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x3fv v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix4x3fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x3fv #-}
ptr_glUniformMatrix4x3fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x3fv = unsafePerformIO $ getCommand "glUniformMatrix4x3fv"

-- glUniformMatrix4x3fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix4x3fv'.
glUniformMatrix4x3fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x3fvNV v1 v2 v3 v4 = liftIO $ dyn868 ptr_glUniformMatrix4x3fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x3fvNV #-}
ptr_glUniformMatrix4x3fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x3fvNV = unsafePerformIO $ getCommand "glUniformMatrix4x3fvNV"

-- glUniformSubroutinesuiv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glUniformSubroutines.xhtml OpenGL 4.x>.
glUniformSubroutinesuiv
  :: MonadIO m
  => GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @indices@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glUniformSubroutinesuiv v1 v2 v3 = liftIO $ dyn204 ptr_glUniformSubroutinesuiv v1 v2 v3

{-# NOINLINE ptr_glUniformSubroutinesuiv #-}
ptr_glUniformSubroutinesuiv :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniformSubroutinesuiv = unsafePerformIO $ getCommand "glUniformSubroutinesuiv"

-- glUniformui64NV -------------------------------------------------------------

glUniformui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @value@.
  -> m ()
glUniformui64NV v1 v2 = liftIO $ dyn844 ptr_glUniformui64NV v1 v2

{-# NOINLINE ptr_glUniformui64NV #-}
ptr_glUniformui64NV :: FunPtr (GLint -> GLuint64EXT -> IO ())
ptr_glUniformui64NV = unsafePerformIO $ getCommand "glUniformui64NV"

-- glUniformui64vNV ------------------------------------------------------------

glUniformui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*1@ elements of type @GLuint64EXT@.
  -> m ()
glUniformui64vNV v1 v2 v3 = liftIO $ dyn846 ptr_glUniformui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniformui64vNV #-}
ptr_glUniformui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniformui64vNV = unsafePerformIO $ getCommand "glUniformui64vNV"

-- glUnlockArraysEXT -----------------------------------------------------------

glUnlockArraysEXT
  :: MonadIO m
  => m ()
glUnlockArraysEXT = liftIO $ dyn11 ptr_glUnlockArraysEXT

{-# NOINLINE ptr_glUnlockArraysEXT #-}
ptr_glUnlockArraysEXT :: FunPtr (IO ())
ptr_glUnlockArraysEXT = unsafePerformIO $ getCommand "glUnlockArraysEXT"

-- glUnmapBuffer ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMapBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUnmapBuffer.xhtml OpenGL 4.x>.
glUnmapBuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapBuffer v1 = liftIO $ dyn501 ptr_glUnmapBuffer v1

{-# NOINLINE ptr_glUnmapBuffer #-}
ptr_glUnmapBuffer :: FunPtr (GLenum -> IO GLboolean)
ptr_glUnmapBuffer = unsafePerformIO $ getCommand "glUnmapBuffer"

-- glUnmapBufferARB ------------------------------------------------------------

-- | This command is an alias for 'glUnmapBuffer'.
glUnmapBufferARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapBufferARB v1 = liftIO $ dyn501 ptr_glUnmapBufferARB v1

{-# NOINLINE ptr_glUnmapBufferARB #-}
ptr_glUnmapBufferARB :: FunPtr (GLenum -> IO GLboolean)
ptr_glUnmapBufferARB = unsafePerformIO $ getCommand "glUnmapBufferARB"

-- glUnmapBufferOES ------------------------------------------------------------

-- | This command is an alias for 'glUnmapBuffer'.
glUnmapBufferOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapBufferOES v1 = liftIO $ dyn501 ptr_glUnmapBufferOES v1

{-# NOINLINE ptr_glUnmapBufferOES #-}
ptr_glUnmapBufferOES :: FunPtr (GLenum -> IO GLboolean)
ptr_glUnmapBufferOES = unsafePerformIO $ getCommand "glUnmapBufferOES"

-- glUnmapNamedBuffer ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glUnmapBuffer.xhtml OpenGL 4.x>.
glUnmapNamedBuffer
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapNamedBuffer v1 = liftIO $ dyn284 ptr_glUnmapNamedBuffer v1

{-# NOINLINE ptr_glUnmapNamedBuffer #-}
ptr_glUnmapNamedBuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glUnmapNamedBuffer = unsafePerformIO $ getCommand "glUnmapNamedBuffer"

-- glUnmapNamedBufferEXT -------------------------------------------------------

glUnmapNamedBufferEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapNamedBufferEXT v1 = liftIO $ dyn284 ptr_glUnmapNamedBufferEXT v1

{-# NOINLINE ptr_glUnmapNamedBufferEXT #-}
ptr_glUnmapNamedBufferEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glUnmapNamedBufferEXT = unsafePerformIO $ getCommand "glUnmapNamedBufferEXT"

-- glUnmapObjectBufferATI ------------------------------------------------------

glUnmapObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m ()
glUnmapObjectBufferATI v1 = liftIO $ dyn3 ptr_glUnmapObjectBufferATI v1

{-# NOINLINE ptr_glUnmapObjectBufferATI #-}
ptr_glUnmapObjectBufferATI :: FunPtr (GLuint -> IO ())
ptr_glUnmapObjectBufferATI = unsafePerformIO $ getCommand "glUnmapObjectBufferATI"

-- glUnmapTexture2DINTEL -------------------------------------------------------

glUnmapTexture2DINTEL
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glUnmapTexture2DINTEL v1 v2 = liftIO $ dyn499 ptr_glUnmapTexture2DINTEL v1 v2

{-# NOINLINE ptr_glUnmapTexture2DINTEL #-}
ptr_glUnmapTexture2DINTEL :: FunPtr (GLuint -> GLint -> IO ())
ptr_glUnmapTexture2DINTEL = unsafePerformIO $ getCommand "glUnmapTexture2DINTEL"

