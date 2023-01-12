{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F17
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

module Graphics.GL.Functions.F17 (
  glMultMatrixf,
  glMultMatrixx,
  glMultMatrixxOES,
  glMultTransposeMatrixd,
  glMultTransposeMatrixdARB,
  glMultTransposeMatrixf,
  glMultTransposeMatrixfARB,
  glMultTransposeMatrixxOES,
  glMultiDrawArrays,
  glMultiDrawArraysEXT,
  glMultiDrawArraysIndirect,
  glMultiDrawArraysIndirectAMD,
  glMultiDrawArraysIndirectBindlessCountNV,
  glMultiDrawArraysIndirectBindlessNV,
  glMultiDrawArraysIndirectCount,
  glMultiDrawArraysIndirectCountARB,
  glMultiDrawArraysIndirectEXT,
  glMultiDrawElementArrayAPPLE,
  glMultiDrawElements,
  glMultiDrawElementsBaseVertex,
  glMultiDrawElementsBaseVertexEXT,
  glMultiDrawElementsEXT,
  glMultiDrawElementsIndirect,
  glMultiDrawElementsIndirectAMD,
  glMultiDrawElementsIndirectBindlessCountNV,
  glMultiDrawElementsIndirectBindlessNV,
  glMultiDrawElementsIndirectCount,
  glMultiDrawElementsIndirectCountARB,
  glMultiDrawElementsIndirectEXT,
  glMultiDrawMeshTasksIndirectCountNV,
  glMultiDrawMeshTasksIndirectNV,
  glMultiDrawRangeElementArrayAPPLE,
  glMultiModeDrawArraysIBM,
  glMultiModeDrawElementsIBM,
  glMultiTexBufferEXT,
  glMultiTexCoord1bOES,
  glMultiTexCoord1bvOES,
  glMultiTexCoord1d,
  glMultiTexCoord1dARB,
  glMultiTexCoord1dv,
  glMultiTexCoord1dvARB,
  glMultiTexCoord1f,
  glMultiTexCoord1fARB,
  glMultiTexCoord1fv,
  glMultiTexCoord1fvARB,
  glMultiTexCoord1hNV,
  glMultiTexCoord1hvNV,
  glMultiTexCoord1i,
  glMultiTexCoord1iARB,
  glMultiTexCoord1iv,
  glMultiTexCoord1ivARB,
  glMultiTexCoord1s,
  glMultiTexCoord1sARB,
  glMultiTexCoord1sv,
  glMultiTexCoord1svARB,
  glMultiTexCoord1xOES,
  glMultiTexCoord1xvOES,
  glMultiTexCoord2bOES,
  glMultiTexCoord2bvOES,
  glMultiTexCoord2d,
  glMultiTexCoord2dARB,
  glMultiTexCoord2dv,
  glMultiTexCoord2dvARB,
  glMultiTexCoord2f,
  glMultiTexCoord2fARB,
  glMultiTexCoord2fv,
  glMultiTexCoord2fvARB,
  glMultiTexCoord2hNV,
  glMultiTexCoord2hvNV,
  glMultiTexCoord2i,
  glMultiTexCoord2iARB,
  glMultiTexCoord2iv,
  glMultiTexCoord2ivARB,
  glMultiTexCoord2s,
  glMultiTexCoord2sARB,
  glMultiTexCoord2sv,
  glMultiTexCoord2svARB,
  glMultiTexCoord2xOES,
  glMultiTexCoord2xvOES,
  glMultiTexCoord3bOES,
  glMultiTexCoord3bvOES,
  glMultiTexCoord3d,
  glMultiTexCoord3dARB,
  glMultiTexCoord3dv,
  glMultiTexCoord3dvARB,
  glMultiTexCoord3f,
  glMultiTexCoord3fARB,
  glMultiTexCoord3fv,
  glMultiTexCoord3fvARB,
  glMultiTexCoord3hNV,
  glMultiTexCoord3hvNV,
  glMultiTexCoord3i,
  glMultiTexCoord3iARB,
  glMultiTexCoord3iv,
  glMultiTexCoord3ivARB,
  glMultiTexCoord3s,
  glMultiTexCoord3sARB,
  glMultiTexCoord3sv,
  glMultiTexCoord3svARB,
  glMultiTexCoord3xOES
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glMultMatrixf ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml OpenGL 2.x>.
glMultMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMultMatrixf v1 = liftIO $ dyn44 ptr_glMultMatrixf v1

{-# NOINLINE ptr_glMultMatrixf #-}
ptr_glMultMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glMultMatrixf = unsafePerformIO $ getCommand "glMultMatrixf"

-- glMultMatrixx ---------------------------------------------------------------

glMultMatrixx
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glMultMatrixx v1 = liftIO $ dyn114 ptr_glMultMatrixx v1

{-# NOINLINE ptr_glMultMatrixx #-}
ptr_glMultMatrixx :: FunPtr (Ptr GLfixed -> IO ())
ptr_glMultMatrixx = unsafePerformIO $ getCommand "glMultMatrixx"

-- glMultMatrixxOES ------------------------------------------------------------

glMultMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glMultMatrixxOES v1 = liftIO $ dyn114 ptr_glMultMatrixxOES v1

{-# NOINLINE ptr_glMultMatrixxOES #-}
ptr_glMultMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glMultMatrixxOES = unsafePerformIO $ getCommand "glMultMatrixxOES"

-- glMultTransposeMatrixd ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml OpenGL 2.x>.
glMultTransposeMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMultTransposeMatrixd v1 = liftIO $ dyn42 ptr_glMultTransposeMatrixd v1

{-# NOINLINE ptr_glMultTransposeMatrixd #-}
ptr_glMultTransposeMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glMultTransposeMatrixd = unsafePerformIO $ getCommand "glMultTransposeMatrixd"

-- glMultTransposeMatrixdARB ---------------------------------------------------

-- | This command is an alias for 'glMultTransposeMatrixd'.
glMultTransposeMatrixdARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMultTransposeMatrixdARB v1 = liftIO $ dyn42 ptr_glMultTransposeMatrixdARB v1

{-# NOINLINE ptr_glMultTransposeMatrixdARB #-}
ptr_glMultTransposeMatrixdARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glMultTransposeMatrixdARB = unsafePerformIO $ getCommand "glMultTransposeMatrixdARB"

-- glMultTransposeMatrixf ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml OpenGL 2.x>.
glMultTransposeMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMultTransposeMatrixf v1 = liftIO $ dyn44 ptr_glMultTransposeMatrixf v1

{-# NOINLINE ptr_glMultTransposeMatrixf #-}
ptr_glMultTransposeMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glMultTransposeMatrixf = unsafePerformIO $ getCommand "glMultTransposeMatrixf"

-- glMultTransposeMatrixfARB ---------------------------------------------------

-- | This command is an alias for 'glMultTransposeMatrixf'.
glMultTransposeMatrixfARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMultTransposeMatrixfARB v1 = liftIO $ dyn44 ptr_glMultTransposeMatrixfARB v1

{-# NOINLINE ptr_glMultTransposeMatrixfARB #-}
ptr_glMultTransposeMatrixfARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glMultTransposeMatrixfARB = unsafePerformIO $ getCommand "glMultTransposeMatrixfARB"

-- glMultTransposeMatrixxOES ---------------------------------------------------

glMultTransposeMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glMultTransposeMatrixxOES v1 = liftIO $ dyn114 ptr_glMultTransposeMatrixxOES v1

{-# NOINLINE ptr_glMultTransposeMatrixxOES #-}
ptr_glMultTransposeMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glMultTransposeMatrixxOES = unsafePerformIO $ getCommand "glMultTransposeMatrixxOES"

-- glMultiDrawArrays -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiDrawArrays.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawArrays.xhtml OpenGL 4.x>.
glMultiDrawArrays
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @COMPSIZE(drawcount)@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLsizei -- ^ @drawcount@.
  -> m ()
glMultiDrawArrays v1 v2 v3 v4 = liftIO $ dyn551 ptr_glMultiDrawArrays v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArrays #-}
ptr_glMultiDrawArrays :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArrays = unsafePerformIO $ getCommand "glMultiDrawArrays"

-- glMultiDrawArraysEXT --------------------------------------------------------

-- | This command is an alias for 'glMultiDrawArrays'.
glMultiDrawArraysEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @COMPSIZE(primcount)@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawArraysEXT v1 v2 v3 v4 = liftIO $ dyn551 ptr_glMultiDrawArraysEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysEXT #-}
ptr_glMultiDrawArraysEXT :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysEXT = unsafePerformIO $ getCommand "glMultiDrawArraysEXT"

-- glMultiDrawArraysIndirect ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawArraysIndirect.xhtml OpenGL 4.x>.
glMultiDrawArraysIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirect v1 v2 v3 v4 = liftIO $ dyn552 ptr_glMultiDrawArraysIndirect v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysIndirect #-}
ptr_glMultiDrawArraysIndirect :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirect = unsafePerformIO $ getCommand "glMultiDrawArraysIndirect"

-- glMultiDrawArraysIndirectAMD ------------------------------------------------

-- | This command is an alias for 'glMultiDrawArraysIndirect'.
glMultiDrawArraysIndirectAMD
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @primcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirectAMD v1 v2 v3 v4 = liftIO $ dyn552 ptr_glMultiDrawArraysIndirectAMD v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysIndirectAMD #-}
ptr_glMultiDrawArraysIndirectAMD :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirectAMD = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectAMD"

-- glMultiDrawArraysIndirectBindlessCountNV ------------------------------------

glMultiDrawArraysIndirectBindlessCountNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @maxDrawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawArraysIndirectBindlessCountNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn553 ptr_glMultiDrawArraysIndirectBindlessCountNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawArraysIndirectBindlessCountNV #-}
ptr_glMultiDrawArraysIndirectBindlessCountNV :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawArraysIndirectBindlessCountNV = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectBindlessCountNV"

-- glMultiDrawArraysIndirectBindlessNV -----------------------------------------

glMultiDrawArraysIndirectBindlessNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawArraysIndirectBindlessNV v1 v2 v3 v4 v5 = liftIO $ dyn554 ptr_glMultiDrawArraysIndirectBindlessNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawArraysIndirectBindlessNV #-}
ptr_glMultiDrawArraysIndirectBindlessNV :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawArraysIndirectBindlessNV = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectBindlessNV"

-- glMultiDrawArraysIndirectCount ----------------------------------------------

glMultiDrawArraysIndirectCount
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@.
  -> GLintptr -- ^ @drawcount@.
  -> GLsizei -- ^ @maxdrawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirectCount v1 v2 v3 v4 v5 = liftIO $ dyn555 ptr_glMultiDrawArraysIndirectCount v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawArraysIndirectCount #-}
ptr_glMultiDrawArraysIndirectCount :: FunPtr (GLenum -> Ptr a -> GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirectCount = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectCount"

-- glMultiDrawArraysIndirectCountARB -------------------------------------------

-- | This command is an alias for 'glMultiDrawArraysIndirectCount'.
glMultiDrawArraysIndirectCountARB
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@.
  -> GLintptr -- ^ @drawcount@.
  -> GLsizei -- ^ @maxdrawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirectCountARB v1 v2 v3 v4 v5 = liftIO $ dyn555 ptr_glMultiDrawArraysIndirectCountARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawArraysIndirectCountARB #-}
ptr_glMultiDrawArraysIndirectCountARB :: FunPtr (GLenum -> Ptr a -> GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirectCountARB = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectCountARB"

-- glMultiDrawArraysIndirectEXT ------------------------------------------------

-- | This command is an alias for 'glMultiDrawArraysIndirect'.
glMultiDrawArraysIndirectEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirectEXT v1 v2 v3 v4 = liftIO $ dyn552 ptr_glMultiDrawArraysIndirectEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysIndirectEXT #-}
ptr_glMultiDrawArraysIndirectEXT :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirectEXT = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectEXT"

-- glMultiDrawElementArrayAPPLE ------------------------------------------------

glMultiDrawElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @primcount@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @primcount@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawElementArrayAPPLE v1 v2 v3 v4 = liftIO $ dyn551 ptr_glMultiDrawElementArrayAPPLE v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawElementArrayAPPLE #-}
ptr_glMultiDrawElementArrayAPPLE :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementArrayAPPLE = unsafePerformIO $ getCommand "glMultiDrawElementArrayAPPLE"

-- glMultiDrawElements ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiDrawElements.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElements.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawElements.xhtml OpenGL 4.x>.
glMultiDrawElements
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(drawcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @drawcount@.
  -> m ()
glMultiDrawElements v1 v2 v3 v4 v5 = liftIO $ dyn556 ptr_glMultiDrawElements v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElements #-}
ptr_glMultiDrawElements :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> IO ())
ptr_glMultiDrawElements = unsafePerformIO $ getCommand "glMultiDrawElements"

-- glMultiDrawElementsBaseVertex -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElementsBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawElementsBaseVertex.xhtml OpenGL 4.x>.
glMultiDrawElementsBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(drawcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @drawcount@.
  -> Ptr GLint -- ^ @basevertex@ pointing to @COMPSIZE(drawcount)@ elements of type @GLint@.
  -> m ()
glMultiDrawElementsBaseVertex v1 v2 v3 v4 v5 v6 = liftIO $ dyn557 ptr_glMultiDrawElementsBaseVertex v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsBaseVertex #-}
ptr_glMultiDrawElementsBaseVertex :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> Ptr GLint -> IO ())
ptr_glMultiDrawElementsBaseVertex = unsafePerformIO $ getCommand "glMultiDrawElementsBaseVertex"

-- glMultiDrawElementsBaseVertexEXT --------------------------------------------

-- | This command is an alias for 'glMultiDrawElementsBaseVertex'.
glMultiDrawElementsBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(drawcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @primcount@.
  -> Ptr GLint -- ^ @basevertex@ pointing to @COMPSIZE(drawcount)@ elements of type @GLint@.
  -> m ()
glMultiDrawElementsBaseVertexEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn557 ptr_glMultiDrawElementsBaseVertexEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsBaseVertexEXT #-}
ptr_glMultiDrawElementsBaseVertexEXT :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> Ptr GLint -> IO ())
ptr_glMultiDrawElementsBaseVertexEXT = unsafePerformIO $ getCommand "glMultiDrawElementsBaseVertexEXT"

-- glMultiDrawElementsEXT ------------------------------------------------------

-- | This command is an alias for 'glMultiDrawElements'.
glMultiDrawElementsEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(primcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawElementsEXT v1 v2 v3 v4 v5 = liftIO $ dyn556 ptr_glMultiDrawElementsEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsEXT #-}
ptr_glMultiDrawElementsEXT :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> IO ())
ptr_glMultiDrawElementsEXT = unsafePerformIO $ getCommand "glMultiDrawElementsEXT"

-- glMultiDrawElementsIndirect -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawElementsIndirect.xhtml OpenGL 4.x>.
glMultiDrawElementsIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirect v1 v2 v3 v4 v5 = liftIO $ dyn558 ptr_glMultiDrawElementsIndirect v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsIndirect #-}
ptr_glMultiDrawElementsIndirect :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirect = unsafePerformIO $ getCommand "glMultiDrawElementsIndirect"

-- glMultiDrawElementsIndirectAMD ----------------------------------------------

-- | This command is an alias for 'glMultiDrawElementsIndirect'.
glMultiDrawElementsIndirectAMD
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @primcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirectAMD v1 v2 v3 v4 v5 = liftIO $ dyn558 ptr_glMultiDrawElementsIndirectAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsIndirectAMD #-}
ptr_glMultiDrawElementsIndirectAMD :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirectAMD = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectAMD"

-- glMultiDrawElementsIndirectBindlessCountNV ----------------------------------

glMultiDrawElementsIndirectBindlessCountNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @maxDrawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawElementsIndirectBindlessCountNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn559 ptr_glMultiDrawElementsIndirectBindlessCountNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMultiDrawElementsIndirectBindlessCountNV #-}
ptr_glMultiDrawElementsIndirectBindlessCountNV :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawElementsIndirectBindlessCountNV = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectBindlessCountNV"

-- glMultiDrawElementsIndirectBindlessNV ---------------------------------------

glMultiDrawElementsIndirectBindlessNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawElementsIndirectBindlessNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn560 ptr_glMultiDrawElementsIndirectBindlessNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsIndirectBindlessNV #-}
ptr_glMultiDrawElementsIndirectBindlessNV :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawElementsIndirectBindlessNV = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectBindlessNV"

-- glMultiDrawElementsIndirectCount --------------------------------------------

glMultiDrawElementsIndirectCount
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@.
  -> GLintptr -- ^ @drawcount@.
  -> GLsizei -- ^ @maxdrawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirectCount v1 v2 v3 v4 v5 v6 = liftIO $ dyn561 ptr_glMultiDrawElementsIndirectCount v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsIndirectCount #-}
ptr_glMultiDrawElementsIndirectCount :: FunPtr (GLenum -> GLenum -> Ptr a -> GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirectCount = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectCount"

-- glMultiDrawElementsIndirectCountARB -----------------------------------------

-- | This command is an alias for 'glMultiDrawElementsIndirectCount'.
glMultiDrawElementsIndirectCountARB
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@.
  -> GLintptr -- ^ @drawcount@.
  -> GLsizei -- ^ @maxdrawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirectCountARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn561 ptr_glMultiDrawElementsIndirectCountARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsIndirectCountARB #-}
ptr_glMultiDrawElementsIndirectCountARB :: FunPtr (GLenum -> GLenum -> Ptr a -> GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirectCountARB = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectCountARB"

-- glMultiDrawElementsIndirectEXT ----------------------------------------------

-- | This command is an alias for 'glMultiDrawElementsIndirect'.
glMultiDrawElementsIndirectEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirectEXT v1 v2 v3 v4 v5 = liftIO $ dyn558 ptr_glMultiDrawElementsIndirectEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsIndirectEXT #-}
ptr_glMultiDrawElementsIndirectEXT :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirectEXT = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectEXT"

-- glMultiDrawMeshTasksIndirectCountNV -----------------------------------------

glMultiDrawMeshTasksIndirectCountNV
  :: MonadIO m
  => GLintptr -- ^ @indirect@.
  -> GLintptr -- ^ @drawcount@.
  -> GLsizei -- ^ @maxdrawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawMeshTasksIndirectCountNV v1 v2 v3 v4 = liftIO $ dyn562 ptr_glMultiDrawMeshTasksIndirectCountNV v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawMeshTasksIndirectCountNV #-}
ptr_glMultiDrawMeshTasksIndirectCountNV :: FunPtr (GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawMeshTasksIndirectCountNV = unsafePerformIO $ getCommand "glMultiDrawMeshTasksIndirectCountNV"

-- glMultiDrawMeshTasksIndirectNV ----------------------------------------------

glMultiDrawMeshTasksIndirectNV
  :: MonadIO m
  => GLintptr -- ^ @indirect@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawMeshTasksIndirectNV v1 v2 v3 = liftIO $ dyn563 ptr_glMultiDrawMeshTasksIndirectNV v1 v2 v3

{-# NOINLINE ptr_glMultiDrawMeshTasksIndirectNV #-}
ptr_glMultiDrawMeshTasksIndirectNV :: FunPtr (GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawMeshTasksIndirectNV = unsafePerformIO $ getCommand "glMultiDrawMeshTasksIndirectNV"

-- glMultiDrawRangeElementArrayAPPLE -------------------------------------------

glMultiDrawRangeElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> Ptr GLint -- ^ @first@ pointing to @primcount@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @primcount@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5 v6 = liftIO $ dyn564 ptr_glMultiDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawRangeElementArrayAPPLE #-}
ptr_glMultiDrawRangeElementArrayAPPLE :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawRangeElementArrayAPPLE = unsafePerformIO $ getCommand "glMultiDrawRangeElementArrayAPPLE"

-- glMultiModeDrawArraysIBM ----------------------------------------------------

glMultiModeDrawArraysIBM
  :: MonadIO m
  => Ptr GLenum -- ^ @mode@ pointing to @COMPSIZE(primcount)@ elements of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @COMPSIZE(primcount)@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> GLint -- ^ @modestride@.
  -> m ()
glMultiModeDrawArraysIBM v1 v2 v3 v4 v5 = liftIO $ dyn565 ptr_glMultiModeDrawArraysIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiModeDrawArraysIBM #-}
ptr_glMultiModeDrawArraysIBM :: FunPtr (Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiModeDrawArraysIBM = unsafePerformIO $ getCommand "glMultiModeDrawArraysIBM"

-- glMultiModeDrawElementsIBM --------------------------------------------------

glMultiModeDrawElementsIBM
  :: MonadIO m
  => Ptr GLenum -- ^ @mode@ pointing to @COMPSIZE(primcount)@ elements of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(primcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @primcount@.
  -> GLint -- ^ @modestride@.
  -> m ()
glMultiModeDrawElementsIBM v1 v2 v3 v4 v5 v6 = liftIO $ dyn566 ptr_glMultiModeDrawElementsIBM v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiModeDrawElementsIBM #-}
ptr_glMultiModeDrawElementsIBM :: FunPtr (Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> GLint -> IO ())
ptr_glMultiModeDrawElementsIBM = unsafePerformIO $ getCommand "glMultiModeDrawElementsIBM"

-- glMultiTexBufferEXT ---------------------------------------------------------

glMultiTexBufferEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glMultiTexBufferEXT v1 v2 v3 v4 = liftIO $ dyn296 ptr_glMultiTexBufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexBufferEXT #-}
ptr_glMultiTexBufferEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexBufferEXT = unsafePerformIO $ getCommand "glMultiTexBufferEXT"

-- glMultiTexCoord1bOES --------------------------------------------------------

glMultiTexCoord1bOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLbyte -- ^ @s@.
  -> m ()
glMultiTexCoord1bOES v1 v2 = liftIO $ dyn567 ptr_glMultiTexCoord1bOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1bOES #-}
ptr_glMultiTexCoord1bOES :: FunPtr (GLenum -> GLbyte -> IO ())
ptr_glMultiTexCoord1bOES = unsafePerformIO $ getCommand "glMultiTexCoord1bOES"

-- glMultiTexCoord1bvOES -------------------------------------------------------

glMultiTexCoord1bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLbyte -- ^ @coords@ pointing to @1@ element of type @GLbyte@.
  -> m ()
glMultiTexCoord1bvOES v1 v2 = liftIO $ dyn568 ptr_glMultiTexCoord1bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1bvOES #-}
ptr_glMultiTexCoord1bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord1bvOES = unsafePerformIO $ getCommand "glMultiTexCoord1bvOES"

-- glMultiTexCoord1d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1dv'.
glMultiTexCoord1d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> m ()
glMultiTexCoord1d v1 v2 = liftIO $ dyn569 ptr_glMultiTexCoord1d v1 v2

{-# NOINLINE ptr_glMultiTexCoord1d #-}
ptr_glMultiTexCoord1d :: FunPtr (GLenum -> GLdouble -> IO ())
ptr_glMultiTexCoord1d = unsafePerformIO $ getCommand "glMultiTexCoord1d"

-- glMultiTexCoord1dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1dv'. This command is an alias for 'glMultiTexCoord1d'.
glMultiTexCoord1dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> m ()
glMultiTexCoord1dARB v1 v2 = liftIO $ dyn569 ptr_glMultiTexCoord1dARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1dARB #-}
ptr_glMultiTexCoord1dARB :: FunPtr (GLenum -> GLdouble -> IO ())
ptr_glMultiTexCoord1dARB = unsafePerformIO $ getCommand "glMultiTexCoord1dARB"

-- glMultiTexCoord1dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @CoordD@.
  -> m ()
glMultiTexCoord1dv v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord1dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1dv #-}
ptr_glMultiTexCoord1dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord1dv = unsafePerformIO $ getCommand "glMultiTexCoord1dv"

-- glMultiTexCoord1dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1dv'.
glMultiTexCoord1dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @CoordD@.
  -> m ()
glMultiTexCoord1dvARB v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord1dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1dvARB #-}
ptr_glMultiTexCoord1dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord1dvARB = unsafePerformIO $ getCommand "glMultiTexCoord1dvARB"

-- glMultiTexCoord1f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1fv'.
glMultiTexCoord1f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> m ()
glMultiTexCoord1f v1 v2 = liftIO $ dyn0 ptr_glMultiTexCoord1f v1 v2

{-# NOINLINE ptr_glMultiTexCoord1f #-}
ptr_glMultiTexCoord1f :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glMultiTexCoord1f = unsafePerformIO $ getCommand "glMultiTexCoord1f"

-- glMultiTexCoord1fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1fv'. This command is an alias for 'glMultiTexCoord1f'.
glMultiTexCoord1fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> m ()
glMultiTexCoord1fARB v1 v2 = liftIO $ dyn0 ptr_glMultiTexCoord1fARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1fARB #-}
ptr_glMultiTexCoord1fARB :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glMultiTexCoord1fARB = unsafePerformIO $ getCommand "glMultiTexCoord1fARB"

-- glMultiTexCoord1fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @CoordF@.
  -> m ()
glMultiTexCoord1fv v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord1fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1fv #-}
ptr_glMultiTexCoord1fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord1fv = unsafePerformIO $ getCommand "glMultiTexCoord1fv"

-- glMultiTexCoord1fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1fv'.
glMultiTexCoord1fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @CoordF@.
  -> m ()
glMultiTexCoord1fvARB v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord1fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1fvARB #-}
ptr_glMultiTexCoord1fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord1fvARB = unsafePerformIO $ getCommand "glMultiTexCoord1fvARB"

-- glMultiTexCoord1hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1hvNV'.
glMultiTexCoord1hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> m ()
glMultiTexCoord1hNV v1 v2 = liftIO $ dyn570 ptr_glMultiTexCoord1hNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord1hNV #-}
ptr_glMultiTexCoord1hNV :: FunPtr (GLenum -> GLhalfNV -> IO ())
ptr_glMultiTexCoord1hNV = unsafePerformIO $ getCommand "glMultiTexCoord1hNV"

-- glMultiTexCoord1hvNV --------------------------------------------------------

glMultiTexCoord1hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLhalfNV -- ^ @v@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glMultiTexCoord1hvNV v1 v2 = liftIO $ dyn571 ptr_glMultiTexCoord1hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord1hvNV #-}
ptr_glMultiTexCoord1hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord1hvNV = unsafePerformIO $ getCommand "glMultiTexCoord1hvNV"

-- glMultiTexCoord1i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1iv'.
glMultiTexCoord1i
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> m ()
glMultiTexCoord1i v1 v2 = liftIO $ dyn58 ptr_glMultiTexCoord1i v1 v2

{-# NOINLINE ptr_glMultiTexCoord1i #-}
ptr_glMultiTexCoord1i :: FunPtr (GLenum -> GLint -> IO ())
ptr_glMultiTexCoord1i = unsafePerformIO $ getCommand "glMultiTexCoord1i"

-- glMultiTexCoord1iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1iv'. This command is an alias for 'glMultiTexCoord1i'.
glMultiTexCoord1iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> m ()
glMultiTexCoord1iARB v1 v2 = liftIO $ dyn58 ptr_glMultiTexCoord1iARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1iARB #-}
ptr_glMultiTexCoord1iARB :: FunPtr (GLenum -> GLint -> IO ())
ptr_glMultiTexCoord1iARB = unsafePerformIO $ getCommand "glMultiTexCoord1iARB"

-- glMultiTexCoord1iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @CoordI@.
  -> m ()
glMultiTexCoord1iv v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord1iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1iv #-}
ptr_glMultiTexCoord1iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord1iv = unsafePerformIO $ getCommand "glMultiTexCoord1iv"

-- glMultiTexCoord1ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1iv'.
glMultiTexCoord1ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @CoordI@.
  -> m ()
glMultiTexCoord1ivARB v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord1ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1ivARB #-}
ptr_glMultiTexCoord1ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord1ivARB = unsafePerformIO $ getCommand "glMultiTexCoord1ivARB"

-- glMultiTexCoord1s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1sv'.
glMultiTexCoord1s
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> m ()
glMultiTexCoord1s v1 v2 = liftIO $ dyn572 ptr_glMultiTexCoord1s v1 v2

{-# NOINLINE ptr_glMultiTexCoord1s #-}
ptr_glMultiTexCoord1s :: FunPtr (GLenum -> GLshort -> IO ())
ptr_glMultiTexCoord1s = unsafePerformIO $ getCommand "glMultiTexCoord1s"

-- glMultiTexCoord1sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1sv'. This command is an alias for 'glMultiTexCoord1s'.
glMultiTexCoord1sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> m ()
glMultiTexCoord1sARB v1 v2 = liftIO $ dyn572 ptr_glMultiTexCoord1sARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1sARB #-}
ptr_glMultiTexCoord1sARB :: FunPtr (GLenum -> GLshort -> IO ())
ptr_glMultiTexCoord1sARB = unsafePerformIO $ getCommand "glMultiTexCoord1sARB"

-- glMultiTexCoord1sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @CoordS@.
  -> m ()
glMultiTexCoord1sv v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord1sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1sv #-}
ptr_glMultiTexCoord1sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord1sv = unsafePerformIO $ getCommand "glMultiTexCoord1sv"

-- glMultiTexCoord1svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1sv'.
glMultiTexCoord1svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @CoordS@.
  -> m ()
glMultiTexCoord1svARB v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord1svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1svARB #-}
ptr_glMultiTexCoord1svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord1svARB = unsafePerformIO $ getCommand "glMultiTexCoord1svARB"

-- glMultiTexCoord1xOES --------------------------------------------------------

glMultiTexCoord1xOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfixed -- ^ @s@.
  -> m ()
glMultiTexCoord1xOES v1 v2 = liftIO $ dyn1 ptr_glMultiTexCoord1xOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1xOES #-}
ptr_glMultiTexCoord1xOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glMultiTexCoord1xOES = unsafePerformIO $ getCommand "glMultiTexCoord1xOES"

-- glMultiTexCoord1xvOES -------------------------------------------------------

glMultiTexCoord1xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfixed -- ^ @coords@ pointing to @1@ element of type @GLfixed@.
  -> m ()
glMultiTexCoord1xvOES v1 v2 = liftIO $ dyn102 ptr_glMultiTexCoord1xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1xvOES #-}
ptr_glMultiTexCoord1xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord1xvOES = unsafePerformIO $ getCommand "glMultiTexCoord1xvOES"

-- glMultiTexCoord2bOES --------------------------------------------------------

glMultiTexCoord2bOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> m ()
glMultiTexCoord2bOES v1 v2 v3 = liftIO $ dyn574 ptr_glMultiTexCoord2bOES v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2bOES #-}
ptr_glMultiTexCoord2bOES :: FunPtr (GLenum -> GLbyte -> GLbyte -> IO ())
ptr_glMultiTexCoord2bOES = unsafePerformIO $ getCommand "glMultiTexCoord2bOES"

-- glMultiTexCoord2bvOES -------------------------------------------------------

glMultiTexCoord2bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLbyte -- ^ @coords@ pointing to @2@ elements of type @GLbyte@.
  -> m ()
glMultiTexCoord2bvOES v1 v2 = liftIO $ dyn568 ptr_glMultiTexCoord2bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord2bvOES #-}
ptr_glMultiTexCoord2bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord2bvOES = unsafePerformIO $ getCommand "glMultiTexCoord2bvOES"

-- glMultiTexCoord2d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2dv'.
glMultiTexCoord2d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> m ()
glMultiTexCoord2d v1 v2 v3 = liftIO $ dyn575 ptr_glMultiTexCoord2d v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2d #-}
ptr_glMultiTexCoord2d :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord2d = unsafePerformIO $ getCommand "glMultiTexCoord2d"

-- glMultiTexCoord2dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2dv'. This command is an alias for 'glMultiTexCoord2d'.
glMultiTexCoord2dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> m ()
glMultiTexCoord2dARB v1 v2 v3 = liftIO $ dyn575 ptr_glMultiTexCoord2dARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2dARB #-}
ptr_glMultiTexCoord2dARB :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord2dARB = unsafePerformIO $ getCommand "glMultiTexCoord2dARB"

-- glMultiTexCoord2dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord2dv v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord2dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2dv #-}
ptr_glMultiTexCoord2dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord2dv = unsafePerformIO $ getCommand "glMultiTexCoord2dv"

-- glMultiTexCoord2dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2dv'.
glMultiTexCoord2dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord2dvARB v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord2dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2dvARB #-}
ptr_glMultiTexCoord2dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord2dvARB = unsafePerformIO $ getCommand "glMultiTexCoord2dvARB"

-- glMultiTexCoord2f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2fv'.
glMultiTexCoord2f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> m ()
glMultiTexCoord2f v1 v2 v3 = liftIO $ dyn576 ptr_glMultiTexCoord2f v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2f #-}
ptr_glMultiTexCoord2f :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord2f = unsafePerformIO $ getCommand "glMultiTexCoord2f"

-- glMultiTexCoord2fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2fv'. This command is an alias for 'glMultiTexCoord2f'.
glMultiTexCoord2fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> m ()
glMultiTexCoord2fARB v1 v2 v3 = liftIO $ dyn576 ptr_glMultiTexCoord2fARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2fARB #-}
ptr_glMultiTexCoord2fARB :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord2fARB = unsafePerformIO $ getCommand "glMultiTexCoord2fARB"

-- glMultiTexCoord2fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord2fv v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord2fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2fv #-}
ptr_glMultiTexCoord2fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord2fv = unsafePerformIO $ getCommand "glMultiTexCoord2fv"

-- glMultiTexCoord2fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2fv'.
glMultiTexCoord2fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord2fvARB v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord2fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2fvARB #-}
ptr_glMultiTexCoord2fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord2fvARB = unsafePerformIO $ getCommand "glMultiTexCoord2fvARB"

-- glMultiTexCoord2hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2hvNV'.
glMultiTexCoord2hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> m ()
glMultiTexCoord2hNV v1 v2 v3 = liftIO $ dyn577 ptr_glMultiTexCoord2hNV v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2hNV #-}
ptr_glMultiTexCoord2hNV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glMultiTexCoord2hNV = unsafePerformIO $ getCommand "glMultiTexCoord2hNV"

-- glMultiTexCoord2hvNV --------------------------------------------------------

glMultiTexCoord2hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLhalfNV -- ^ @v@ pointing to @2@ elements of type @Half16NV@.
  -> m ()
glMultiTexCoord2hvNV v1 v2 = liftIO $ dyn571 ptr_glMultiTexCoord2hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord2hvNV #-}
ptr_glMultiTexCoord2hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord2hvNV = unsafePerformIO $ getCommand "glMultiTexCoord2hvNV"

-- glMultiTexCoord2i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2iv'.
glMultiTexCoord2i
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> m ()
glMultiTexCoord2i v1 v2 v3 = liftIO $ dyn275 ptr_glMultiTexCoord2i v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2i #-}
ptr_glMultiTexCoord2i :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord2i = unsafePerformIO $ getCommand "glMultiTexCoord2i"

-- glMultiTexCoord2iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2iv'. This command is an alias for 'glMultiTexCoord2i'.
glMultiTexCoord2iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> m ()
glMultiTexCoord2iARB v1 v2 v3 = liftIO $ dyn275 ptr_glMultiTexCoord2iARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2iARB #-}
ptr_glMultiTexCoord2iARB :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord2iARB = unsafePerformIO $ getCommand "glMultiTexCoord2iARB"

-- glMultiTexCoord2iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord2iv v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord2iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2iv #-}
ptr_glMultiTexCoord2iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord2iv = unsafePerformIO $ getCommand "glMultiTexCoord2iv"

-- glMultiTexCoord2ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2iv'.
glMultiTexCoord2ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord2ivARB v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord2ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2ivARB #-}
ptr_glMultiTexCoord2ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord2ivARB = unsafePerformIO $ getCommand "glMultiTexCoord2ivARB"

-- glMultiTexCoord2s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2sv'.
glMultiTexCoord2s
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> m ()
glMultiTexCoord2s v1 v2 v3 = liftIO $ dyn578 ptr_glMultiTexCoord2s v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2s #-}
ptr_glMultiTexCoord2s :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord2s = unsafePerformIO $ getCommand "glMultiTexCoord2s"

-- glMultiTexCoord2sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2sv'. This command is an alias for 'glMultiTexCoord2s'.
glMultiTexCoord2sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> m ()
glMultiTexCoord2sARB v1 v2 v3 = liftIO $ dyn578 ptr_glMultiTexCoord2sARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2sARB #-}
ptr_glMultiTexCoord2sARB :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord2sARB = unsafePerformIO $ getCommand "glMultiTexCoord2sARB"

-- glMultiTexCoord2sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord2sv v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord2sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2sv #-}
ptr_glMultiTexCoord2sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord2sv = unsafePerformIO $ getCommand "glMultiTexCoord2sv"

-- glMultiTexCoord2svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2sv'.
glMultiTexCoord2svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord2svARB v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord2svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2svARB #-}
ptr_glMultiTexCoord2svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord2svARB = unsafePerformIO $ getCommand "glMultiTexCoord2svARB"

-- glMultiTexCoord2xOES --------------------------------------------------------

glMultiTexCoord2xOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> m ()
glMultiTexCoord2xOES v1 v2 v3 = liftIO $ dyn579 ptr_glMultiTexCoord2xOES v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2xOES #-}
ptr_glMultiTexCoord2xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord2xOES = unsafePerformIO $ getCommand "glMultiTexCoord2xOES"

-- glMultiTexCoord2xvOES -------------------------------------------------------

glMultiTexCoord2xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glMultiTexCoord2xvOES v1 v2 = liftIO $ dyn102 ptr_glMultiTexCoord2xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord2xvOES #-}
ptr_glMultiTexCoord2xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord2xvOES = unsafePerformIO $ getCommand "glMultiTexCoord2xvOES"

-- glMultiTexCoord3bOES --------------------------------------------------------

glMultiTexCoord3bOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> GLbyte -- ^ @r@.
  -> m ()
glMultiTexCoord3bOES v1 v2 v3 v4 = liftIO $ dyn580 ptr_glMultiTexCoord3bOES v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3bOES #-}
ptr_glMultiTexCoord3bOES :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glMultiTexCoord3bOES = unsafePerformIO $ getCommand "glMultiTexCoord3bOES"

-- glMultiTexCoord3bvOES -------------------------------------------------------

glMultiTexCoord3bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLbyte -- ^ @coords@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glMultiTexCoord3bvOES v1 v2 = liftIO $ dyn568 ptr_glMultiTexCoord3bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord3bvOES #-}
ptr_glMultiTexCoord3bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord3bvOES = unsafePerformIO $ getCommand "glMultiTexCoord3bvOES"

-- glMultiTexCoord3d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3dv'.
glMultiTexCoord3d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> m ()
glMultiTexCoord3d v1 v2 v3 v4 = liftIO $ dyn548 ptr_glMultiTexCoord3d v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3d #-}
ptr_glMultiTexCoord3d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord3d = unsafePerformIO $ getCommand "glMultiTexCoord3d"

-- glMultiTexCoord3dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3dv'. This command is an alias for 'glMultiTexCoord3d'.
glMultiTexCoord3dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> m ()
glMultiTexCoord3dARB v1 v2 v3 v4 = liftIO $ dyn548 ptr_glMultiTexCoord3dARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3dARB #-}
ptr_glMultiTexCoord3dARB :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord3dARB = unsafePerformIO $ getCommand "glMultiTexCoord3dARB"

-- glMultiTexCoord3dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord3dv v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord3dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3dv #-}
ptr_glMultiTexCoord3dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord3dv = unsafePerformIO $ getCommand "glMultiTexCoord3dv"

-- glMultiTexCoord3dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3dv'.
glMultiTexCoord3dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord3dvARB v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord3dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3dvARB #-}
ptr_glMultiTexCoord3dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord3dvARB = unsafePerformIO $ getCommand "glMultiTexCoord3dvARB"

-- glMultiTexCoord3f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3fv'.
glMultiTexCoord3f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> m ()
glMultiTexCoord3f v1 v2 v3 v4 = liftIO $ dyn549 ptr_glMultiTexCoord3f v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3f #-}
ptr_glMultiTexCoord3f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord3f = unsafePerformIO $ getCommand "glMultiTexCoord3f"

-- glMultiTexCoord3fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3fv'. This command is an alias for 'glMultiTexCoord3f'.
glMultiTexCoord3fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> m ()
glMultiTexCoord3fARB v1 v2 v3 v4 = liftIO $ dyn549 ptr_glMultiTexCoord3fARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3fARB #-}
ptr_glMultiTexCoord3fARB :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord3fARB = unsafePerformIO $ getCommand "glMultiTexCoord3fARB"

-- glMultiTexCoord3fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord3fv v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord3fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3fv #-}
ptr_glMultiTexCoord3fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord3fv = unsafePerformIO $ getCommand "glMultiTexCoord3fv"

-- glMultiTexCoord3fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3fv'.
glMultiTexCoord3fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord3fvARB v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord3fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3fvARB #-}
ptr_glMultiTexCoord3fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord3fvARB = unsafePerformIO $ getCommand "glMultiTexCoord3fvARB"

-- glMultiTexCoord3hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3hvNV'.
glMultiTexCoord3hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> GLhalfNV -- ^ @r@ of type @Half16NV@.
  -> m ()
glMultiTexCoord3hNV v1 v2 v3 v4 = liftIO $ dyn581 ptr_glMultiTexCoord3hNV v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3hNV #-}
ptr_glMultiTexCoord3hNV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glMultiTexCoord3hNV = unsafePerformIO $ getCommand "glMultiTexCoord3hNV"

-- glMultiTexCoord3hvNV --------------------------------------------------------

glMultiTexCoord3hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glMultiTexCoord3hvNV v1 v2 = liftIO $ dyn571 ptr_glMultiTexCoord3hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord3hvNV #-}
ptr_glMultiTexCoord3hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord3hvNV = unsafePerformIO $ getCommand "glMultiTexCoord3hvNV"

-- glMultiTexCoord3i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3iv'.
glMultiTexCoord3i
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> m ()
glMultiTexCoord3i v1 v2 v3 v4 = liftIO $ dyn582 ptr_glMultiTexCoord3i v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3i #-}
ptr_glMultiTexCoord3i :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord3i = unsafePerformIO $ getCommand "glMultiTexCoord3i"

-- glMultiTexCoord3iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3iv'. This command is an alias for 'glMultiTexCoord3i'.
glMultiTexCoord3iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> m ()
glMultiTexCoord3iARB v1 v2 v3 v4 = liftIO $ dyn582 ptr_glMultiTexCoord3iARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3iARB #-}
ptr_glMultiTexCoord3iARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord3iARB = unsafePerformIO $ getCommand "glMultiTexCoord3iARB"

-- glMultiTexCoord3iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord3iv v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord3iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3iv #-}
ptr_glMultiTexCoord3iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord3iv = unsafePerformIO $ getCommand "glMultiTexCoord3iv"

-- glMultiTexCoord3ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3iv'.
glMultiTexCoord3ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord3ivARB v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord3ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3ivARB #-}
ptr_glMultiTexCoord3ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord3ivARB = unsafePerformIO $ getCommand "glMultiTexCoord3ivARB"

-- glMultiTexCoord3s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3sv'.
glMultiTexCoord3s
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> m ()
glMultiTexCoord3s v1 v2 v3 v4 = liftIO $ dyn583 ptr_glMultiTexCoord3s v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3s #-}
ptr_glMultiTexCoord3s :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord3s = unsafePerformIO $ getCommand "glMultiTexCoord3s"

-- glMultiTexCoord3sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3sv'. This command is an alias for 'glMultiTexCoord3s'.
glMultiTexCoord3sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> m ()
glMultiTexCoord3sARB v1 v2 v3 v4 = liftIO $ dyn583 ptr_glMultiTexCoord3sARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3sARB #-}
ptr_glMultiTexCoord3sARB :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord3sARB = unsafePerformIO $ getCommand "glMultiTexCoord3sARB"

-- glMultiTexCoord3sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord3sv v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord3sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3sv #-}
ptr_glMultiTexCoord3sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord3sv = unsafePerformIO $ getCommand "glMultiTexCoord3sv"

-- glMultiTexCoord3svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3sv'.
glMultiTexCoord3svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord3svARB v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord3svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3svARB #-}
ptr_glMultiTexCoord3svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord3svARB = unsafePerformIO $ getCommand "glMultiTexCoord3svARB"

-- glMultiTexCoord3xOES --------------------------------------------------------

glMultiTexCoord3xOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> m ()
glMultiTexCoord3xOES v1 v2 v3 v4 = liftIO $ dyn584 ptr_glMultiTexCoord3xOES v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3xOES #-}
ptr_glMultiTexCoord3xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord3xOES = unsafePerformIO $ getCommand "glMultiTexCoord3xOES"

