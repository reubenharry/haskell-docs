{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F16
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

module Graphics.GL.Functions.F16 (
  glLoadTransposeMatrixd,
  glLoadTransposeMatrixdARB,
  glLoadTransposeMatrixf,
  glLoadTransposeMatrixfARB,
  glLoadTransposeMatrixxOES,
  glLockArraysEXT,
  glLogicOp,
  glMakeBufferNonResidentNV,
  glMakeBufferResidentNV,
  glMakeImageHandleNonResidentARB,
  glMakeImageHandleNonResidentNV,
  glMakeImageHandleResidentARB,
  glMakeImageHandleResidentNV,
  glMakeNamedBufferNonResidentNV,
  glMakeNamedBufferResidentNV,
  glMakeTextureHandleNonResidentARB,
  glMakeTextureHandleNonResidentNV,
  glMakeTextureHandleResidentARB,
  glMakeTextureHandleResidentNV,
  glMap1d,
  glMap1f,
  glMap1xOES,
  glMap2d,
  glMap2f,
  glMap2xOES,
  glMapBuffer,
  glMapBufferARB,
  glMapBufferOES,
  glMapBufferRange,
  glMapBufferRangeEXT,
  glMapControlPointsNV,
  glMapGrid1d,
  glMapGrid1f,
  glMapGrid1xOES,
  glMapGrid2d,
  glMapGrid2f,
  glMapGrid2xOES,
  glMapNamedBuffer,
  glMapNamedBufferEXT,
  glMapNamedBufferRange,
  glMapNamedBufferRangeEXT,
  glMapObjectBufferATI,
  glMapParameterfvNV,
  glMapParameterivNV,
  glMapTexture2DINTEL,
  glMapVertexAttrib1dAPPLE,
  glMapVertexAttrib1fAPPLE,
  glMapVertexAttrib2dAPPLE,
  glMapVertexAttrib2fAPPLE,
  glMaterialf,
  glMaterialfv,
  glMateriali,
  glMaterialiv,
  glMaterialx,
  glMaterialxOES,
  glMaterialxv,
  glMaterialxvOES,
  glMatrixFrustumEXT,
  glMatrixIndexPointerARB,
  glMatrixIndexPointerOES,
  glMatrixIndexubvARB,
  glMatrixIndexuivARB,
  glMatrixIndexusvARB,
  glMatrixLoad3x2fNV,
  glMatrixLoad3x3fNV,
  glMatrixLoadIdentityEXT,
  glMatrixLoadTranspose3x3fNV,
  glMatrixLoadTransposedEXT,
  glMatrixLoadTransposefEXT,
  glMatrixLoaddEXT,
  glMatrixLoadfEXT,
  glMatrixMode,
  glMatrixMult3x2fNV,
  glMatrixMult3x3fNV,
  glMatrixMultTranspose3x3fNV,
  glMatrixMultTransposedEXT,
  glMatrixMultTransposefEXT,
  glMatrixMultdEXT,
  glMatrixMultfEXT,
  glMatrixOrthoEXT,
  glMatrixPopEXT,
  glMatrixPushEXT,
  glMatrixRotatedEXT,
  glMatrixRotatefEXT,
  glMatrixScaledEXT,
  glMatrixScalefEXT,
  glMatrixTranslatedEXT,
  glMatrixTranslatefEXT,
  glMaxShaderCompilerThreadsARB,
  glMaxShaderCompilerThreadsKHR,
  glMemoryBarrier,
  glMemoryBarrierByRegion,
  glMemoryBarrierEXT,
  glMemoryObjectParameterivEXT,
  glMinSampleShading,
  glMinSampleShadingARB,
  glMinSampleShadingOES,
  glMinmax,
  glMinmaxEXT,
  glMultMatrixd
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glLoadTransposeMatrixd ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadTransposeMatrix.xml OpenGL 2.x>.
glLoadTransposeMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glLoadTransposeMatrixd v1 = liftIO $ dyn42 ptr_glLoadTransposeMatrixd v1

{-# NOINLINE ptr_glLoadTransposeMatrixd #-}
ptr_glLoadTransposeMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glLoadTransposeMatrixd = unsafePerformIO $ getCommand "glLoadTransposeMatrixd"

-- glLoadTransposeMatrixdARB ---------------------------------------------------

-- | This command is an alias for 'glLoadTransposeMatrixd'.
glLoadTransposeMatrixdARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glLoadTransposeMatrixdARB v1 = liftIO $ dyn42 ptr_glLoadTransposeMatrixdARB v1

{-# NOINLINE ptr_glLoadTransposeMatrixdARB #-}
ptr_glLoadTransposeMatrixdARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glLoadTransposeMatrixdARB = unsafePerformIO $ getCommand "glLoadTransposeMatrixdARB"

-- glLoadTransposeMatrixf ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadTransposeMatrix.xml OpenGL 2.x>.
glLoadTransposeMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glLoadTransposeMatrixf v1 = liftIO $ dyn44 ptr_glLoadTransposeMatrixf v1

{-# NOINLINE ptr_glLoadTransposeMatrixf #-}
ptr_glLoadTransposeMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glLoadTransposeMatrixf = unsafePerformIO $ getCommand "glLoadTransposeMatrixf"

-- glLoadTransposeMatrixfARB ---------------------------------------------------

-- | This command is an alias for 'glLoadTransposeMatrixf'.
glLoadTransposeMatrixfARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glLoadTransposeMatrixfARB v1 = liftIO $ dyn44 ptr_glLoadTransposeMatrixfARB v1

{-# NOINLINE ptr_glLoadTransposeMatrixfARB #-}
ptr_glLoadTransposeMatrixfARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glLoadTransposeMatrixfARB = unsafePerformIO $ getCommand "glLoadTransposeMatrixfARB"

-- glLoadTransposeMatrixxOES ---------------------------------------------------

glLoadTransposeMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glLoadTransposeMatrixxOES v1 = liftIO $ dyn114 ptr_glLoadTransposeMatrixxOES v1

{-# NOINLINE ptr_glLoadTransposeMatrixxOES #-}
ptr_glLoadTransposeMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glLoadTransposeMatrixxOES = unsafePerformIO $ getCommand "glLoadTransposeMatrixxOES"

-- glLockArraysEXT -------------------------------------------------------------

glLockArraysEXT
  :: MonadIO m
  => GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glLockArraysEXT v1 v2 = liftIO $ dyn516 ptr_glLockArraysEXT v1 v2

{-# NOINLINE ptr_glLockArraysEXT #-}
ptr_glLockArraysEXT :: FunPtr (GLint -> GLsizei -> IO ())
ptr_glLockArraysEXT = unsafePerformIO $ getCommand "glLockArraysEXT"

-- glLogicOp -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glLogicOp.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glLogicOp.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glLogicOp.xhtml OpenGL 4.x>.
glLogicOp
  :: MonadIO m
  => GLenum -- ^ @opcode@ of type [LogicOp](Graphics-GL-Groups.html#LogicOp).
  -> m ()
glLogicOp v1 = liftIO $ dyn5 ptr_glLogicOp v1

{-# NOINLINE ptr_glLogicOp #-}
ptr_glLogicOp :: FunPtr (GLenum -> IO ())
ptr_glLogicOp = unsafePerformIO $ getCommand "glLogicOp"

-- glMakeBufferNonResidentNV ---------------------------------------------------

glMakeBufferNonResidentNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glMakeBufferNonResidentNV v1 = liftIO $ dyn5 ptr_glMakeBufferNonResidentNV v1

{-# NOINLINE ptr_glMakeBufferNonResidentNV #-}
ptr_glMakeBufferNonResidentNV :: FunPtr (GLenum -> IO ())
ptr_glMakeBufferNonResidentNV = unsafePerformIO $ getCommand "glMakeBufferNonResidentNV"

-- glMakeBufferResidentNV ------------------------------------------------------

glMakeBufferResidentNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeBufferResidentNV v1 v2 = liftIO $ dyn54 ptr_glMakeBufferResidentNV v1 v2

{-# NOINLINE ptr_glMakeBufferResidentNV #-}
ptr_glMakeBufferResidentNV :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glMakeBufferResidentNV = unsafePerformIO $ getCommand "glMakeBufferResidentNV"

-- glMakeImageHandleNonResidentARB ---------------------------------------------

glMakeImageHandleNonResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeImageHandleNonResidentARB v1 = liftIO $ dyn517 ptr_glMakeImageHandleNonResidentARB v1

{-# NOINLINE ptr_glMakeImageHandleNonResidentARB #-}
ptr_glMakeImageHandleNonResidentARB :: FunPtr (GLuint64 -> IO ())
ptr_glMakeImageHandleNonResidentARB = unsafePerformIO $ getCommand "glMakeImageHandleNonResidentARB"

-- glMakeImageHandleNonResidentNV ----------------------------------------------

glMakeImageHandleNonResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeImageHandleNonResidentNV v1 = liftIO $ dyn517 ptr_glMakeImageHandleNonResidentNV v1

{-# NOINLINE ptr_glMakeImageHandleNonResidentNV #-}
ptr_glMakeImageHandleNonResidentNV :: FunPtr (GLuint64 -> IO ())
ptr_glMakeImageHandleNonResidentNV = unsafePerformIO $ getCommand "glMakeImageHandleNonResidentNV"

-- glMakeImageHandleResidentARB ------------------------------------------------

glMakeImageHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeImageHandleResidentARB v1 v2 = liftIO $ dyn518 ptr_glMakeImageHandleResidentARB v1 v2

{-# NOINLINE ptr_glMakeImageHandleResidentARB #-}
ptr_glMakeImageHandleResidentARB :: FunPtr (GLuint64 -> GLenum -> IO ())
ptr_glMakeImageHandleResidentARB = unsafePerformIO $ getCommand "glMakeImageHandleResidentARB"

-- glMakeImageHandleResidentNV -------------------------------------------------

glMakeImageHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeImageHandleResidentNV v1 v2 = liftIO $ dyn518 ptr_glMakeImageHandleResidentNV v1 v2

{-# NOINLINE ptr_glMakeImageHandleResidentNV #-}
ptr_glMakeImageHandleResidentNV :: FunPtr (GLuint64 -> GLenum -> IO ())
ptr_glMakeImageHandleResidentNV = unsafePerformIO $ getCommand "glMakeImageHandleResidentNV"

-- glMakeNamedBufferNonResidentNV ----------------------------------------------

glMakeNamedBufferNonResidentNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m ()
glMakeNamedBufferNonResidentNV v1 = liftIO $ dyn3 ptr_glMakeNamedBufferNonResidentNV v1

{-# NOINLINE ptr_glMakeNamedBufferNonResidentNV #-}
ptr_glMakeNamedBufferNonResidentNV :: FunPtr (GLuint -> IO ())
ptr_glMakeNamedBufferNonResidentNV = unsafePerformIO $ getCommand "glMakeNamedBufferNonResidentNV"

-- glMakeNamedBufferResidentNV -------------------------------------------------

glMakeNamedBufferResidentNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeNamedBufferResidentNV v1 v2 = liftIO $ dyn18 ptr_glMakeNamedBufferResidentNV v1 v2

{-# NOINLINE ptr_glMakeNamedBufferResidentNV #-}
ptr_glMakeNamedBufferResidentNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glMakeNamedBufferResidentNV = unsafePerformIO $ getCommand "glMakeNamedBufferResidentNV"

-- glMakeTextureHandleNonResidentARB -------------------------------------------

glMakeTextureHandleNonResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleNonResidentARB v1 = liftIO $ dyn517 ptr_glMakeTextureHandleNonResidentARB v1

{-# NOINLINE ptr_glMakeTextureHandleNonResidentARB #-}
ptr_glMakeTextureHandleNonResidentARB :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleNonResidentARB = unsafePerformIO $ getCommand "glMakeTextureHandleNonResidentARB"

-- glMakeTextureHandleNonResidentNV --------------------------------------------

glMakeTextureHandleNonResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleNonResidentNV v1 = liftIO $ dyn517 ptr_glMakeTextureHandleNonResidentNV v1

{-# NOINLINE ptr_glMakeTextureHandleNonResidentNV #-}
ptr_glMakeTextureHandleNonResidentNV :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleNonResidentNV = unsafePerformIO $ getCommand "glMakeTextureHandleNonResidentNV"

-- glMakeTextureHandleResidentARB ----------------------------------------------

glMakeTextureHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleResidentARB v1 = liftIO $ dyn517 ptr_glMakeTextureHandleResidentARB v1

{-# NOINLINE ptr_glMakeTextureHandleResidentARB #-}
ptr_glMakeTextureHandleResidentARB :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleResidentARB = unsafePerformIO $ getCommand "glMakeTextureHandleResidentARB"

-- glMakeTextureHandleResidentNV -----------------------------------------------

glMakeTextureHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleResidentNV v1 = liftIO $ dyn517 ptr_glMakeTextureHandleResidentNV v1

{-# NOINLINE ptr_glMakeTextureHandleResidentNV #-}
ptr_glMakeTextureHandleResidentNV :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleResidentNV = unsafePerformIO $ getCommand "glMakeTextureHandleResidentNV"

-- glMap1d ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap1.xml OpenGL 2.x>.
glMap1d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(target,stride,order)@ elements of type @CoordD@.
  -> m ()
glMap1d v1 v2 v3 v4 v5 v6 = liftIO $ dyn519 ptr_glMap1d v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMap1d #-}
ptr_glMap1d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMap1d = unsafePerformIO $ getCommand "glMap1d"

-- glMap1f ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap1.xml OpenGL 2.x>.
glMap1f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target,stride,order)@ elements of type @CoordF@.
  -> m ()
glMap1f v1 v2 v3 v4 v5 v6 = liftIO $ dyn520 ptr_glMap1f v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMap1f #-}
ptr_glMap1f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMap1f = unsafePerformIO $ getCommand "glMap1f"

-- glMap1xOES ------------------------------------------------------------------

glMap1xOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@.
  -> GLfixed -- ^ @points@.
  -> m ()
glMap1xOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn521 ptr_glMap1xOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMap1xOES #-}
ptr_glMap1xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ())
ptr_glMap1xOES = unsafePerformIO $ getCommand "glMap1xOES"

-- glMap2d ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap2.xml OpenGL 2.x>.
glMap2d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder)@ elements of type @CoordD@.
  -> m ()
glMap2d v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn522 ptr_glMap2d v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMap2d #-}
ptr_glMap2d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMap2d = unsafePerformIO $ getCommand "glMap2d"

-- glMap2f ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap2.xml OpenGL 2.x>.
glMap2f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder)@ elements of type @CoordF@.
  -> m ()
glMap2f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn523 ptr_glMap2f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMap2f #-}
ptr_glMap2f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMap2f = unsafePerformIO $ getCommand "glMap2f"

-- glMap2xOES ------------------------------------------------------------------

glMap2xOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@.
  -> GLfixed -- ^ @v1@.
  -> GLfixed -- ^ @v2@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@.
  -> GLfixed -- ^ @points@.
  -> m ()
glMap2xOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn524 ptr_glMap2xOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMap2xOES #-}
ptr_glMap2xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ())
ptr_glMap2xOES = unsafePerformIO $ getCommand "glMap2xOES"

-- glMapBuffer -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMapBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMapBuffer.xhtml OpenGL 4.x>.
glMapBuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @access@ of type [BufferAccessARB](Graphics-GL-Groups.html#BufferAccessARB).
  -> m (Ptr a)
glMapBuffer v1 v2 = liftIO $ dyn525 ptr_glMapBuffer v1 v2

{-# NOINLINE ptr_glMapBuffer #-}
ptr_glMapBuffer :: FunPtr (GLenum -> GLenum -> IO (Ptr a))
ptr_glMapBuffer = unsafePerformIO $ getCommand "glMapBuffer"

-- glMapBufferARB --------------------------------------------------------------

-- | This command is an alias for 'glMapBuffer'.
glMapBufferARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @access@ of type [BufferAccessARB](Graphics-GL-Groups.html#BufferAccessARB).
  -> m (Ptr a)
glMapBufferARB v1 v2 = liftIO $ dyn525 ptr_glMapBufferARB v1 v2

{-# NOINLINE ptr_glMapBufferARB #-}
ptr_glMapBufferARB :: FunPtr (GLenum -> GLenum -> IO (Ptr a))
ptr_glMapBufferARB = unsafePerformIO $ getCommand "glMapBufferARB"

-- glMapBufferOES --------------------------------------------------------------

-- | This command is an alias for 'glMapBuffer'.
glMapBufferOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLenum -- ^ @access@ of type [BufferAccessARB](Graphics-GL-Groups.html#BufferAccessARB).
  -> m (Ptr a)
glMapBufferOES v1 v2 = liftIO $ dyn525 ptr_glMapBufferOES v1 v2

{-# NOINLINE ptr_glMapBufferOES #-}
ptr_glMapBufferOES :: FunPtr (GLenum -> GLenum -> IO (Ptr a))
ptr_glMapBufferOES = unsafePerformIO $ getCommand "glMapBufferOES"

-- glMapBufferRange ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glMapBufferRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMapBufferRange.xhtml OpenGL 4.x>.
glMapBufferRange
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> GLbitfield -- ^ @access@ of type [MapBufferAccessMask](Graphics-GL-Groups.html#MapBufferAccessMask).
  -> m (Ptr a)
glMapBufferRange v1 v2 v3 v4 = liftIO $ dyn526 ptr_glMapBufferRange v1 v2 v3 v4

{-# NOINLINE ptr_glMapBufferRange #-}
ptr_glMapBufferRange :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapBufferRange = unsafePerformIO $ getCommand "glMapBufferRange"

-- glMapBufferRangeEXT ---------------------------------------------------------

-- | This command is an alias for 'glMapBufferRange'.
glMapBufferRangeEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> GLbitfield -- ^ @access@ of type [MapBufferAccessMask](Graphics-GL-Groups.html#MapBufferAccessMask).
  -> m (Ptr a)
glMapBufferRangeEXT v1 v2 v3 v4 = liftIO $ dyn526 ptr_glMapBufferRangeEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMapBufferRangeEXT #-}
ptr_glMapBufferRangeEXT :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapBufferRangeEXT = unsafePerformIO $ getCommand "glMapBufferRangeEXT"

-- glMapControlPointsNV --------------------------------------------------------

glMapControlPointsNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EvalTargetNV](Graphics-GL-Groups.html#EvalTargetNV).
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [MapTypeNV](Graphics-GL-Groups.html#MapTypeNV).
  -> GLsizei -- ^ @ustride@.
  -> GLsizei -- ^ @vstride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> GLboolean -- ^ @packed@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr a -- ^ @points@ pointing to @COMPSIZE(target,uorder,vorder)@ elements of type @a@.
  -> m ()
glMapControlPointsNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn527 ptr_glMapControlPointsNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glMapControlPointsNV #-}
ptr_glMapControlPointsNV :: FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr a -> IO ())
ptr_glMapControlPointsNV = unsafePerformIO $ getCommand "glMapControlPointsNV"

-- glMapGrid1d -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid1d
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> m ()
glMapGrid1d v1 v2 v3 = liftIO $ dyn528 ptr_glMapGrid1d v1 v2 v3

{-# NOINLINE ptr_glMapGrid1d #-}
ptr_glMapGrid1d :: FunPtr (GLint -> GLdouble -> GLdouble -> IO ())
ptr_glMapGrid1d = unsafePerformIO $ getCommand "glMapGrid1d"

-- glMapGrid1f -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid1f
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> m ()
glMapGrid1f v1 v2 v3 = liftIO $ dyn529 ptr_glMapGrid1f v1 v2 v3

{-# NOINLINE ptr_glMapGrid1f #-}
ptr_glMapGrid1f :: FunPtr (GLint -> GLfloat -> GLfloat -> IO ())
ptr_glMapGrid1f = unsafePerformIO $ getCommand "glMapGrid1f"

-- glMapGrid1xOES --------------------------------------------------------------

glMapGrid1xOES
  :: MonadIO m
  => GLint -- ^ @n@.
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> m ()
glMapGrid1xOES v1 v2 v3 = liftIO $ dyn530 ptr_glMapGrid1xOES v1 v2 v3

{-# NOINLINE ptr_glMapGrid1xOES #-}
ptr_glMapGrid1xOES :: FunPtr (GLint -> GLfixed -> GLfixed -> IO ())
ptr_glMapGrid1xOES = unsafePerformIO $ getCommand "glMapGrid1xOES"

-- glMapGrid2d -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid2d
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @vn@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> m ()
glMapGrid2d v1 v2 v3 v4 v5 v6 = liftIO $ dyn531 ptr_glMapGrid2d v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMapGrid2d #-}
ptr_glMapGrid2d :: FunPtr (GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ())
ptr_glMapGrid2d = unsafePerformIO $ getCommand "glMapGrid2d"

-- glMapGrid2f -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid2f
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @vn@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> m ()
glMapGrid2f v1 v2 v3 v4 v5 v6 = liftIO $ dyn532 ptr_glMapGrid2f v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMapGrid2f #-}
ptr_glMapGrid2f :: FunPtr (GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ())
ptr_glMapGrid2f = unsafePerformIO $ getCommand "glMapGrid2f"

-- glMapGrid2xOES --------------------------------------------------------------

glMapGrid2xOES
  :: MonadIO m
  => GLint -- ^ @n@.
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> GLfixed -- ^ @v1@.
  -> GLfixed -- ^ @v2@.
  -> m ()
glMapGrid2xOES v1 v2 v3 v4 v5 = liftIO $ dyn533 ptr_glMapGrid2xOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMapGrid2xOES #-}
ptr_glMapGrid2xOES :: FunPtr (GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMapGrid2xOES = unsafePerformIO $ getCommand "glMapGrid2xOES"

-- glMapNamedBuffer ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMapBuffer.xhtml OpenGL 4.x>.
glMapNamedBuffer
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @access@ of type [BufferAccessARB](Graphics-GL-Groups.html#BufferAccessARB).
  -> m (Ptr a)
glMapNamedBuffer v1 v2 = liftIO $ dyn534 ptr_glMapNamedBuffer v1 v2

{-# NOINLINE ptr_glMapNamedBuffer #-}
ptr_glMapNamedBuffer :: FunPtr (GLuint -> GLenum -> IO (Ptr a))
ptr_glMapNamedBuffer = unsafePerformIO $ getCommand "glMapNamedBuffer"

-- glMapNamedBufferEXT ---------------------------------------------------------

glMapNamedBufferEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @access@ of type [BufferAccessARB](Graphics-GL-Groups.html#BufferAccessARB).
  -> m (Ptr a)
glMapNamedBufferEXT v1 v2 = liftIO $ dyn534 ptr_glMapNamedBufferEXT v1 v2

{-# NOINLINE ptr_glMapNamedBufferEXT #-}
ptr_glMapNamedBufferEXT :: FunPtr (GLuint -> GLenum -> IO (Ptr a))
ptr_glMapNamedBufferEXT = unsafePerformIO $ getCommand "glMapNamedBufferEXT"

-- glMapNamedBufferRange -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMapBufferRange.xhtml OpenGL 4.x>.
glMapNamedBufferRange
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> GLbitfield -- ^ @access@ of type [MapBufferAccessMask](Graphics-GL-Groups.html#MapBufferAccessMask).
  -> m (Ptr a)
glMapNamedBufferRange v1 v2 v3 v4 = liftIO $ dyn535 ptr_glMapNamedBufferRange v1 v2 v3 v4

{-# NOINLINE ptr_glMapNamedBufferRange #-}
ptr_glMapNamedBufferRange :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapNamedBufferRange = unsafePerformIO $ getCommand "glMapNamedBufferRange"

-- glMapNamedBufferRangeEXT ----------------------------------------------------

glMapNamedBufferRangeEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> GLbitfield -- ^ @access@ of type [MapBufferAccessMask](Graphics-GL-Groups.html#MapBufferAccessMask).
  -> m (Ptr a)
glMapNamedBufferRangeEXT v1 v2 v3 v4 = liftIO $ dyn535 ptr_glMapNamedBufferRangeEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMapNamedBufferRangeEXT #-}
ptr_glMapNamedBufferRangeEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapNamedBufferRangeEXT = unsafePerformIO $ getCommand "glMapNamedBufferRangeEXT"

-- glMapObjectBufferATI --------------------------------------------------------

glMapObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m (Ptr a)
glMapObjectBufferATI v1 = liftIO $ dyn536 ptr_glMapObjectBufferATI v1

{-# NOINLINE ptr_glMapObjectBufferATI #-}
ptr_glMapObjectBufferATI :: FunPtr (GLuint -> IO (Ptr a))
ptr_glMapObjectBufferATI = unsafePerformIO $ getCommand "glMapObjectBufferATI"

-- glMapParameterfvNV ----------------------------------------------------------

glMapParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EvalTargetNV](Graphics-GL-Groups.html#EvalTargetNV).
  -> GLenum -- ^ @pname@ of type [MapParameterNV](Graphics-GL-Groups.html#MapParameterNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(target,pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMapParameterfvNV v1 v2 v3 = liftIO $ dyn139 ptr_glMapParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glMapParameterfvNV #-}
ptr_glMapParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMapParameterfvNV = unsafePerformIO $ getCommand "glMapParameterfvNV"

-- glMapParameterivNV ----------------------------------------------------------

glMapParameterivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EvalTargetNV](Graphics-GL-Groups.html#EvalTargetNV).
  -> GLenum -- ^ @pname@ of type [MapParameterNV](Graphics-GL-Groups.html#MapParameterNV).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(target,pname)@ elements of type @CheckedInt32@.
  -> m ()
glMapParameterivNV v1 v2 v3 = liftIO $ dyn140 ptr_glMapParameterivNV v1 v2 v3

{-# NOINLINE ptr_glMapParameterivNV #-}
ptr_glMapParameterivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMapParameterivNV = unsafePerformIO $ getCommand "glMapParameterivNV"

-- glMapTexture2DINTEL ---------------------------------------------------------

glMapTexture2DINTEL
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLbitfield -- ^ @access@.
  -> Ptr GLint -- ^ @stride@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @layout@ pointing to @1@ element of type @GLenum@.
  -> m (Ptr a)
glMapTexture2DINTEL v1 v2 v3 v4 v5 = liftIO $ dyn537 ptr_glMapTexture2DINTEL v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMapTexture2DINTEL #-}
ptr_glMapTexture2DINTEL :: FunPtr (GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> IO (Ptr a))
ptr_glMapTexture2DINTEL = unsafePerformIO $ getCommand "glMapTexture2DINTEL"

-- glMapVertexAttrib1dAPPLE ----------------------------------------------------

glMapVertexAttrib1dAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(size,stride,order)@ elements of type @CoordD@.
  -> m ()
glMapVertexAttrib1dAPPLE v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn538 ptr_glMapVertexAttrib1dAPPLE v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMapVertexAttrib1dAPPLE #-}
ptr_glMapVertexAttrib1dAPPLE :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMapVertexAttrib1dAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib1dAPPLE"

-- glMapVertexAttrib1fAPPLE ----------------------------------------------------

glMapVertexAttrib1fAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(size,stride,order)@ elements of type @CoordF@.
  -> m ()
glMapVertexAttrib1fAPPLE v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn539 ptr_glMapVertexAttrib1fAPPLE v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMapVertexAttrib1fAPPLE #-}
ptr_glMapVertexAttrib1fAPPLE :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMapVertexAttrib1fAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib1fAPPLE"

-- glMapVertexAttrib2dAPPLE ----------------------------------------------------

glMapVertexAttrib2dAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(size,ustride,uorder,vstride,vorder)@ elements of type @CoordD@.
  -> m ()
glMapVertexAttrib2dAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn540 ptr_glMapVertexAttrib2dAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glMapVertexAttrib2dAPPLE #-}
ptr_glMapVertexAttrib2dAPPLE :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMapVertexAttrib2dAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib2dAPPLE"

-- glMapVertexAttrib2fAPPLE ----------------------------------------------------

glMapVertexAttrib2fAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(size,ustride,uorder,vstride,vorder)@ elements of type @CoordF@.
  -> m ()
glMapVertexAttrib2fAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn541 ptr_glMapVertexAttrib2fAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glMapVertexAttrib2fAPPLE #-}
ptr_glMapVertexAttrib2fAPPLE :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMapVertexAttrib2fAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib2fAPPLE"

-- glMaterialf -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMaterialf
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMaterialf v1 v2 v3 = liftIO $ dyn168 ptr_glMaterialf v1 v2 v3

{-# NOINLINE ptr_glMaterialf #-}
ptr_glMaterialf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMaterialf = unsafePerformIO $ getCommand "glMaterialf"

-- glMaterialfv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMaterialfv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMaterialfv v1 v2 v3 = liftIO $ dyn139 ptr_glMaterialfv v1 v2 v3

{-# NOINLINE ptr_glMaterialfv #-}
ptr_glMaterialfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMaterialfv = unsafePerformIO $ getCommand "glMaterialfv"

-- glMateriali -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMateriali
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMateriali v1 v2 v3 = liftIO $ dyn66 ptr_glMateriali v1 v2 v3

{-# NOINLINE ptr_glMateriali #-}
ptr_glMateriali :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glMateriali = unsafePerformIO $ getCommand "glMateriali"

-- glMaterialiv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMaterialiv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMaterialiv v1 v2 v3 = liftIO $ dyn140 ptr_glMaterialiv v1 v2 v3

{-# NOINLINE ptr_glMaterialiv #-}
ptr_glMaterialiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMaterialiv = unsafePerformIO $ getCommand "glMaterialiv"

-- glMaterialx -----------------------------------------------------------------

glMaterialx
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glMaterialx v1 v2 v3 = liftIO $ dyn169 ptr_glMaterialx v1 v2 v3

{-# NOINLINE ptr_glMaterialx #-}
ptr_glMaterialx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glMaterialx = unsafePerformIO $ getCommand "glMaterialx"

-- glMaterialxOES --------------------------------------------------------------

glMaterialxOES
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glMaterialxOES v1 v2 v3 = liftIO $ dyn169 ptr_glMaterialxOES v1 v2 v3

{-# NOINLINE ptr_glMaterialxOES #-}
ptr_glMaterialxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glMaterialxOES = unsafePerformIO $ getCommand "glMaterialxOES"

-- glMaterialxv ----------------------------------------------------------------

glMaterialxv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glMaterialxv v1 v2 v3 = liftIO $ dyn170 ptr_glMaterialxv v1 v2 v3

{-# NOINLINE ptr_glMaterialxv #-}
ptr_glMaterialxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glMaterialxv = unsafePerformIO $ getCommand "glMaterialxv"

-- glMaterialxvOES -------------------------------------------------------------

glMaterialxvOES
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glMaterialxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glMaterialxvOES v1 v2 v3

{-# NOINLINE ptr_glMaterialxvOES #-}
ptr_glMaterialxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glMaterialxvOES = unsafePerformIO $ getCommand "glMaterialxvOES"

-- glMatrixFrustumEXT ----------------------------------------------------------

glMatrixFrustumEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glMatrixFrustumEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn542 ptr_glMatrixFrustumEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMatrixFrustumEXT #-}
ptr_glMatrixFrustumEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixFrustumEXT = unsafePerformIO $ getCommand "glMatrixFrustumEXT"

-- glMatrixIndexPointerARB -----------------------------------------------------

glMatrixIndexPointerARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [MatrixIndexPointerTypeARB](Graphics-GL-Groups.html#MatrixIndexPointerTypeARB).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glMatrixIndexPointerARB v1 v2 v3 v4 = liftIO $ dyn133 ptr_glMatrixIndexPointerARB v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixIndexPointerARB #-}
ptr_glMatrixIndexPointerARB :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glMatrixIndexPointerARB = unsafePerformIO $ getCommand "glMatrixIndexPointerARB"

-- glMatrixIndexPointerOES -----------------------------------------------------

glMatrixIndexPointerOES
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [MatrixIndexPointerTypeARB](Graphics-GL-Groups.html#MatrixIndexPointerTypeARB).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glMatrixIndexPointerOES v1 v2 v3 v4 = liftIO $ dyn133 ptr_glMatrixIndexPointerOES v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixIndexPointerOES #-}
ptr_glMatrixIndexPointerOES :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glMatrixIndexPointerOES = unsafePerformIO $ getCommand "glMatrixIndexPointerOES"

-- glMatrixIndexubvARB ---------------------------------------------------------

glMatrixIndexubvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLubyte -- ^ @indices@ pointing to @size@ elements of type @GLubyte@.
  -> m ()
glMatrixIndexubvARB v1 v2 = liftIO $ dyn543 ptr_glMatrixIndexubvARB v1 v2

{-# NOINLINE ptr_glMatrixIndexubvARB #-}
ptr_glMatrixIndexubvARB :: FunPtr (GLint -> Ptr GLubyte -> IO ())
ptr_glMatrixIndexubvARB = unsafePerformIO $ getCommand "glMatrixIndexubvARB"

-- glMatrixIndexuivARB ---------------------------------------------------------

glMatrixIndexuivARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLuint -- ^ @indices@ pointing to @size@ elements of type @GLuint@.
  -> m ()
glMatrixIndexuivARB v1 v2 = liftIO $ dyn544 ptr_glMatrixIndexuivARB v1 v2

{-# NOINLINE ptr_glMatrixIndexuivARB #-}
ptr_glMatrixIndexuivARB :: FunPtr (GLint -> Ptr GLuint -> IO ())
ptr_glMatrixIndexuivARB = unsafePerformIO $ getCommand "glMatrixIndexuivARB"

-- glMatrixIndexusvARB ---------------------------------------------------------

glMatrixIndexusvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLushort -- ^ @indices@ pointing to @size@ elements of type @GLushort@.
  -> m ()
glMatrixIndexusvARB v1 v2 = liftIO $ dyn545 ptr_glMatrixIndexusvARB v1 v2

{-# NOINLINE ptr_glMatrixIndexusvARB #-}
ptr_glMatrixIndexusvARB :: FunPtr (GLint -> Ptr GLushort -> IO ())
ptr_glMatrixIndexusvARB = unsafePerformIO $ getCommand "glMatrixIndexusvARB"

-- glMatrixLoad3x2fNV ----------------------------------------------------------

glMatrixLoad3x2fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixLoad3x2fNV v1 v2 = liftIO $ dyn101 ptr_glMatrixLoad3x2fNV v1 v2

{-# NOINLINE ptr_glMatrixLoad3x2fNV #-}
ptr_glMatrixLoad3x2fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoad3x2fNV = unsafePerformIO $ getCommand "glMatrixLoad3x2fNV"

-- glMatrixLoad3x3fNV ----------------------------------------------------------

glMatrixLoad3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixLoad3x3fNV v1 v2 = liftIO $ dyn101 ptr_glMatrixLoad3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixLoad3x3fNV #-}
ptr_glMatrixLoad3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoad3x3fNV = unsafePerformIO $ getCommand "glMatrixLoad3x3fNV"

-- glMatrixLoadIdentityEXT -----------------------------------------------------

glMatrixLoadIdentityEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixLoadIdentityEXT v1 = liftIO $ dyn5 ptr_glMatrixLoadIdentityEXT v1

{-# NOINLINE ptr_glMatrixLoadIdentityEXT #-}
ptr_glMatrixLoadIdentityEXT :: FunPtr (GLenum -> IO ())
ptr_glMatrixLoadIdentityEXT = unsafePerformIO $ getCommand "glMatrixLoadIdentityEXT"

-- glMatrixLoadTranspose3x3fNV -------------------------------------------------

glMatrixLoadTranspose3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixLoadTranspose3x3fNV v1 v2 = liftIO $ dyn101 ptr_glMatrixLoadTranspose3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixLoadTranspose3x3fNV #-}
ptr_glMatrixLoadTranspose3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoadTranspose3x3fNV = unsafePerformIO $ getCommand "glMatrixLoadTranspose3x3fNV"

-- glMatrixLoadTransposedEXT ---------------------------------------------------

glMatrixLoadTransposedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixLoadTransposedEXT v1 v2 = liftIO $ dyn100 ptr_glMatrixLoadTransposedEXT v1 v2

{-# NOINLINE ptr_glMatrixLoadTransposedEXT #-}
ptr_glMatrixLoadTransposedEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixLoadTransposedEXT = unsafePerformIO $ getCommand "glMatrixLoadTransposedEXT"

-- glMatrixLoadTransposefEXT ---------------------------------------------------

glMatrixLoadTransposefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixLoadTransposefEXT v1 v2 = liftIO $ dyn101 ptr_glMatrixLoadTransposefEXT v1 v2

{-# NOINLINE ptr_glMatrixLoadTransposefEXT #-}
ptr_glMatrixLoadTransposefEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoadTransposefEXT = unsafePerformIO $ getCommand "glMatrixLoadTransposefEXT"

-- glMatrixLoaddEXT ------------------------------------------------------------

glMatrixLoaddEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixLoaddEXT v1 v2 = liftIO $ dyn100 ptr_glMatrixLoaddEXT v1 v2

{-# NOINLINE ptr_glMatrixLoaddEXT #-}
ptr_glMatrixLoaddEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixLoaddEXT = unsafePerformIO $ getCommand "glMatrixLoaddEXT"

-- glMatrixLoadfEXT ------------------------------------------------------------

glMatrixLoadfEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixLoadfEXT v1 v2 = liftIO $ dyn101 ptr_glMatrixLoadfEXT v1 v2

{-# NOINLINE ptr_glMatrixLoadfEXT #-}
ptr_glMatrixLoadfEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoadfEXT = unsafePerformIO $ getCommand "glMatrixLoadfEXT"

-- glMatrixMode ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMatrixMode.xml OpenGL 2.x>.
glMatrixMode
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixMode v1 = liftIO $ dyn5 ptr_glMatrixMode v1

{-# NOINLINE ptr_glMatrixMode #-}
ptr_glMatrixMode :: FunPtr (GLenum -> IO ())
ptr_glMatrixMode = unsafePerformIO $ getCommand "glMatrixMode"

-- glMatrixMult3x2fNV ----------------------------------------------------------

glMatrixMult3x2fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixMult3x2fNV v1 v2 = liftIO $ dyn101 ptr_glMatrixMult3x2fNV v1 v2

{-# NOINLINE ptr_glMatrixMult3x2fNV #-}
ptr_glMatrixMult3x2fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMult3x2fNV = unsafePerformIO $ getCommand "glMatrixMult3x2fNV"

-- glMatrixMult3x3fNV ----------------------------------------------------------

glMatrixMult3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixMult3x3fNV v1 v2 = liftIO $ dyn101 ptr_glMatrixMult3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixMult3x3fNV #-}
ptr_glMatrixMult3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMult3x3fNV = unsafePerformIO $ getCommand "glMatrixMult3x3fNV"

-- glMatrixMultTranspose3x3fNV -------------------------------------------------

glMatrixMultTranspose3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixMultTranspose3x3fNV v1 v2 = liftIO $ dyn101 ptr_glMatrixMultTranspose3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixMultTranspose3x3fNV #-}
ptr_glMatrixMultTranspose3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMultTranspose3x3fNV = unsafePerformIO $ getCommand "glMatrixMultTranspose3x3fNV"

-- glMatrixMultTransposedEXT ---------------------------------------------------

glMatrixMultTransposedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixMultTransposedEXT v1 v2 = liftIO $ dyn100 ptr_glMatrixMultTransposedEXT v1 v2

{-# NOINLINE ptr_glMatrixMultTransposedEXT #-}
ptr_glMatrixMultTransposedEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixMultTransposedEXT = unsafePerformIO $ getCommand "glMatrixMultTransposedEXT"

-- glMatrixMultTransposefEXT ---------------------------------------------------

glMatrixMultTransposefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixMultTransposefEXT v1 v2 = liftIO $ dyn101 ptr_glMatrixMultTransposefEXT v1 v2

{-# NOINLINE ptr_glMatrixMultTransposefEXT #-}
ptr_glMatrixMultTransposefEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMultTransposefEXT = unsafePerformIO $ getCommand "glMatrixMultTransposefEXT"

-- glMatrixMultdEXT ------------------------------------------------------------

glMatrixMultdEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixMultdEXT v1 v2 = liftIO $ dyn100 ptr_glMatrixMultdEXT v1 v2

{-# NOINLINE ptr_glMatrixMultdEXT #-}
ptr_glMatrixMultdEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixMultdEXT = unsafePerformIO $ getCommand "glMatrixMultdEXT"

-- glMatrixMultfEXT ------------------------------------------------------------

glMatrixMultfEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixMultfEXT v1 v2 = liftIO $ dyn101 ptr_glMatrixMultfEXT v1 v2

{-# NOINLINE ptr_glMatrixMultfEXT #-}
ptr_glMatrixMultfEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMultfEXT = unsafePerformIO $ getCommand "glMatrixMultfEXT"

-- glMatrixOrthoEXT ------------------------------------------------------------

glMatrixOrthoEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glMatrixOrthoEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn542 ptr_glMatrixOrthoEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMatrixOrthoEXT #-}
ptr_glMatrixOrthoEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixOrthoEXT = unsafePerformIO $ getCommand "glMatrixOrthoEXT"

-- glMatrixPopEXT --------------------------------------------------------------

glMatrixPopEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixPopEXT v1 = liftIO $ dyn5 ptr_glMatrixPopEXT v1

{-# NOINLINE ptr_glMatrixPopEXT #-}
ptr_glMatrixPopEXT :: FunPtr (GLenum -> IO ())
ptr_glMatrixPopEXT = unsafePerformIO $ getCommand "glMatrixPopEXT"

-- glMatrixPushEXT -------------------------------------------------------------

glMatrixPushEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixPushEXT v1 = liftIO $ dyn5 ptr_glMatrixPushEXT v1

{-# NOINLINE ptr_glMatrixPushEXT #-}
ptr_glMatrixPushEXT :: FunPtr (GLenum -> IO ())
ptr_glMatrixPushEXT = unsafePerformIO $ getCommand "glMatrixPushEXT"

-- glMatrixRotatedEXT ----------------------------------------------------------

glMatrixRotatedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @angle@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glMatrixRotatedEXT v1 v2 v3 v4 v5 = liftIO $ dyn546 ptr_glMatrixRotatedEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMatrixRotatedEXT #-}
ptr_glMatrixRotatedEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixRotatedEXT = unsafePerformIO $ getCommand "glMatrixRotatedEXT"

-- glMatrixRotatefEXT ----------------------------------------------------------

glMatrixRotatefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLfloat -- ^ @angle@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glMatrixRotatefEXT v1 v2 v3 v4 v5 = liftIO $ dyn547 ptr_glMatrixRotatefEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMatrixRotatefEXT #-}
ptr_glMatrixRotatefEXT :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMatrixRotatefEXT = unsafePerformIO $ getCommand "glMatrixRotatefEXT"

-- glMatrixScaledEXT -----------------------------------------------------------

glMatrixScaledEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glMatrixScaledEXT v1 v2 v3 v4 = liftIO $ dyn548 ptr_glMatrixScaledEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixScaledEXT #-}
ptr_glMatrixScaledEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixScaledEXT = unsafePerformIO $ getCommand "glMatrixScaledEXT"

-- glMatrixScalefEXT -----------------------------------------------------------

glMatrixScalefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glMatrixScalefEXT v1 v2 v3 v4 = liftIO $ dyn549 ptr_glMatrixScalefEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixScalefEXT #-}
ptr_glMatrixScalefEXT :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMatrixScalefEXT = unsafePerformIO $ getCommand "glMatrixScalefEXT"

-- glMatrixTranslatedEXT -------------------------------------------------------

glMatrixTranslatedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glMatrixTranslatedEXT v1 v2 v3 v4 = liftIO $ dyn548 ptr_glMatrixTranslatedEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixTranslatedEXT #-}
ptr_glMatrixTranslatedEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixTranslatedEXT = unsafePerformIO $ getCommand "glMatrixTranslatedEXT"

-- glMatrixTranslatefEXT -------------------------------------------------------

glMatrixTranslatefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glMatrixTranslatefEXT v1 v2 v3 v4 = liftIO $ dyn549 ptr_glMatrixTranslatefEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixTranslatefEXT #-}
ptr_glMatrixTranslatefEXT :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMatrixTranslatefEXT = unsafePerformIO $ getCommand "glMatrixTranslatefEXT"

-- glMaxShaderCompilerThreadsARB -----------------------------------------------

-- | This command is an alias for 'glMaxShaderCompilerThreadsKHR'.
glMaxShaderCompilerThreadsARB
  :: MonadIO m
  => GLuint -- ^ @count@.
  -> m ()
glMaxShaderCompilerThreadsARB v1 = liftIO $ dyn3 ptr_glMaxShaderCompilerThreadsARB v1

{-# NOINLINE ptr_glMaxShaderCompilerThreadsARB #-}
ptr_glMaxShaderCompilerThreadsARB :: FunPtr (GLuint -> IO ())
ptr_glMaxShaderCompilerThreadsARB = unsafePerformIO $ getCommand "glMaxShaderCompilerThreadsARB"

-- glMaxShaderCompilerThreadsKHR -----------------------------------------------

glMaxShaderCompilerThreadsKHR
  :: MonadIO m
  => GLuint -- ^ @count@.
  -> m ()
glMaxShaderCompilerThreadsKHR v1 = liftIO $ dyn3 ptr_glMaxShaderCompilerThreadsKHR v1

{-# NOINLINE ptr_glMaxShaderCompilerThreadsKHR #-}
ptr_glMaxShaderCompilerThreadsKHR :: FunPtr (GLuint -> IO ())
ptr_glMaxShaderCompilerThreadsKHR = unsafePerformIO $ getCommand "glMaxShaderCompilerThreadsKHR"

-- glMemoryBarrier -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMemoryBarrier.xhtml OpenGL 4.x>.
glMemoryBarrier
  :: MonadIO m
  => GLbitfield -- ^ @barriers@ of type [MemoryBarrierMask](Graphics-GL-Groups.html#MemoryBarrierMask).
  -> m ()
glMemoryBarrier v1 = liftIO $ dyn75 ptr_glMemoryBarrier v1

{-# NOINLINE ptr_glMemoryBarrier #-}
ptr_glMemoryBarrier :: FunPtr (GLbitfield -> IO ())
ptr_glMemoryBarrier = unsafePerformIO $ getCommand "glMemoryBarrier"

-- glMemoryBarrierByRegion -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMemoryBarrier.xhtml OpenGL 4.x>.
glMemoryBarrierByRegion
  :: MonadIO m
  => GLbitfield -- ^ @barriers@ of type [MemoryBarrierMask](Graphics-GL-Groups.html#MemoryBarrierMask).
  -> m ()
glMemoryBarrierByRegion v1 = liftIO $ dyn75 ptr_glMemoryBarrierByRegion v1

{-# NOINLINE ptr_glMemoryBarrierByRegion #-}
ptr_glMemoryBarrierByRegion :: FunPtr (GLbitfield -> IO ())
ptr_glMemoryBarrierByRegion = unsafePerformIO $ getCommand "glMemoryBarrierByRegion"

-- glMemoryBarrierEXT ----------------------------------------------------------

-- | This command is an alias for 'glMemoryBarrier'.
glMemoryBarrierEXT
  :: MonadIO m
  => GLbitfield -- ^ @barriers@ of type [MemoryBarrierMask](Graphics-GL-Groups.html#MemoryBarrierMask).
  -> m ()
glMemoryBarrierEXT v1 = liftIO $ dyn75 ptr_glMemoryBarrierEXT v1

{-# NOINLINE ptr_glMemoryBarrierEXT #-}
ptr_glMemoryBarrierEXT :: FunPtr (GLbitfield -> IO ())
ptr_glMemoryBarrierEXT = unsafePerformIO $ getCommand "glMemoryBarrierEXT"

-- glMemoryObjectParameterivEXT ------------------------------------------------

glMemoryObjectParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @memoryObject@.
  -> GLenum -- ^ @pname@ of type [MemoryObjectParameterName](Graphics-GL-Groups.html#MemoryObjectParameterName).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glMemoryObjectParameterivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glMemoryObjectParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glMemoryObjectParameterivEXT #-}
ptr_glMemoryObjectParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glMemoryObjectParameterivEXT = unsafePerformIO $ getCommand "glMemoryObjectParameterivEXT"

-- glMinSampleShading ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMinSampleShading.xhtml OpenGL 4.x>.
glMinSampleShading
  :: MonadIO m
  => GLfloat -- ^ @value@ of type @ColorF@.
  -> m ()
glMinSampleShading v1 = liftIO $ dyn85 ptr_glMinSampleShading v1

{-# NOINLINE ptr_glMinSampleShading #-}
ptr_glMinSampleShading :: FunPtr (GLfloat -> IO ())
ptr_glMinSampleShading = unsafePerformIO $ getCommand "glMinSampleShading"

-- glMinSampleShadingARB -------------------------------------------------------

-- | This command is an alias for 'glMinSampleShading'.
glMinSampleShadingARB
  :: MonadIO m
  => GLfloat -- ^ @value@ of type @ColorF@.
  -> m ()
glMinSampleShadingARB v1 = liftIO $ dyn85 ptr_glMinSampleShadingARB v1

{-# NOINLINE ptr_glMinSampleShadingARB #-}
ptr_glMinSampleShadingARB :: FunPtr (GLfloat -> IO ())
ptr_glMinSampleShadingARB = unsafePerformIO $ getCommand "glMinSampleShadingARB"

-- glMinSampleShadingOES -------------------------------------------------------

-- | This command is an alias for 'glMinSampleShading'.
glMinSampleShadingOES
  :: MonadIO m
  => GLfloat -- ^ @value@ of type @ColorF@.
  -> m ()
glMinSampleShadingOES v1 = liftIO $ dyn85 ptr_glMinSampleShadingOES v1

{-# NOINLINE ptr_glMinSampleShadingOES #-}
ptr_glMinSampleShadingOES :: FunPtr (GLfloat -> IO ())
ptr_glMinSampleShadingOES = unsafePerformIO $ getCommand "glMinSampleShadingOES"

-- glMinmax --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMinmax.xml OpenGL 2.x>.
glMinmax
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLboolean -- ^ @sink@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glMinmax v1 v2 v3 = liftIO $ dyn550 ptr_glMinmax v1 v2 v3

{-# NOINLINE ptr_glMinmax #-}
ptr_glMinmax :: FunPtr (GLenum -> GLenum -> GLboolean -> IO ())
ptr_glMinmax = unsafePerformIO $ getCommand "glMinmax"

-- glMinmaxEXT -----------------------------------------------------------------

-- | This command is an alias for 'glMinmax'.
glMinmaxEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLboolean -- ^ @sink@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glMinmaxEXT v1 v2 v3 = liftIO $ dyn550 ptr_glMinmaxEXT v1 v2 v3

{-# NOINLINE ptr_glMinmaxEXT #-}
ptr_glMinmaxEXT :: FunPtr (GLenum -> GLenum -> GLboolean -> IO ())
ptr_glMinmaxEXT = unsafePerformIO $ getCommand "glMinmaxEXT"

-- glMultMatrixd ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml OpenGL 2.x>.
glMultMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMultMatrixd v1 = liftIO $ dyn42 ptr_glMultMatrixd v1

{-# NOINLINE ptr_glMultMatrixd #-}
ptr_glMultMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glMultMatrixd = unsafePerformIO $ getCommand "glMultMatrixd"

