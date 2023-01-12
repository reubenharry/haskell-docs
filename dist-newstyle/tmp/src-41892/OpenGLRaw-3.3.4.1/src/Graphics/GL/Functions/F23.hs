{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F23
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

module Graphics.GL.Functions.F23 (
  glRasterPos4xvOES,
  glRasterSamplesEXT,
  glReadBuffer,
  glReadBufferIndexedEXT,
  glReadBufferNV,
  glReadInstrumentsSGIX,
  glReadPixels,
  glReadnPixels,
  glReadnPixelsARB,
  glReadnPixelsEXT,
  glReadnPixelsKHR,
  glRectd,
  glRectdv,
  glRectf,
  glRectfv,
  glRecti,
  glRectiv,
  glRects,
  glRectsv,
  glRectxOES,
  glRectxvOES,
  glReferencePlaneSGIX,
  glReleaseKeyedMutexWin32EXT,
  glReleaseShaderCompiler,
  glRenderGpuMaskNV,
  glRenderMode,
  glRenderbufferStorage,
  glRenderbufferStorageEXT,
  glRenderbufferStorageMultisample,
  glRenderbufferStorageMultisampleANGLE,
  glRenderbufferStorageMultisampleAPPLE,
  glRenderbufferStorageMultisampleAdvancedAMD,
  glRenderbufferStorageMultisampleCoverageNV,
  glRenderbufferStorageMultisampleEXT,
  glRenderbufferStorageMultisampleIMG,
  glRenderbufferStorageMultisampleNV,
  glRenderbufferStorageOES,
  glReplacementCodePointerSUN,
  glReplacementCodeubSUN,
  glReplacementCodeubvSUN,
  glReplacementCodeuiColor3fVertex3fSUN,
  glReplacementCodeuiColor3fVertex3fvSUN,
  glReplacementCodeuiColor4fNormal3fVertex3fSUN,
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN,
  glReplacementCodeuiColor4ubVertex3fSUN,
  glReplacementCodeuiColor4ubVertex3fvSUN,
  glReplacementCodeuiNormal3fVertex3fSUN,
  glReplacementCodeuiNormal3fVertex3fvSUN,
  glReplacementCodeuiSUN,
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN,
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN,
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN,
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN,
  glReplacementCodeuiTexCoord2fVertex3fSUN,
  glReplacementCodeuiTexCoord2fVertex3fvSUN,
  glReplacementCodeuiVertex3fSUN,
  glReplacementCodeuiVertex3fvSUN,
  glReplacementCodeuivSUN,
  glReplacementCodeusSUN,
  glReplacementCodeusvSUN,
  glRequestResidentProgramsNV,
  glResetHistogram,
  glResetHistogramEXT,
  glResetMemoryObjectParameterNV,
  glResetMinmax,
  glResetMinmaxEXT,
  glResizeBuffersMESA,
  glResolveDepthValuesNV,
  glResolveMultisampleFramebufferAPPLE,
  glResumeTransformFeedback,
  glResumeTransformFeedbackNV,
  glRotated,
  glRotatef,
  glRotatex,
  glRotatexOES,
  glSampleCoverage,
  glSampleCoverageARB,
  glSampleCoveragex,
  glSampleCoveragexOES,
  glSampleMapATI,
  glSampleMaskEXT,
  glSampleMaskIndexedNV,
  glSampleMaskSGIS,
  glSampleMaski,
  glSamplePatternEXT,
  glSamplePatternSGIS,
  glSamplerParameterIiv,
  glSamplerParameterIivEXT,
  glSamplerParameterIivOES,
  glSamplerParameterIuiv,
  glSamplerParameterIuivEXT,
  glSamplerParameterIuivOES,
  glSamplerParameterf,
  glSamplerParameterfv,
  glSamplerParameteri,
  glSamplerParameteriv,
  glScaled,
  glScalef,
  glScalex,
  glScalexOES
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glRasterPos4xvOES -----------------------------------------------------------

glRasterPos4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glRasterPos4xvOES v1 = liftIO $ dyn114 ptr_glRasterPos4xvOES v1

{-# NOINLINE ptr_glRasterPos4xvOES #-}
ptr_glRasterPos4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glRasterPos4xvOES = unsafePerformIO $ getCommand "glRasterPos4xvOES"

-- glRasterSamplesEXT ----------------------------------------------------------

glRasterSamplesEXT
  :: MonadIO m
  => GLuint -- ^ @samples@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glRasterSamplesEXT v1 v2 = liftIO $ dyn710 ptr_glRasterSamplesEXT v1 v2

{-# NOINLINE ptr_glRasterSamplesEXT #-}
ptr_glRasterSamplesEXT :: FunPtr (GLuint -> GLboolean -> IO ())
ptr_glRasterSamplesEXT = unsafePerformIO $ getCommand "glRasterSamplesEXT"

-- glReadBuffer ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glReadBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glReadBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glReadBuffer.xhtml OpenGL 4.x>.
glReadBuffer
  :: MonadIO m
  => GLenum -- ^ @src@ of type [ReadBufferMode](Graphics-GL-Groups.html#ReadBufferMode).
  -> m ()
glReadBuffer v1 = liftIO $ dyn5 ptr_glReadBuffer v1

{-# NOINLINE ptr_glReadBuffer #-}
ptr_glReadBuffer :: FunPtr (GLenum -> IO ())
ptr_glReadBuffer = unsafePerformIO $ getCommand "glReadBuffer"

-- glReadBufferIndexedEXT ------------------------------------------------------

glReadBufferIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @src@ of type [ReadBufferMode](Graphics-GL-Groups.html#ReadBufferMode).
  -> GLint -- ^ @index@.
  -> m ()
glReadBufferIndexedEXT v1 v2 = liftIO $ dyn58 ptr_glReadBufferIndexedEXT v1 v2

{-# NOINLINE ptr_glReadBufferIndexedEXT #-}
ptr_glReadBufferIndexedEXT :: FunPtr (GLenum -> GLint -> IO ())
ptr_glReadBufferIndexedEXT = unsafePerformIO $ getCommand "glReadBufferIndexedEXT"

-- glReadBufferNV --------------------------------------------------------------

glReadBufferNV
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> m ()
glReadBufferNV v1 = liftIO $ dyn5 ptr_glReadBufferNV v1

{-# NOINLINE ptr_glReadBufferNV #-}
ptr_glReadBufferNV :: FunPtr (GLenum -> IO ())
ptr_glReadBufferNV = unsafePerformIO $ getCommand "glReadBufferNV"

-- glReadInstrumentsSGIX -------------------------------------------------------

glReadInstrumentsSGIX
  :: MonadIO m
  => GLint -- ^ @marker@.
  -> m ()
glReadInstrumentsSGIX v1 = liftIO $ dyn13 ptr_glReadInstrumentsSGIX v1

{-# NOINLINE ptr_glReadInstrumentsSGIX #-}
ptr_glReadInstrumentsSGIX :: FunPtr (GLint -> IO ())
ptr_glReadInstrumentsSGIX = unsafePerformIO $ getCommand "glReadInstrumentsSGIX"

-- glReadPixels ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glReadPixels.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glReadPixels.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glReadPixels.xhtml OpenGL 4.x>.
glReadPixels
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glReadPixels v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn711 ptr_glReadPixels v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glReadPixels #-}
ptr_glReadPixels :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glReadPixels = unsafePerformIO $ getCommand "glReadPixels"

-- glReadnPixels ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glReadPixels.xhtml OpenGL 4.x>.
glReadnPixels
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glReadnPixels v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn712 ptr_glReadnPixels v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixels #-}
ptr_glReadnPixels :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixels = unsafePerformIO $ getCommand "glReadnPixels"

-- glReadnPixelsARB ------------------------------------------------------------

-- | This command is an alias for 'glReadnPixels'.
glReadnPixelsARB
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glReadnPixelsARB v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn712 ptr_glReadnPixelsARB v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixelsARB #-}
ptr_glReadnPixelsARB :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixelsARB = unsafePerformIO $ getCommand "glReadnPixelsARB"

-- glReadnPixelsEXT ------------------------------------------------------------

-- | This command is an alias for 'glReadnPixels'.
glReadnPixelsEXT
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glReadnPixelsEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn712 ptr_glReadnPixelsEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixelsEXT #-}
ptr_glReadnPixelsEXT :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixelsEXT = unsafePerformIO $ getCommand "glReadnPixelsEXT"

-- glReadnPixelsKHR ------------------------------------------------------------

-- | This command is an alias for 'glReadnPixels'.
glReadnPixelsKHR
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glReadnPixelsKHR v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn712 ptr_glReadnPixelsKHR v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixelsKHR #-}
ptr_glReadnPixelsKHR :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixelsKHR = unsafePerformIO $ getCommand "glReadnPixelsKHR"

-- glRectd ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectdv'.
glRectd
  :: MonadIO m
  => GLdouble -- ^ @x1@ of type @CoordD@.
  -> GLdouble -- ^ @y1@ of type @CoordD@.
  -> GLdouble -- ^ @x2@ of type @CoordD@.
  -> GLdouble -- ^ @y2@ of type @CoordD@.
  -> m ()
glRectd v1 v2 v3 v4 = liftIO $ dyn116 ptr_glRectd v1 v2 v3 v4

{-# NOINLINE ptr_glRectd #-}
ptr_glRectd :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRectd = unsafePerformIO $ getCommand "glRectd"

-- glRectdv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectdv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v1@ pointing to @2@ elements of type @CoordD@.
  -> Ptr GLdouble -- ^ @v2@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glRectdv v1 v2 = liftIO $ dyn713 ptr_glRectdv v1 v2

{-# NOINLINE ptr_glRectdv #-}
ptr_glRectdv :: FunPtr (Ptr GLdouble -> Ptr GLdouble -> IO ())
ptr_glRectdv = unsafePerformIO $ getCommand "glRectdv"

-- glRectf ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectfv'.
glRectf
  :: MonadIO m
  => GLfloat -- ^ @x1@ of type @CoordF@.
  -> GLfloat -- ^ @y1@ of type @CoordF@.
  -> GLfloat -- ^ @x2@ of type @CoordF@.
  -> GLfloat -- ^ @y2@ of type @CoordF@.
  -> m ()
glRectf v1 v2 v3 v4 = liftIO $ dyn52 ptr_glRectf v1 v2 v3 v4

{-# NOINLINE ptr_glRectf #-}
ptr_glRectf :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRectf = unsafePerformIO $ getCommand "glRectf"

-- glRectfv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectfv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v1@ pointing to @2@ elements of type @CoordF@.
  -> Ptr GLfloat -- ^ @v2@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glRectfv v1 v2 = liftIO $ dyn104 ptr_glRectfv v1 v2

{-# NOINLINE ptr_glRectfv #-}
ptr_glRectfv :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glRectfv = unsafePerformIO $ getCommand "glRectfv"

-- glRecti ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectiv'.
glRecti
  :: MonadIO m
  => GLint -- ^ @x1@ of type @CoordI@.
  -> GLint -- ^ @y1@ of type @CoordI@.
  -> GLint -- ^ @x2@ of type @CoordI@.
  -> GLint -- ^ @y2@ of type @CoordI@.
  -> m ()
glRecti v1 v2 v3 v4 = liftIO $ dyn82 ptr_glRecti v1 v2 v3 v4

{-# NOINLINE ptr_glRecti #-}
ptr_glRecti :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glRecti = unsafePerformIO $ getCommand "glRecti"

-- glRectiv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectiv
  :: MonadIO m
  => Ptr GLint -- ^ @v1@ pointing to @2@ elements of type @CoordI@.
  -> Ptr GLint -- ^ @v2@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glRectiv v1 v2 = liftIO $ dyn714 ptr_glRectiv v1 v2

{-# NOINLINE ptr_glRectiv #-}
ptr_glRectiv :: FunPtr (Ptr GLint -> Ptr GLint -> IO ())
ptr_glRectiv = unsafePerformIO $ getCommand "glRectiv"

-- glRects ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectsv'.
glRects
  :: MonadIO m
  => GLshort -- ^ @x1@ of type @CoordS@.
  -> GLshort -- ^ @y1@ of type @CoordS@.
  -> GLshort -- ^ @x2@ of type @CoordS@.
  -> GLshort -- ^ @y2@ of type @CoordS@.
  -> m ()
glRects v1 v2 v3 v4 = liftIO $ dyn120 ptr_glRects v1 v2 v3 v4

{-# NOINLINE ptr_glRects #-}
ptr_glRects :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glRects = unsafePerformIO $ getCommand "glRects"

-- glRectsv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectsv
  :: MonadIO m
  => Ptr GLshort -- ^ @v1@ pointing to @2@ elements of type @CoordS@.
  -> Ptr GLshort -- ^ @v2@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glRectsv v1 v2 = liftIO $ dyn715 ptr_glRectsv v1 v2

{-# NOINLINE ptr_glRectsv #-}
ptr_glRectsv :: FunPtr (Ptr GLshort -> Ptr GLshort -> IO ())
ptr_glRectsv = unsafePerformIO $ getCommand "glRectsv"

-- glRectxOES ------------------------------------------------------------------

glRectxOES
  :: MonadIO m
  => GLfixed -- ^ @x1@.
  -> GLfixed -- ^ @y1@.
  -> GLfixed -- ^ @x2@.
  -> GLfixed -- ^ @y2@.
  -> m ()
glRectxOES v1 v2 v3 v4 = liftIO $ dyn53 ptr_glRectxOES v1 v2 v3 v4

{-# NOINLINE ptr_glRectxOES #-}
ptr_glRectxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRectxOES = unsafePerformIO $ getCommand "glRectxOES"

-- glRectxvOES -----------------------------------------------------------------

glRectxvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @v1@ pointing to @2@ elements of type @GLfixed@.
  -> Ptr GLfixed -- ^ @v2@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glRectxvOES v1 v2 = liftIO $ dyn716 ptr_glRectxvOES v1 v2

{-# NOINLINE ptr_glRectxvOES #-}
ptr_glRectxvOES :: FunPtr (Ptr GLfixed -> Ptr GLfixed -> IO ())
ptr_glRectxvOES = unsafePerformIO $ getCommand "glRectxvOES"

-- glReferencePlaneSGIX --------------------------------------------------------

glReferencePlaneSGIX
  :: MonadIO m
  => Ptr GLdouble -- ^ @equation@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glReferencePlaneSGIX v1 = liftIO $ dyn42 ptr_glReferencePlaneSGIX v1

{-# NOINLINE ptr_glReferencePlaneSGIX #-}
ptr_glReferencePlaneSGIX :: FunPtr (Ptr GLdouble -> IO ())
ptr_glReferencePlaneSGIX = unsafePerformIO $ getCommand "glReferencePlaneSGIX"

-- glReleaseKeyedMutexWin32EXT -------------------------------------------------

glReleaseKeyedMutexWin32EXT
  :: MonadIO m
  => GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @key@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glReleaseKeyedMutexWin32EXT v1 v2 = liftIO $ dyn717 ptr_glReleaseKeyedMutexWin32EXT v1 v2

{-# NOINLINE ptr_glReleaseKeyedMutexWin32EXT #-}
ptr_glReleaseKeyedMutexWin32EXT :: FunPtr (GLuint -> GLuint64 -> IO GLboolean)
ptr_glReleaseKeyedMutexWin32EXT = unsafePerformIO $ getCommand "glReleaseKeyedMutexWin32EXT"

-- glReleaseShaderCompiler -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glReleaseShaderCompiler.xhtml OpenGL 4.x>.
glReleaseShaderCompiler
  :: MonadIO m
  => m ()
glReleaseShaderCompiler = liftIO $ dyn11 ptr_glReleaseShaderCompiler

{-# NOINLINE ptr_glReleaseShaderCompiler #-}
ptr_glReleaseShaderCompiler :: FunPtr (IO ())
ptr_glReleaseShaderCompiler = unsafePerformIO $ getCommand "glReleaseShaderCompiler"

-- glRenderGpuMaskNV -----------------------------------------------------------

glRenderGpuMaskNV
  :: MonadIO m
  => GLbitfield -- ^ @mask@.
  -> m ()
glRenderGpuMaskNV v1 = liftIO $ dyn75 ptr_glRenderGpuMaskNV v1

{-# NOINLINE ptr_glRenderGpuMaskNV #-}
ptr_glRenderGpuMaskNV :: FunPtr (GLbitfield -> IO ())
ptr_glRenderGpuMaskNV = unsafePerformIO $ getCommand "glRenderGpuMaskNV"

-- glRenderMode ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRenderMode.xml OpenGL 2.x>.
glRenderMode
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [RenderingMode](Graphics-GL-Groups.html#RenderingMode).
  -> m GLint
glRenderMode v1 = liftIO $ dyn718 ptr_glRenderMode v1

{-# NOINLINE ptr_glRenderMode #-}
ptr_glRenderMode :: FunPtr (GLenum -> IO GLint)
ptr_glRenderMode = unsafePerformIO $ getCommand "glRenderMode"

-- glRenderbufferStorage -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glRenderbufferStorage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorage.xhtml OpenGL 4.x>.
glRenderbufferStorage
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorage v1 v2 v3 v4 = liftIO $ dyn719 ptr_glRenderbufferStorage v1 v2 v3 v4

{-# NOINLINE ptr_glRenderbufferStorage #-}
ptr_glRenderbufferStorage :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorage = unsafePerformIO $ getCommand "glRenderbufferStorage"

-- glRenderbufferStorageEXT ----------------------------------------------------

-- | This command is an alias for 'glRenderbufferStorage'.
glRenderbufferStorageEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageEXT v1 v2 v3 v4 = liftIO $ dyn719 ptr_glRenderbufferStorageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glRenderbufferStorageEXT #-}
ptr_glRenderbufferStorageEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageEXT = unsafePerformIO $ getCommand "glRenderbufferStorageEXT"

-- glRenderbufferStorageMultisample --------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glRenderbufferStorageMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorageMultisample.xhtml OpenGL 4.x>.
glRenderbufferStorageMultisample
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisample v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glRenderbufferStorageMultisample v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisample #-}
ptr_glRenderbufferStorageMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisample = unsafePerformIO $ getCommand "glRenderbufferStorageMultisample"

-- glRenderbufferStorageMultisampleANGLE ---------------------------------------

glRenderbufferStorageMultisampleANGLE
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleANGLE v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glRenderbufferStorageMultisampleANGLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleANGLE #-}
ptr_glRenderbufferStorageMultisampleANGLE :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleANGLE = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleANGLE"

-- glRenderbufferStorageMultisampleAPPLE ---------------------------------------

glRenderbufferStorageMultisampleAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleAPPLE v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glRenderbufferStorageMultisampleAPPLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleAPPLE #-}
ptr_glRenderbufferStorageMultisampleAPPLE :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleAPPLE = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleAPPLE"

-- glRenderbufferStorageMultisampleAdvancedAMD ---------------------------------

glRenderbufferStorageMultisampleAdvancedAMD
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @samples@.
  -> GLsizei -- ^ @storageSamples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleAdvancedAMD v1 v2 v3 v4 v5 v6 = liftIO $ dyn721 ptr_glRenderbufferStorageMultisampleAdvancedAMD v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glRenderbufferStorageMultisampleAdvancedAMD #-}
ptr_glRenderbufferStorageMultisampleAdvancedAMD :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleAdvancedAMD = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleAdvancedAMD"

-- glRenderbufferStorageMultisampleCoverageNV ----------------------------------

glRenderbufferStorageMultisampleCoverageNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleCoverageNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn721 ptr_glRenderbufferStorageMultisampleCoverageNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glRenderbufferStorageMultisampleCoverageNV #-}
ptr_glRenderbufferStorageMultisampleCoverageNV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleCoverageNV = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleCoverageNV"

-- glRenderbufferStorageMultisampleEXT -----------------------------------------

-- | This command is an alias for 'glRenderbufferStorageMultisample'.
glRenderbufferStorageMultisampleEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleEXT #-}
ptr_glRenderbufferStorageMultisampleEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleEXT = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleEXT"

-- glRenderbufferStorageMultisampleIMG -----------------------------------------

glRenderbufferStorageMultisampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleIMG v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glRenderbufferStorageMultisampleIMG v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleIMG #-}
ptr_glRenderbufferStorageMultisampleIMG :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleIMG = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleIMG"

-- glRenderbufferStorageMultisampleNV ------------------------------------------

-- | This command is an alias for 'glRenderbufferStorageMultisample'.
glRenderbufferStorageMultisampleNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleNV v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glRenderbufferStorageMultisampleNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleNV #-}
ptr_glRenderbufferStorageMultisampleNV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleNV = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleNV"

-- glRenderbufferStorageOES ----------------------------------------------------

glRenderbufferStorageOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageOES v1 v2 v3 v4 = liftIO $ dyn719 ptr_glRenderbufferStorageOES v1 v2 v3 v4

{-# NOINLINE ptr_glRenderbufferStorageOES #-}
ptr_glRenderbufferStorageOES :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageOES = unsafePerformIO $ getCommand "glRenderbufferStorageOES"

-- glReplacementCodePointerSUN -------------------------------------------------

glReplacementCodePointerSUN
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ReplacementCodeTypeSUN](Graphics-GL-Groups.html#ReplacementCodeTypeSUN).
  -> GLsizei -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @Ptr a@.
  -> m ()
glReplacementCodePointerSUN v1 v2 v3 = liftIO $ dyn722 ptr_glReplacementCodePointerSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodePointerSUN #-}
ptr_glReplacementCodePointerSUN :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr a) -> IO ())
ptr_glReplacementCodePointerSUN = unsafePerformIO $ getCommand "glReplacementCodePointerSUN"

-- glReplacementCodeubSUN ------------------------------------------------------

glReplacementCodeubSUN
  :: MonadIO m
  => GLubyte -- ^ @code@.
  -> m ()
glReplacementCodeubSUN v1 = liftIO $ dyn486 ptr_glReplacementCodeubSUN v1

{-# NOINLINE ptr_glReplacementCodeubSUN #-}
ptr_glReplacementCodeubSUN :: FunPtr (GLubyte -> IO ())
ptr_glReplacementCodeubSUN = unsafePerformIO $ getCommand "glReplacementCodeubSUN"

-- glReplacementCodeubvSUN -----------------------------------------------------

glReplacementCodeubvSUN
  :: MonadIO m
  => Ptr GLubyte -- ^ @code@ pointing to @COMPSIZE()@ elements of type @GLubyte@.
  -> m ()
glReplacementCodeubvSUN v1 = liftIO $ dyn108 ptr_glReplacementCodeubvSUN v1

{-# NOINLINE ptr_glReplacementCodeubvSUN #-}
ptr_glReplacementCodeubvSUN :: FunPtr (Ptr GLubyte -> IO ())
ptr_glReplacementCodeubvSUN = unsafePerformIO $ getCommand "glReplacementCodeubvSUN"

-- glReplacementCodeuiColor3fVertex3fSUN ---------------------------------------

glReplacementCodeuiColor3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn723 ptr_glReplacementCodeuiColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glReplacementCodeuiColor3fVertex3fSUN #-}
ptr_glReplacementCodeuiColor3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiColor3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor3fVertex3fSUN"

-- glReplacementCodeuiColor3fVertex3fvSUN --------------------------------------

glReplacementCodeuiColor3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @c@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiColor3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn724 ptr_glReplacementCodeuiColor3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiColor3fVertex3fvSUN #-}
ptr_glReplacementCodeuiColor3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiColor3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor3fVertex3fvSUN"

-- glReplacementCodeuiColor4fNormal3fVertex3fSUN -------------------------------

glReplacementCodeuiColor4fNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @a@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn725 ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4fNormal3fVertex3fSUN"

-- glReplacementCodeuiColor4fNormal3fVertex3fvSUN ------------------------------

glReplacementCodeuiColor4fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiColor4fNormal3fVertex3fvSUN v1 v2 v3 v4 = liftIO $ dyn726 ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN v1 v2 v3 v4

{-# NOINLINE ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4fNormal3fVertex3fvSUN"

-- glReplacementCodeuiColor4ubVertex3fSUN --------------------------------------

glReplacementCodeuiColor4ubVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLubyte -- ^ @r@.
  -> GLubyte -- ^ @g@.
  -> GLubyte -- ^ @b@.
  -> GLubyte -- ^ @a@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn727 ptr_glReplacementCodeuiColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReplacementCodeuiColor4ubVertex3fSUN #-}
ptr_glReplacementCodeuiColor4ubVertex3fSUN :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiColor4ubVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4ubVertex3fSUN"

-- glReplacementCodeuiColor4ubVertex3fvSUN -------------------------------------

glReplacementCodeuiColor4ubVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLubyte -- ^ @c@ pointing to @4@ elements of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiColor4ubVertex3fvSUN v1 v2 v3 = liftIO $ dyn728 ptr_glReplacementCodeuiColor4ubVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiColor4ubVertex3fvSUN #-}
ptr_glReplacementCodeuiColor4ubVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiColor4ubVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4ubVertex3fvSUN"

-- glReplacementCodeuiNormal3fVertex3fSUN --------------------------------------

glReplacementCodeuiNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn723 ptr_glReplacementCodeuiNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glReplacementCodeuiNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiNormal3fVertex3fSUN"

-- glReplacementCodeuiNormal3fVertex3fvSUN -------------------------------------

glReplacementCodeuiNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiNormal3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn724 ptr_glReplacementCodeuiNormal3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiNormal3fVertex3fvSUN"

-- glReplacementCodeuiSUN ------------------------------------------------------

glReplacementCodeuiSUN
  :: MonadIO m
  => GLuint -- ^ @code@.
  -> m ()
glReplacementCodeuiSUN v1 = liftIO $ dyn3 ptr_glReplacementCodeuiSUN v1

{-# NOINLINE ptr_glReplacementCodeuiSUN #-}
ptr_glReplacementCodeuiSUN :: FunPtr (GLuint -> IO ())
ptr_glReplacementCodeuiSUN = unsafePerformIO $ getCommand "glReplacementCodeuiSUN"

-- glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN ---------------------

glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @a@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn729 ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN"

-- glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN --------------------

glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN v1 v2 v3 v4 v5 = liftIO $ dyn730 ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN v1 v2 v3 v4 v5

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN"

-- glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN ----------------------------

glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn731 ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN"

-- glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN ---------------------------

glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN v1 v2 v3 v4 = liftIO $ dyn726 ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN v1 v2 v3 v4

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN"

-- glReplacementCodeuiTexCoord2fVertex3fSUN ------------------------------------

glReplacementCodeuiTexCoord2fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiTexCoord2fVertex3fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn732 ptr_glReplacementCodeuiTexCoord2fVertex3fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fVertex3fSUN #-}
ptr_glReplacementCodeuiTexCoord2fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fVertex3fSUN"

-- glReplacementCodeuiTexCoord2fVertex3fvSUN -----------------------------------

glReplacementCodeuiTexCoord2fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiTexCoord2fVertex3fvSUN v1 v2 v3 = liftIO $ dyn724 ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN #-}
ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fVertex3fvSUN"

-- glReplacementCodeuiVertex3fSUN ----------------------------------------------

glReplacementCodeuiVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiVertex3fSUN v1 v2 v3 v4 = liftIO $ dyn733 ptr_glReplacementCodeuiVertex3fSUN v1 v2 v3 v4

{-# NOINLINE ptr_glReplacementCodeuiVertex3fSUN #-}
ptr_glReplacementCodeuiVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiVertex3fSUN"

-- glReplacementCodeuiVertex3fvSUN ---------------------------------------------

glReplacementCodeuiVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiVertex3fvSUN v1 v2 = liftIO $ dyn734 ptr_glReplacementCodeuiVertex3fvSUN v1 v2

{-# NOINLINE ptr_glReplacementCodeuiVertex3fvSUN #-}
ptr_glReplacementCodeuiVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiVertex3fvSUN"

-- glReplacementCodeuivSUN -----------------------------------------------------

glReplacementCodeuivSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @code@ pointing to @COMPSIZE()@ elements of type @GLuint@.
  -> m ()
glReplacementCodeuivSUN v1 = liftIO $ dyn110 ptr_glReplacementCodeuivSUN v1

{-# NOINLINE ptr_glReplacementCodeuivSUN #-}
ptr_glReplacementCodeuivSUN :: FunPtr (Ptr GLuint -> IO ())
ptr_glReplacementCodeuivSUN = unsafePerformIO $ getCommand "glReplacementCodeuivSUN"

-- glReplacementCodeusSUN ------------------------------------------------------

glReplacementCodeusSUN
  :: MonadIO m
  => GLushort -- ^ @code@.
  -> m ()
glReplacementCodeusSUN v1 = liftIO $ dyn487 ptr_glReplacementCodeusSUN v1

{-# NOINLINE ptr_glReplacementCodeusSUN #-}
ptr_glReplacementCodeusSUN :: FunPtr (GLushort -> IO ())
ptr_glReplacementCodeusSUN = unsafePerformIO $ getCommand "glReplacementCodeusSUN"

-- glReplacementCodeusvSUN -----------------------------------------------------

glReplacementCodeusvSUN
  :: MonadIO m
  => Ptr GLushort -- ^ @code@ pointing to @COMPSIZE()@ elements of type @GLushort@.
  -> m ()
glReplacementCodeusvSUN v1 = liftIO $ dyn112 ptr_glReplacementCodeusvSUN v1

{-# NOINLINE ptr_glReplacementCodeusvSUN #-}
ptr_glReplacementCodeusvSUN :: FunPtr (Ptr GLushort -> IO ())
ptr_glReplacementCodeusvSUN = unsafePerformIO $ getCommand "glReplacementCodeusvSUN"

-- glRequestResidentProgramsNV -------------------------------------------------

glRequestResidentProgramsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glRequestResidentProgramsNV v1 v2 = liftIO $ dyn200 ptr_glRequestResidentProgramsNV v1 v2

{-# NOINLINE ptr_glRequestResidentProgramsNV #-}
ptr_glRequestResidentProgramsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glRequestResidentProgramsNV = unsafePerformIO $ getCommand "glRequestResidentProgramsNV"

-- glResetHistogram ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glResetHistogram.xml OpenGL 2.x>.
glResetHistogram
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HistogramTargetEXT](Graphics-GL-Groups.html#HistogramTargetEXT).
  -> m ()
glResetHistogram v1 = liftIO $ dyn5 ptr_glResetHistogram v1

{-# NOINLINE ptr_glResetHistogram #-}
ptr_glResetHistogram :: FunPtr (GLenum -> IO ())
ptr_glResetHistogram = unsafePerformIO $ getCommand "glResetHistogram"

-- glResetHistogramEXT ---------------------------------------------------------

-- | This command is an alias for 'glResetHistogram'.
glResetHistogramEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HistogramTargetEXT](Graphics-GL-Groups.html#HistogramTargetEXT).
  -> m ()
glResetHistogramEXT v1 = liftIO $ dyn5 ptr_glResetHistogramEXT v1

{-# NOINLINE ptr_glResetHistogramEXT #-}
ptr_glResetHistogramEXT :: FunPtr (GLenum -> IO ())
ptr_glResetHistogramEXT = unsafePerformIO $ getCommand "glResetHistogramEXT"

-- glResetMemoryObjectParameterNV ----------------------------------------------

glResetMemoryObjectParameterNV
  :: MonadIO m
  => GLuint -- ^ @memory@.
  -> GLenum -- ^ @pname@.
  -> m ()
glResetMemoryObjectParameterNV v1 v2 = liftIO $ dyn18 ptr_glResetMemoryObjectParameterNV v1 v2

{-# NOINLINE ptr_glResetMemoryObjectParameterNV #-}
ptr_glResetMemoryObjectParameterNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glResetMemoryObjectParameterNV = unsafePerformIO $ getCommand "glResetMemoryObjectParameterNV"

-- glResetMinmax ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glResetMinmax.xml OpenGL 2.x>.
glResetMinmax
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> m ()
glResetMinmax v1 = liftIO $ dyn5 ptr_glResetMinmax v1

{-# NOINLINE ptr_glResetMinmax #-}
ptr_glResetMinmax :: FunPtr (GLenum -> IO ())
ptr_glResetMinmax = unsafePerformIO $ getCommand "glResetMinmax"

-- glResetMinmaxEXT ------------------------------------------------------------

-- | This command is an alias for 'glResetMinmax'.
glResetMinmaxEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> m ()
glResetMinmaxEXT v1 = liftIO $ dyn5 ptr_glResetMinmaxEXT v1

{-# NOINLINE ptr_glResetMinmaxEXT #-}
ptr_glResetMinmaxEXT :: FunPtr (GLenum -> IO ())
ptr_glResetMinmaxEXT = unsafePerformIO $ getCommand "glResetMinmaxEXT"

-- glResizeBuffersMESA ---------------------------------------------------------

glResizeBuffersMESA
  :: MonadIO m
  => m ()
glResizeBuffersMESA = liftIO $ dyn11 ptr_glResizeBuffersMESA

{-# NOINLINE ptr_glResizeBuffersMESA #-}
ptr_glResizeBuffersMESA :: FunPtr (IO ())
ptr_glResizeBuffersMESA = unsafePerformIO $ getCommand "glResizeBuffersMESA"

-- glResolveDepthValuesNV ------------------------------------------------------

glResolveDepthValuesNV
  :: MonadIO m
  => m ()
glResolveDepthValuesNV = liftIO $ dyn11 ptr_glResolveDepthValuesNV

{-# NOINLINE ptr_glResolveDepthValuesNV #-}
ptr_glResolveDepthValuesNV :: FunPtr (IO ())
ptr_glResolveDepthValuesNV = unsafePerformIO $ getCommand "glResolveDepthValuesNV"

-- glResolveMultisampleFramebufferAPPLE ----------------------------------------

glResolveMultisampleFramebufferAPPLE
  :: MonadIO m
  => m ()
glResolveMultisampleFramebufferAPPLE = liftIO $ dyn11 ptr_glResolveMultisampleFramebufferAPPLE

{-# NOINLINE ptr_glResolveMultisampleFramebufferAPPLE #-}
ptr_glResolveMultisampleFramebufferAPPLE :: FunPtr (IO ())
ptr_glResolveMultisampleFramebufferAPPLE = unsafePerformIO $ getCommand "glResolveMultisampleFramebufferAPPLE"

-- glResumeTransformFeedback ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glResumeTransformFeedback.xhtml OpenGL 4.x>.
glResumeTransformFeedback
  :: MonadIO m
  => m ()
glResumeTransformFeedback = liftIO $ dyn11 ptr_glResumeTransformFeedback

{-# NOINLINE ptr_glResumeTransformFeedback #-}
ptr_glResumeTransformFeedback :: FunPtr (IO ())
ptr_glResumeTransformFeedback = unsafePerformIO $ getCommand "glResumeTransformFeedback"

-- glResumeTransformFeedbackNV -------------------------------------------------

-- | This command is an alias for 'glResumeTransformFeedback'.
glResumeTransformFeedbackNV
  :: MonadIO m
  => m ()
glResumeTransformFeedbackNV = liftIO $ dyn11 ptr_glResumeTransformFeedbackNV

{-# NOINLINE ptr_glResumeTransformFeedbackNV #-}
ptr_glResumeTransformFeedbackNV :: FunPtr (IO ())
ptr_glResumeTransformFeedbackNV = unsafePerformIO $ getCommand "glResumeTransformFeedbackNV"

-- glRotated -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRotate.xml OpenGL 2.x>.
glRotated
  :: MonadIO m
  => GLdouble -- ^ @angle@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glRotated v1 v2 v3 v4 = liftIO $ dyn116 ptr_glRotated v1 v2 v3 v4

{-# NOINLINE ptr_glRotated #-}
ptr_glRotated :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRotated = unsafePerformIO $ getCommand "glRotated"

-- glRotatef -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRotate.xml OpenGL 2.x>.
glRotatef
  :: MonadIO m
  => GLfloat -- ^ @angle@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glRotatef v1 v2 v3 v4 = liftIO $ dyn52 ptr_glRotatef v1 v2 v3 v4

{-# NOINLINE ptr_glRotatef #-}
ptr_glRotatef :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRotatef = unsafePerformIO $ getCommand "glRotatef"

-- glRotatex -------------------------------------------------------------------

glRotatex
  :: MonadIO m
  => GLfixed -- ^ @angle@.
  -> GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glRotatex v1 v2 v3 v4 = liftIO $ dyn53 ptr_glRotatex v1 v2 v3 v4

{-# NOINLINE ptr_glRotatex #-}
ptr_glRotatex :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRotatex = unsafePerformIO $ getCommand "glRotatex"

-- glRotatexOES ----------------------------------------------------------------

glRotatexOES
  :: MonadIO m
  => GLfixed -- ^ @angle@.
  -> GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glRotatexOES v1 v2 v3 v4 = liftIO $ dyn53 ptr_glRotatexOES v1 v2 v3 v4

{-# NOINLINE ptr_glRotatexOES #-}
ptr_glRotatexOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRotatexOES = unsafePerformIO $ getCommand "glRotatexOES"

-- glSampleCoverage ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glSampleCoverage.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glSampleCoverage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSampleCoverage.xhtml OpenGL 4.x>.
glSampleCoverage
  :: MonadIO m
  => GLfloat -- ^ @value@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleCoverage v1 v2 = liftIO $ dyn735 ptr_glSampleCoverage v1 v2

{-# NOINLINE ptr_glSampleCoverage #-}
ptr_glSampleCoverage :: FunPtr (GLfloat -> GLboolean -> IO ())
ptr_glSampleCoverage = unsafePerformIO $ getCommand "glSampleCoverage"

-- glSampleCoverageARB ---------------------------------------------------------

-- | This command is an alias for 'glSampleCoverage'.
glSampleCoverageARB
  :: MonadIO m
  => GLfloat -- ^ @value@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleCoverageARB v1 v2 = liftIO $ dyn735 ptr_glSampleCoverageARB v1 v2

{-# NOINLINE ptr_glSampleCoverageARB #-}
ptr_glSampleCoverageARB :: FunPtr (GLfloat -> GLboolean -> IO ())
ptr_glSampleCoverageARB = unsafePerformIO $ getCommand "glSampleCoverageARB"

-- glSampleCoveragex -----------------------------------------------------------

glSampleCoveragex
  :: MonadIO m
  => GLclampx -- ^ @value@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleCoveragex v1 v2 = liftIO $ dyn736 ptr_glSampleCoveragex v1 v2

{-# NOINLINE ptr_glSampleCoveragex #-}
ptr_glSampleCoveragex :: FunPtr (GLclampx -> GLboolean -> IO ())
ptr_glSampleCoveragex = unsafePerformIO $ getCommand "glSampleCoveragex"

-- glSampleCoveragexOES --------------------------------------------------------

glSampleCoveragexOES
  :: MonadIO m
  => GLclampx -- ^ @value@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleCoveragexOES v1 v2 = liftIO $ dyn736 ptr_glSampleCoveragexOES v1 v2

{-# NOINLINE ptr_glSampleCoveragexOES #-}
ptr_glSampleCoveragexOES :: FunPtr (GLclampx -> GLboolean -> IO ())
ptr_glSampleCoveragexOES = unsafePerformIO $ getCommand "glSampleCoveragexOES"

-- glSampleMapATI --------------------------------------------------------------

glSampleMapATI
  :: MonadIO m
  => GLuint -- ^ @dst@.
  -> GLuint -- ^ @interp@.
  -> GLenum -- ^ @swizzle@ of type [SwizzleOpATI](Graphics-GL-Groups.html#SwizzleOpATI).
  -> m ()
glSampleMapATI v1 v2 v3 = liftIO $ dyn635 ptr_glSampleMapATI v1 v2 v3

{-# NOINLINE ptr_glSampleMapATI #-}
ptr_glSampleMapATI :: FunPtr (GLuint -> GLuint -> GLenum -> IO ())
ptr_glSampleMapATI = unsafePerformIO $ getCommand "glSampleMapATI"

-- glSampleMaskEXT -------------------------------------------------------------

glSampleMaskEXT
  :: MonadIO m
  => GLclampf -- ^ @value@ of type @ClampedFloat32@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleMaskEXT v1 v2 = liftIO $ dyn737 ptr_glSampleMaskEXT v1 v2

{-# NOINLINE ptr_glSampleMaskEXT #-}
ptr_glSampleMaskEXT :: FunPtr (GLclampf -> GLboolean -> IO ())
ptr_glSampleMaskEXT = unsafePerformIO $ getCommand "glSampleMaskEXT"

-- glSampleMaskIndexedNV -------------------------------------------------------

glSampleMaskIndexedNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLbitfield -- ^ @mask@ of type @SampleMaskNV@.
  -> m ()
glSampleMaskIndexedNV v1 v2 = liftIO $ dyn606 ptr_glSampleMaskIndexedNV v1 v2

{-# NOINLINE ptr_glSampleMaskIndexedNV #-}
ptr_glSampleMaskIndexedNV :: FunPtr (GLuint -> GLbitfield -> IO ())
ptr_glSampleMaskIndexedNV = unsafePerformIO $ getCommand "glSampleMaskIndexedNV"

-- glSampleMaskSGIS ------------------------------------------------------------

-- | This command is an alias for 'glSampleMaskEXT'.
glSampleMaskSGIS
  :: MonadIO m
  => GLclampf -- ^ @value@ of type @ClampedFloat32@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleMaskSGIS v1 v2 = liftIO $ dyn737 ptr_glSampleMaskSGIS v1 v2

{-# NOINLINE ptr_glSampleMaskSGIS #-}
ptr_glSampleMaskSGIS :: FunPtr (GLclampf -> GLboolean -> IO ())
ptr_glSampleMaskSGIS = unsafePerformIO $ getCommand "glSampleMaskSGIS"

-- glSampleMaski ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSampleMaski.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSampleMaski.xhtml OpenGL 4.x>.
glSampleMaski
  :: MonadIO m
  => GLuint -- ^ @maskNumber@.
  -> GLbitfield -- ^ @mask@.
  -> m ()
glSampleMaski v1 v2 = liftIO $ dyn606 ptr_glSampleMaski v1 v2

{-# NOINLINE ptr_glSampleMaski #-}
ptr_glSampleMaski :: FunPtr (GLuint -> GLbitfield -> IO ())
ptr_glSampleMaski = unsafePerformIO $ getCommand "glSampleMaski"

-- glSamplePatternEXT ----------------------------------------------------------

glSamplePatternEXT
  :: MonadIO m
  => GLenum -- ^ @pattern@ of type [SamplePatternEXT](Graphics-GL-Groups.html#SamplePatternEXT).
  -> m ()
glSamplePatternEXT v1 = liftIO $ dyn5 ptr_glSamplePatternEXT v1

{-# NOINLINE ptr_glSamplePatternEXT #-}
ptr_glSamplePatternEXT :: FunPtr (GLenum -> IO ())
ptr_glSamplePatternEXT = unsafePerformIO $ getCommand "glSamplePatternEXT"

-- glSamplePatternSGIS ---------------------------------------------------------

-- | This command is an alias for 'glSamplePatternEXT'.
glSamplePatternSGIS
  :: MonadIO m
  => GLenum -- ^ @pattern@ of type [SamplePatternSGIS](Graphics-GL-Groups.html#SamplePatternSGIS).
  -> m ()
glSamplePatternSGIS v1 = liftIO $ dyn5 ptr_glSamplePatternSGIS v1

{-# NOINLINE ptr_glSamplePatternSGIS #-}
ptr_glSamplePatternSGIS :: FunPtr (GLenum -> IO ())
ptr_glSamplePatternSGIS = unsafePerformIO $ getCommand "glSamplePatternSGIS"

-- glSamplerParameterIiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterIiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameterIiv v1 v2 v3 = liftIO $ dyn348 ptr_glSamplerParameterIiv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIiv #-}
ptr_glSamplerParameterIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameterIiv = unsafePerformIO $ getCommand "glSamplerParameterIiv"

-- glSamplerParameterIivEXT ----------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIiv'.
glSamplerParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameterIivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glSamplerParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIivEXT #-}
ptr_glSamplerParameterIivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameterIivEXT = unsafePerformIO $ getCommand "glSamplerParameterIivEXT"

-- glSamplerParameterIivOES ----------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIiv'.
glSamplerParameterIivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameterIivOES v1 v2 v3 = liftIO $ dyn348 ptr_glSamplerParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIivOES #-}
ptr_glSamplerParameterIivOES :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameterIivOES = unsafePerformIO $ getCommand "glSamplerParameterIivOES"

-- glSamplerParameterIuiv ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterIuiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glSamplerParameterIuiv v1 v2 v3 = liftIO $ dyn392 ptr_glSamplerParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIuiv #-}
ptr_glSamplerParameterIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glSamplerParameterIuiv = unsafePerformIO $ getCommand "glSamplerParameterIuiv"

-- glSamplerParameterIuivEXT ---------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIuiv'.
glSamplerParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glSamplerParameterIuivEXT v1 v2 v3 = liftIO $ dyn392 ptr_glSamplerParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIuivEXT #-}
ptr_glSamplerParameterIuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glSamplerParameterIuivEXT = unsafePerformIO $ getCommand "glSamplerParameterIuivEXT"

-- glSamplerParameterIuivOES ---------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIuiv'.
glSamplerParameterIuivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glSamplerParameterIuivOES v1 v2 v3 = liftIO $ dyn392 ptr_glSamplerParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIuivOES #-}
ptr_glSamplerParameterIuivOES :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glSamplerParameterIuivOES = unsafePerformIO $ getCommand "glSamplerParameterIuivOES"

-- glSamplerParameterf ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterf
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterF](Graphics-GL-Groups.html#SamplerParameterF).
  -> GLfloat -- ^ @param@.
  -> m ()
glSamplerParameterf v1 v2 v3 = liftIO $ dyn514 ptr_glSamplerParameterf v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterf #-}
ptr_glSamplerParameterf :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
ptr_glSamplerParameterf = unsafePerformIO $ getCommand "glSamplerParameterf"

-- glSamplerParameterfv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterfv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterF](Graphics-GL-Groups.html#SamplerParameterF).
  -> Ptr GLfloat -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glSamplerParameterfv v1 v2 v3 = liftIO $ dyn364 ptr_glSamplerParameterfv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterfv #-}
ptr_glSamplerParameterfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glSamplerParameterfv = unsafePerformIO $ getCommand "glSamplerParameterfv"

-- glSamplerParameteri ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameteri
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> GLint -- ^ @param@.
  -> m ()
glSamplerParameteri v1 v2 v3 = liftIO $ dyn491 ptr_glSamplerParameteri v1 v2 v3

{-# NOINLINE ptr_glSamplerParameteri #-}
ptr_glSamplerParameteri :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glSamplerParameteri = unsafePerformIO $ getCommand "glSamplerParameteri"

-- glSamplerParameteriv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameteriv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@ of type [SamplerParameterI](Graphics-GL-Groups.html#SamplerParameterI).
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameteriv v1 v2 v3 = liftIO $ dyn348 ptr_glSamplerParameteriv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameteriv #-}
ptr_glSamplerParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameteriv = unsafePerformIO $ getCommand "glSamplerParameteriv"

-- glScaled --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml OpenGL 2.x>.
glScaled
  :: MonadIO m
  => GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glScaled v1 v2 v3 = liftIO $ dyn41 ptr_glScaled v1 v2 v3

{-# NOINLINE ptr_glScaled #-}
ptr_glScaled :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glScaled = unsafePerformIO $ getCommand "glScaled"

-- glScalef --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml OpenGL 2.x>.
glScalef
  :: MonadIO m
  => GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glScalef v1 v2 v3 = liftIO $ dyn43 ptr_glScalef v1 v2 v3

{-# NOINLINE ptr_glScalef #-}
ptr_glScalef :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glScalef = unsafePerformIO $ getCommand "glScalef"

-- glScalex --------------------------------------------------------------------

glScalex
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glScalex v1 v2 v3 = liftIO $ dyn113 ptr_glScalex v1 v2 v3

{-# NOINLINE ptr_glScalex #-}
ptr_glScalex :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glScalex = unsafePerformIO $ getCommand "glScalex"

-- glScalexOES -----------------------------------------------------------------

glScalexOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glScalexOES v1 v2 v3 = liftIO $ dyn113 ptr_glScalexOES v1 v2 v3

{-# NOINLINE ptr_glScalexOES #-}
ptr_glScalexOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glScalexOES = unsafePerformIO $ getCommand "glScalexOES"

