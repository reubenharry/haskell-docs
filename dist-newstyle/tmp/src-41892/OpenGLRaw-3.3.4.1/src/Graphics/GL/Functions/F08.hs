{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F08
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

module Graphics.GL.Functions.F08 (
  glFinishTextureSUNX,
  glFlush,
  glFlushMappedBufferRange,
  glFlushMappedBufferRangeAPPLE,
  glFlushMappedBufferRangeEXT,
  glFlushMappedNamedBufferRange,
  glFlushMappedNamedBufferRangeEXT,
  glFlushPixelDataRangeNV,
  glFlushRasterSGIX,
  glFlushStaticDataIBM,
  glFlushVertexArrayRangeAPPLE,
  glFlushVertexArrayRangeNV,
  glFogCoordFormatNV,
  glFogCoordPointer,
  glFogCoordPointerEXT,
  glFogCoordPointerListIBM,
  glFogCoordd,
  glFogCoorddEXT,
  glFogCoorddv,
  glFogCoorddvEXT,
  glFogCoordf,
  glFogCoordfEXT,
  glFogCoordfv,
  glFogCoordfvEXT,
  glFogCoordhNV,
  glFogCoordhvNV,
  glFogFuncSGIS,
  glFogf,
  glFogfv,
  glFogi,
  glFogiv,
  glFogx,
  glFogxOES,
  glFogxv,
  glFogxvOES,
  glFragmentColorMaterialSGIX,
  glFragmentCoverageColorNV,
  glFragmentLightModelfSGIX,
  glFragmentLightModelfvSGIX,
  glFragmentLightModeliSGIX,
  glFragmentLightModelivSGIX,
  glFragmentLightfSGIX,
  glFragmentLightfvSGIX,
  glFragmentLightiSGIX,
  glFragmentLightivSGIX,
  glFragmentMaterialfSGIX,
  glFragmentMaterialfvSGIX,
  glFragmentMaterialiSGIX,
  glFragmentMaterialivSGIX,
  glFrameTerminatorGREMEDY,
  glFrameZoomSGIX,
  glFramebufferDrawBufferEXT,
  glFramebufferDrawBuffersEXT,
  glFramebufferFetchBarrierEXT,
  glFramebufferFetchBarrierQCOM,
  glFramebufferFoveationConfigQCOM,
  glFramebufferFoveationParametersQCOM,
  glFramebufferParameteri,
  glFramebufferParameteriMESA,
  glFramebufferPixelLocalStorageSizeEXT,
  glFramebufferReadBufferEXT,
  glFramebufferRenderbuffer,
  glFramebufferRenderbufferEXT,
  glFramebufferRenderbufferOES,
  glFramebufferSampleLocationsfvARB,
  glFramebufferSampleLocationsfvNV,
  glFramebufferSamplePositionsfvAMD,
  glFramebufferTexture,
  glFramebufferTexture1D,
  glFramebufferTexture1DEXT,
  glFramebufferTexture2D,
  glFramebufferTexture2DDownsampleIMG,
  glFramebufferTexture2DEXT,
  glFramebufferTexture2DMultisampleEXT,
  glFramebufferTexture2DMultisampleIMG,
  glFramebufferTexture2DOES,
  glFramebufferTexture3D,
  glFramebufferTexture3DEXT,
  glFramebufferTexture3DOES,
  glFramebufferTextureARB,
  glFramebufferTextureEXT,
  glFramebufferTextureFaceARB,
  glFramebufferTextureFaceEXT,
  glFramebufferTextureLayer,
  glFramebufferTextureLayerARB,
  glFramebufferTextureLayerDownsampleIMG,
  glFramebufferTextureLayerEXT,
  glFramebufferTextureMultisampleMultiviewOVR,
  glFramebufferTextureMultiviewOVR,
  glFramebufferTextureOES,
  glFreeObjectBufferATI,
  glFrontFace,
  glFrustum,
  glFrustumf,
  glFrustumfOES,
  glFrustumx,
  glFrustumxOES,
  glGenAsyncMarkersSGIX,
  glGenBuffers,
  glGenBuffersARB
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glFinishTextureSUNX ---------------------------------------------------------

glFinishTextureSUNX
  :: MonadIO m
  => m ()
glFinishTextureSUNX = liftIO $ dyn11 ptr_glFinishTextureSUNX

{-# NOINLINE ptr_glFinishTextureSUNX #-}
ptr_glFinishTextureSUNX :: FunPtr (IO ())
ptr_glFinishTextureSUNX = unsafePerformIO $ getCommand "glFinishTextureSUNX"

-- glFlush ---------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glFlush.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glFlush.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFlush.xhtml OpenGL 4.x>.
glFlush
  :: MonadIO m
  => m ()
glFlush = liftIO $ dyn11 ptr_glFlush

{-# NOINLINE ptr_glFlush #-}
ptr_glFlush :: FunPtr (IO ())
ptr_glFlush = unsafePerformIO $ getCommand "glFlush"

-- glFlushMappedBufferRange ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFlushMappedBufferRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFlushMappedBufferRange.xhtml OpenGL 4.x>.
glFlushMappedBufferRange
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> m ()
glFlushMappedBufferRange v1 v2 v3 = liftIO $ dyn289 ptr_glFlushMappedBufferRange v1 v2 v3

{-# NOINLINE ptr_glFlushMappedBufferRange #-}
ptr_glFlushMappedBufferRange :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedBufferRange = unsafePerformIO $ getCommand "glFlushMappedBufferRange"

-- glFlushMappedBufferRangeAPPLE -----------------------------------------------

-- | This command is an alias for 'glFlushMappedBufferRange'.
glFlushMappedBufferRangeAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glFlushMappedBufferRangeAPPLE v1 v2 v3 = liftIO $ dyn289 ptr_glFlushMappedBufferRangeAPPLE v1 v2 v3

{-# NOINLINE ptr_glFlushMappedBufferRangeAPPLE #-}
ptr_glFlushMappedBufferRangeAPPLE :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedBufferRangeAPPLE = unsafePerformIO $ getCommand "glFlushMappedBufferRangeAPPLE"

-- glFlushMappedBufferRangeEXT -------------------------------------------------

-- | This command is an alias for 'glFlushMappedBufferRange'.
glFlushMappedBufferRangeEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> m ()
glFlushMappedBufferRangeEXT v1 v2 v3 = liftIO $ dyn289 ptr_glFlushMappedBufferRangeEXT v1 v2 v3

{-# NOINLINE ptr_glFlushMappedBufferRangeEXT #-}
ptr_glFlushMappedBufferRangeEXT :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedBufferRangeEXT = unsafePerformIO $ getCommand "glFlushMappedBufferRangeEXT"

-- glFlushMappedNamedBufferRange -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFlushMappedBufferRange.xhtml OpenGL 4.x>.
glFlushMappedNamedBufferRange
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> m ()
glFlushMappedNamedBufferRange v1 v2 v3 = liftIO $ dyn290 ptr_glFlushMappedNamedBufferRange v1 v2 v3

{-# NOINLINE ptr_glFlushMappedNamedBufferRange #-}
ptr_glFlushMappedNamedBufferRange :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedNamedBufferRange = unsafePerformIO $ getCommand "glFlushMappedNamedBufferRange"

-- glFlushMappedNamedBufferRangeEXT --------------------------------------------

glFlushMappedNamedBufferRangeEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> m ()
glFlushMappedNamedBufferRangeEXT v1 v2 v3 = liftIO $ dyn290 ptr_glFlushMappedNamedBufferRangeEXT v1 v2 v3

{-# NOINLINE ptr_glFlushMappedNamedBufferRangeEXT #-}
ptr_glFlushMappedNamedBufferRangeEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedNamedBufferRangeEXT = unsafePerformIO $ getCommand "glFlushMappedNamedBufferRangeEXT"

-- glFlushPixelDataRangeNV -----------------------------------------------------

glFlushPixelDataRangeNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [PixelDataRangeTargetNV](Graphics-GL-Groups.html#PixelDataRangeTargetNV).
  -> m ()
glFlushPixelDataRangeNV v1 = liftIO $ dyn5 ptr_glFlushPixelDataRangeNV v1

{-# NOINLINE ptr_glFlushPixelDataRangeNV #-}
ptr_glFlushPixelDataRangeNV :: FunPtr (GLenum -> IO ())
ptr_glFlushPixelDataRangeNV = unsafePerformIO $ getCommand "glFlushPixelDataRangeNV"

-- glFlushRasterSGIX -----------------------------------------------------------

glFlushRasterSGIX
  :: MonadIO m
  => m ()
glFlushRasterSGIX = liftIO $ dyn11 ptr_glFlushRasterSGIX

{-# NOINLINE ptr_glFlushRasterSGIX #-}
ptr_glFlushRasterSGIX :: FunPtr (IO ())
ptr_glFlushRasterSGIX = unsafePerformIO $ getCommand "glFlushRasterSGIX"

-- glFlushStaticDataIBM --------------------------------------------------------

glFlushStaticDataIBM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glFlushStaticDataIBM v1 = liftIO $ dyn5 ptr_glFlushStaticDataIBM v1

{-# NOINLINE ptr_glFlushStaticDataIBM #-}
ptr_glFlushStaticDataIBM :: FunPtr (GLenum -> IO ())
ptr_glFlushStaticDataIBM = unsafePerformIO $ getCommand "glFlushStaticDataIBM"

-- glFlushVertexArrayRangeAPPLE ------------------------------------------------

glFlushVertexArrayRangeAPPLE
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @length@ elements of type @a@.
  -> m ()
glFlushVertexArrayRangeAPPLE v1 v2 = liftIO $ dyn271 ptr_glFlushVertexArrayRangeAPPLE v1 v2

{-# NOINLINE ptr_glFlushVertexArrayRangeAPPLE #-}
ptr_glFlushVertexArrayRangeAPPLE :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glFlushVertexArrayRangeAPPLE = unsafePerformIO $ getCommand "glFlushVertexArrayRangeAPPLE"

-- glFlushVertexArrayRangeNV ---------------------------------------------------

glFlushVertexArrayRangeNV
  :: MonadIO m
  => m ()
glFlushVertexArrayRangeNV = liftIO $ dyn11 ptr_glFlushVertexArrayRangeNV

{-# NOINLINE ptr_glFlushVertexArrayRangeNV #-}
ptr_glFlushVertexArrayRangeNV :: FunPtr (IO ())
ptr_glFlushVertexArrayRangeNV = unsafePerformIO $ getCommand "glFlushVertexArrayRangeNV"

-- glFogCoordFormatNV ----------------------------------------------------------

glFogCoordFormatNV
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glFogCoordFormatNV v1 v2 = liftIO $ dyn247 ptr_glFogCoordFormatNV v1 v2

{-# NOINLINE ptr_glFogCoordFormatNV #-}
ptr_glFogCoordFormatNV :: FunPtr (GLenum -> GLsizei -> IO ())
ptr_glFogCoordFormatNV = unsafePerformIO $ getCommand "glFogCoordFormatNV"

-- glFogCoordPointer -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoordPointer.xml OpenGL 2.x>.
glFogCoordPointer
  :: MonadIO m
  => GLenum -- ^ @type@ of type [FogPointerTypeEXT](Graphics-GL-Groups.html#FogPointerTypeEXT).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glFogCoordPointer v1 v2 v3 = liftIO $ dyn49 ptr_glFogCoordPointer v1 v2 v3

{-# NOINLINE ptr_glFogCoordPointer #-}
ptr_glFogCoordPointer :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glFogCoordPointer = unsafePerformIO $ getCommand "glFogCoordPointer"

-- glFogCoordPointerEXT --------------------------------------------------------

-- | This command is an alias for 'glFogCoordPointer'.
glFogCoordPointerEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type [FogPointerTypeEXT](Graphics-GL-Groups.html#FogPointerTypeEXT).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glFogCoordPointerEXT v1 v2 v3 = liftIO $ dyn49 ptr_glFogCoordPointerEXT v1 v2 v3

{-# NOINLINE ptr_glFogCoordPointerEXT #-}
ptr_glFogCoordPointerEXT :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glFogCoordPointerEXT = unsafePerformIO $ getCommand "glFogCoordPointerEXT"

-- glFogCoordPointerListIBM ----------------------------------------------------

glFogCoordPointerListIBM
  :: MonadIO m
  => GLenum -- ^ @type@ of type [FogPointerTypeIBM](Graphics-GL-Groups.html#FogPointerTypeIBM).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glFogCoordPointerListIBM v1 v2 v3 v4 = liftIO $ dyn291 ptr_glFogCoordPointerListIBM v1 v2 v3 v4

{-# NOINLINE ptr_glFogCoordPointerListIBM #-}
ptr_glFogCoordPointerListIBM :: FunPtr (GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glFogCoordPointerListIBM = unsafePerformIO $ getCommand "glFogCoordPointerListIBM"

-- glFogCoordd -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glFogCoorddv'.
glFogCoordd
  :: MonadIO m
  => GLdouble -- ^ @coord@ of type @CoordD@.
  -> m ()
glFogCoordd v1 = liftIO $ dyn84 ptr_glFogCoordd v1

{-# NOINLINE ptr_glFogCoordd #-}
ptr_glFogCoordd :: FunPtr (GLdouble -> IO ())
ptr_glFogCoordd = unsafePerformIO $ getCommand "glFogCoordd"

-- glFogCoorddEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glFogCoorddvEXT'. This command is an alias for 'glFogCoordd'.
glFogCoorddEXT
  :: MonadIO m
  => GLdouble -- ^ @coord@ of type @CoordD@.
  -> m ()
glFogCoorddEXT v1 = liftIO $ dyn84 ptr_glFogCoorddEXT v1

{-# NOINLINE ptr_glFogCoorddEXT #-}
ptr_glFogCoorddEXT :: FunPtr (GLdouble -> IO ())
ptr_glFogCoorddEXT = unsafePerformIO $ getCommand "glFogCoorddEXT"

-- glFogCoorddv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>.
glFogCoorddv
  :: MonadIO m
  => Ptr GLdouble -- ^ @coord@ pointing to @1@ element of type @CoordD@.
  -> m ()
glFogCoorddv v1 = liftIO $ dyn42 ptr_glFogCoorddv v1

{-# NOINLINE ptr_glFogCoorddv #-}
ptr_glFogCoorddv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glFogCoorddv = unsafePerformIO $ getCommand "glFogCoorddv"

-- glFogCoorddvEXT -------------------------------------------------------------

-- | This command is an alias for 'glFogCoorddv'.
glFogCoorddvEXT
  :: MonadIO m
  => Ptr GLdouble -- ^ @coord@ pointing to @1@ element of type @CoordD@.
  -> m ()
glFogCoorddvEXT v1 = liftIO $ dyn42 ptr_glFogCoorddvEXT v1

{-# NOINLINE ptr_glFogCoorddvEXT #-}
ptr_glFogCoorddvEXT :: FunPtr (Ptr GLdouble -> IO ())
ptr_glFogCoorddvEXT = unsafePerformIO $ getCommand "glFogCoorddvEXT"

-- glFogCoordf -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glFogCoordfv'.
glFogCoordf
  :: MonadIO m
  => GLfloat -- ^ @coord@ of type @CoordF@.
  -> m ()
glFogCoordf v1 = liftIO $ dyn85 ptr_glFogCoordf v1

{-# NOINLINE ptr_glFogCoordf #-}
ptr_glFogCoordf :: FunPtr (GLfloat -> IO ())
ptr_glFogCoordf = unsafePerformIO $ getCommand "glFogCoordf"

-- glFogCoordfEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glFogCoordfvEXT'. This command is an alias for 'glFogCoordf'.
glFogCoordfEXT
  :: MonadIO m
  => GLfloat -- ^ @coord@ of type @CoordF@.
  -> m ()
glFogCoordfEXT v1 = liftIO $ dyn85 ptr_glFogCoordfEXT v1

{-# NOINLINE ptr_glFogCoordfEXT #-}
ptr_glFogCoordfEXT :: FunPtr (GLfloat -> IO ())
ptr_glFogCoordfEXT = unsafePerformIO $ getCommand "glFogCoordfEXT"

-- glFogCoordfv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>.
glFogCoordfv
  :: MonadIO m
  => Ptr GLfloat -- ^ @coord@ pointing to @1@ element of type @CoordF@.
  -> m ()
glFogCoordfv v1 = liftIO $ dyn44 ptr_glFogCoordfv v1

{-# NOINLINE ptr_glFogCoordfv #-}
ptr_glFogCoordfv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glFogCoordfv = unsafePerformIO $ getCommand "glFogCoordfv"

-- glFogCoordfvEXT -------------------------------------------------------------

-- | This command is an alias for 'glFogCoordfv'.
glFogCoordfvEXT
  :: MonadIO m
  => Ptr GLfloat -- ^ @coord@ pointing to @1@ element of type @CoordF@.
  -> m ()
glFogCoordfvEXT v1 = liftIO $ dyn44 ptr_glFogCoordfvEXT v1

{-# NOINLINE ptr_glFogCoordfvEXT #-}
ptr_glFogCoordfvEXT :: FunPtr (Ptr GLfloat -> IO ())
ptr_glFogCoordfvEXT = unsafePerformIO $ getCommand "glFogCoordfvEXT"

-- glFogCoordhNV ---------------------------------------------------------------

-- | The vector equivalent of this command is 'glFogCoordhvNV'.
glFogCoordhNV
  :: MonadIO m
  => GLhalfNV -- ^ @fog@ of type @Half16NV@.
  -> m ()
glFogCoordhNV v1 = liftIO $ dyn292 ptr_glFogCoordhNV v1

{-# NOINLINE ptr_glFogCoordhNV #-}
ptr_glFogCoordhNV :: FunPtr (GLhalfNV -> IO ())
ptr_glFogCoordhNV = unsafePerformIO $ getCommand "glFogCoordhNV"

-- glFogCoordhvNV --------------------------------------------------------------

glFogCoordhvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @fog@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glFogCoordhvNV v1 = liftIO $ dyn106 ptr_glFogCoordhvNV v1

{-# NOINLINE ptr_glFogCoordhvNV #-}
ptr_glFogCoordhvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glFogCoordhvNV = unsafePerformIO $ getCommand "glFogCoordhvNV"

-- glFogFuncSGIS ---------------------------------------------------------------

glFogFuncSGIS
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @points@ pointing to @n*2@ elements of type @GLfloat@.
  -> m ()
glFogFuncSGIS v1 v2 = liftIO $ dyn199 ptr_glFogFuncSGIS v1 v2

{-# NOINLINE ptr_glFogFuncSGIS #-}
ptr_glFogFuncSGIS :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
ptr_glFogFuncSGIS = unsafePerformIO $ getCommand "glFogFuncSGIS"

-- glFogf ----------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogf
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFogf v1 v2 = liftIO $ dyn0 ptr_glFogf v1 v2

{-# NOINLINE ptr_glFogf #-}
ptr_glFogf :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glFogf = unsafePerformIO $ getCommand "glFogf"

-- glFogfv ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogfv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFogfv v1 v2 = liftIO $ dyn101 ptr_glFogfv v1 v2

{-# NOINLINE ptr_glFogfv #-}
ptr_glFogfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glFogfv = unsafePerformIO $ getCommand "glFogfv"

-- glFogi ----------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogi
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFogi v1 v2 = liftIO $ dyn58 ptr_glFogi v1 v2

{-# NOINLINE ptr_glFogi #-}
ptr_glFogi :: FunPtr (GLenum -> GLint -> IO ())
ptr_glFogi = unsafePerformIO $ getCommand "glFogi"

-- glFogiv ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogiv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFogiv v1 v2 = liftIO $ dyn143 ptr_glFogiv v1 v2

{-# NOINLINE ptr_glFogiv #-}
ptr_glFogiv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glFogiv = unsafePerformIO $ getCommand "glFogiv"

-- glFogx ----------------------------------------------------------------------

glFogx
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogPName](Graphics-GL-Groups.html#FogPName).
  -> GLfixed -- ^ @param@.
  -> m ()
glFogx v1 v2 = liftIO $ dyn1 ptr_glFogx v1 v2

{-# NOINLINE ptr_glFogx #-}
ptr_glFogx :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glFogx = unsafePerformIO $ getCommand "glFogx"

-- glFogxOES -------------------------------------------------------------------

glFogxOES
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogPName](Graphics-GL-Groups.html#FogPName).
  -> GLfixed -- ^ @param@.
  -> m ()
glFogxOES v1 v2 = liftIO $ dyn1 ptr_glFogxOES v1 v2

{-# NOINLINE ptr_glFogxOES #-}
ptr_glFogxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glFogxOES = unsafePerformIO $ getCommand "glFogxOES"

-- glFogxv ---------------------------------------------------------------------

glFogxv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogPName](Graphics-GL-Groups.html#FogPName).
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glFogxv v1 v2 = liftIO $ dyn102 ptr_glFogxv v1 v2

{-# NOINLINE ptr_glFogxv #-}
ptr_glFogxv :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glFogxv = unsafePerformIO $ getCommand "glFogxv"

-- glFogxvOES ------------------------------------------------------------------

glFogxvOES
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogPName](Graphics-GL-Groups.html#FogPName).
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glFogxvOES v1 v2 = liftIO $ dyn102 ptr_glFogxvOES v1 v2

{-# NOINLINE ptr_glFogxvOES #-}
ptr_glFogxvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glFogxvOES = unsafePerformIO $ getCommand "glFogxvOES"

-- glFragmentColorMaterialSGIX -------------------------------------------------

glFragmentColorMaterialSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> m ()
glFragmentColorMaterialSGIX v1 v2 = liftIO $ dyn54 ptr_glFragmentColorMaterialSGIX v1 v2

{-# NOINLINE ptr_glFragmentColorMaterialSGIX #-}
ptr_glFragmentColorMaterialSGIX :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glFragmentColorMaterialSGIX = unsafePerformIO $ getCommand "glFragmentColorMaterialSGIX"

-- glFragmentCoverageColorNV ---------------------------------------------------

glFragmentCoverageColorNV
  :: MonadIO m
  => GLuint -- ^ @color@.
  -> m ()
glFragmentCoverageColorNV v1 = liftIO $ dyn3 ptr_glFragmentCoverageColorNV v1

{-# NOINLINE ptr_glFragmentCoverageColorNV #-}
ptr_glFragmentCoverageColorNV :: FunPtr (GLuint -> IO ())
ptr_glFragmentCoverageColorNV = unsafePerformIO $ getCommand "glFragmentCoverageColorNV"

-- glFragmentLightModelfSGIX ---------------------------------------------------

glFragmentLightModelfSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFragmentLightModelfSGIX v1 v2 = liftIO $ dyn0 ptr_glFragmentLightModelfSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModelfSGIX #-}
ptr_glFragmentLightModelfSGIX :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glFragmentLightModelfSGIX = unsafePerformIO $ getCommand "glFragmentLightModelfSGIX"

-- glFragmentLightModelfvSGIX --------------------------------------------------

glFragmentLightModelfvSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFragmentLightModelfvSGIX v1 v2 = liftIO $ dyn101 ptr_glFragmentLightModelfvSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModelfvSGIX #-}
ptr_glFragmentLightModelfvSGIX :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glFragmentLightModelfvSGIX = unsafePerformIO $ getCommand "glFragmentLightModelfvSGIX"

-- glFragmentLightModeliSGIX ---------------------------------------------------

glFragmentLightModeliSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFragmentLightModeliSGIX v1 v2 = liftIO $ dyn58 ptr_glFragmentLightModeliSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModeliSGIX #-}
ptr_glFragmentLightModeliSGIX :: FunPtr (GLenum -> GLint -> IO ())
ptr_glFragmentLightModeliSGIX = unsafePerformIO $ getCommand "glFragmentLightModeliSGIX"

-- glFragmentLightModelivSGIX --------------------------------------------------

glFragmentLightModelivSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFragmentLightModelivSGIX v1 v2 = liftIO $ dyn143 ptr_glFragmentLightModelivSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModelivSGIX #-}
ptr_glFragmentLightModelivSGIX :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glFragmentLightModelivSGIX = unsafePerformIO $ getCommand "glFragmentLightModelivSGIX"

-- glFragmentLightfSGIX --------------------------------------------------------

glFragmentLightfSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type [FragmentLightNameSGIX](Graphics-GL-Groups.html#FragmentLightNameSGIX).
  -> GLenum -- ^ @pname@ of type [FragmentLightParameterSGIX](Graphics-GL-Groups.html#FragmentLightParameterSGIX).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFragmentLightfSGIX v1 v2 v3 = liftIO $ dyn168 ptr_glFragmentLightfSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightfSGIX #-}
ptr_glFragmentLightfSGIX :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glFragmentLightfSGIX = unsafePerformIO $ getCommand "glFragmentLightfSGIX"

-- glFragmentLightfvSGIX -------------------------------------------------------

glFragmentLightfvSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type [FragmentLightNameSGIX](Graphics-GL-Groups.html#FragmentLightNameSGIX).
  -> GLenum -- ^ @pname@ of type [FragmentLightParameterSGIX](Graphics-GL-Groups.html#FragmentLightParameterSGIX).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFragmentLightfvSGIX v1 v2 v3 = liftIO $ dyn139 ptr_glFragmentLightfvSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightfvSGIX #-}
ptr_glFragmentLightfvSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glFragmentLightfvSGIX = unsafePerformIO $ getCommand "glFragmentLightfvSGIX"

-- glFragmentLightiSGIX --------------------------------------------------------

glFragmentLightiSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type [FragmentLightNameSGIX](Graphics-GL-Groups.html#FragmentLightNameSGIX).
  -> GLenum -- ^ @pname@ of type [FragmentLightParameterSGIX](Graphics-GL-Groups.html#FragmentLightParameterSGIX).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFragmentLightiSGIX v1 v2 v3 = liftIO $ dyn66 ptr_glFragmentLightiSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightiSGIX #-}
ptr_glFragmentLightiSGIX :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glFragmentLightiSGIX = unsafePerformIO $ getCommand "glFragmentLightiSGIX"

-- glFragmentLightivSGIX -------------------------------------------------------

glFragmentLightivSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type [FragmentLightNameSGIX](Graphics-GL-Groups.html#FragmentLightNameSGIX).
  -> GLenum -- ^ @pname@ of type [FragmentLightParameterSGIX](Graphics-GL-Groups.html#FragmentLightParameterSGIX).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFragmentLightivSGIX v1 v2 v3 = liftIO $ dyn140 ptr_glFragmentLightivSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightivSGIX #-}
ptr_glFragmentLightivSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glFragmentLightivSGIX = unsafePerformIO $ getCommand "glFragmentLightivSGIX"

-- glFragmentMaterialfSGIX -----------------------------------------------------

glFragmentMaterialfSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFragmentMaterialfSGIX v1 v2 v3 = liftIO $ dyn168 ptr_glFragmentMaterialfSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialfSGIX #-}
ptr_glFragmentMaterialfSGIX :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glFragmentMaterialfSGIX = unsafePerformIO $ getCommand "glFragmentMaterialfSGIX"

-- glFragmentMaterialfvSGIX ----------------------------------------------------

glFragmentMaterialfvSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFragmentMaterialfvSGIX v1 v2 v3 = liftIO $ dyn139 ptr_glFragmentMaterialfvSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialfvSGIX #-}
ptr_glFragmentMaterialfvSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glFragmentMaterialfvSGIX = unsafePerformIO $ getCommand "glFragmentMaterialfvSGIX"

-- glFragmentMaterialiSGIX -----------------------------------------------------

glFragmentMaterialiSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFragmentMaterialiSGIX v1 v2 v3 = liftIO $ dyn66 ptr_glFragmentMaterialiSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialiSGIX #-}
ptr_glFragmentMaterialiSGIX :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glFragmentMaterialiSGIX = unsafePerformIO $ getCommand "glFragmentMaterialiSGIX"

-- glFragmentMaterialivSGIX ----------------------------------------------------

glFragmentMaterialivSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFragmentMaterialivSGIX v1 v2 v3 = liftIO $ dyn140 ptr_glFragmentMaterialivSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialivSGIX #-}
ptr_glFragmentMaterialivSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glFragmentMaterialivSGIX = unsafePerformIO $ getCommand "glFragmentMaterialivSGIX"

-- glFrameTerminatorGREMEDY ----------------------------------------------------

glFrameTerminatorGREMEDY
  :: MonadIO m
  => m ()
glFrameTerminatorGREMEDY = liftIO $ dyn11 ptr_glFrameTerminatorGREMEDY

{-# NOINLINE ptr_glFrameTerminatorGREMEDY #-}
ptr_glFrameTerminatorGREMEDY :: FunPtr (IO ())
ptr_glFrameTerminatorGREMEDY = unsafePerformIO $ getCommand "glFrameTerminatorGREMEDY"

-- glFrameZoomSGIX -------------------------------------------------------------

glFrameZoomSGIX
  :: MonadIO m
  => GLint -- ^ @factor@ of type @CheckedInt32@.
  -> m ()
glFrameZoomSGIX v1 = liftIO $ dyn13 ptr_glFrameZoomSGIX v1

{-# NOINLINE ptr_glFrameZoomSGIX #-}
ptr_glFrameZoomSGIX :: FunPtr (GLint -> IO ())
ptr_glFrameZoomSGIX = unsafePerformIO $ getCommand "glFrameZoomSGIX"

-- glFramebufferDrawBufferEXT --------------------------------------------------

glFramebufferDrawBufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @mode@ of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glFramebufferDrawBufferEXT v1 v2 = liftIO $ dyn18 ptr_glFramebufferDrawBufferEXT v1 v2

{-# NOINLINE ptr_glFramebufferDrawBufferEXT #-}
ptr_glFramebufferDrawBufferEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glFramebufferDrawBufferEXT = unsafePerformIO $ getCommand "glFramebufferDrawBufferEXT"

-- glFramebufferDrawBuffersEXT -------------------------------------------------

glFramebufferDrawBuffersEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glFramebufferDrawBuffersEXT v1 v2 v3 = liftIO $ dyn293 ptr_glFramebufferDrawBuffersEXT v1 v2 v3

{-# NOINLINE ptr_glFramebufferDrawBuffersEXT #-}
ptr_glFramebufferDrawBuffersEXT :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ())
ptr_glFramebufferDrawBuffersEXT = unsafePerformIO $ getCommand "glFramebufferDrawBuffersEXT"

-- glFramebufferFetchBarrierEXT ------------------------------------------------

glFramebufferFetchBarrierEXT
  :: MonadIO m
  => m ()
glFramebufferFetchBarrierEXT = liftIO $ dyn11 ptr_glFramebufferFetchBarrierEXT

{-# NOINLINE ptr_glFramebufferFetchBarrierEXT #-}
ptr_glFramebufferFetchBarrierEXT :: FunPtr (IO ())
ptr_glFramebufferFetchBarrierEXT = unsafePerformIO $ getCommand "glFramebufferFetchBarrierEXT"

-- glFramebufferFetchBarrierQCOM -----------------------------------------------

glFramebufferFetchBarrierQCOM
  :: MonadIO m
  => m ()
glFramebufferFetchBarrierQCOM = liftIO $ dyn11 ptr_glFramebufferFetchBarrierQCOM

{-# NOINLINE ptr_glFramebufferFetchBarrierQCOM #-}
ptr_glFramebufferFetchBarrierQCOM :: FunPtr (IO ())
ptr_glFramebufferFetchBarrierQCOM = unsafePerformIO $ getCommand "glFramebufferFetchBarrierQCOM"

-- glFramebufferFoveationConfigQCOM --------------------------------------------

glFramebufferFoveationConfigQCOM
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLuint -- ^ @numLayers@.
  -> GLuint -- ^ @focalPointsPerLayer@.
  -> GLuint -- ^ @requestedFeatures@.
  -> Ptr GLuint -- ^ @providedFeatures@ pointing to @1@ element of type @GLuint@.
  -> m ()
glFramebufferFoveationConfigQCOM v1 v2 v3 v4 v5 = liftIO $ dyn294 ptr_glFramebufferFoveationConfigQCOM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferFoveationConfigQCOM #-}
ptr_glFramebufferFoveationConfigQCOM :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> Ptr GLuint -> IO ())
ptr_glFramebufferFoveationConfigQCOM = unsafePerformIO $ getCommand "glFramebufferFoveationConfigQCOM"

-- glFramebufferFoveationParametersQCOM ----------------------------------------

glFramebufferFoveationParametersQCOM
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLuint -- ^ @layer@.
  -> GLuint -- ^ @focalPoint@.
  -> GLfloat -- ^ @focalX@ of type @CheckedFloat32@.
  -> GLfloat -- ^ @focalY@ of type @CheckedFloat32@.
  -> GLfloat -- ^ @gainX@ of type @CheckedFloat32@.
  -> GLfloat -- ^ @gainY@ of type @CheckedFloat32@.
  -> GLfloat -- ^ @foveaArea@ of type @CheckedFloat32@.
  -> m ()
glFramebufferFoveationParametersQCOM v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn295 ptr_glFramebufferFoveationParametersQCOM v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glFramebufferFoveationParametersQCOM #-}
ptr_glFramebufferFoveationParametersQCOM :: FunPtr (GLuint -> GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glFramebufferFoveationParametersQCOM = unsafePerformIO $ getCommand "glFramebufferFoveationParametersQCOM"

-- glFramebufferParameteri -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferParameteri.xhtml OpenGL 4.x>.
glFramebufferParameteri
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @pname@ of type [FramebufferParameterName](Graphics-GL-Groups.html#FramebufferParameterName).
  -> GLint -- ^ @param@.
  -> m ()
glFramebufferParameteri v1 v2 v3 = liftIO $ dyn66 ptr_glFramebufferParameteri v1 v2 v3

{-# NOINLINE ptr_glFramebufferParameteri #-}
ptr_glFramebufferParameteri :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glFramebufferParameteri = unsafePerformIO $ getCommand "glFramebufferParameteri"

-- glFramebufferParameteriMESA -------------------------------------------------

glFramebufferParameteriMESA
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @pname@ of type [FramebufferParameterName](Graphics-GL-Groups.html#FramebufferParameterName).
  -> GLint -- ^ @param@.
  -> m ()
glFramebufferParameteriMESA v1 v2 v3 = liftIO $ dyn66 ptr_glFramebufferParameteriMESA v1 v2 v3

{-# NOINLINE ptr_glFramebufferParameteriMESA #-}
ptr_glFramebufferParameteriMESA :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glFramebufferParameteriMESA = unsafePerformIO $ getCommand "glFramebufferParameteriMESA"

-- glFramebufferPixelLocalStorageSizeEXT ---------------------------------------

glFramebufferPixelLocalStorageSizeEXT
  :: MonadIO m
  => GLuint -- ^ @target@.
  -> GLsizei -- ^ @size@.
  -> m ()
glFramebufferPixelLocalStorageSizeEXT v1 v2 = liftIO $ dyn219 ptr_glFramebufferPixelLocalStorageSizeEXT v1 v2

{-# NOINLINE ptr_glFramebufferPixelLocalStorageSizeEXT #-}
ptr_glFramebufferPixelLocalStorageSizeEXT :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glFramebufferPixelLocalStorageSizeEXT = unsafePerformIO $ getCommand "glFramebufferPixelLocalStorageSizeEXT"

-- glFramebufferReadBufferEXT --------------------------------------------------

glFramebufferReadBufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @mode@ of type [ReadBufferMode](Graphics-GL-Groups.html#ReadBufferMode).
  -> m ()
glFramebufferReadBufferEXT v1 v2 = liftIO $ dyn18 ptr_glFramebufferReadBufferEXT v1 v2

{-# NOINLINE ptr_glFramebufferReadBufferEXT #-}
ptr_glFramebufferReadBufferEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glFramebufferReadBufferEXT = unsafePerformIO $ getCommand "glFramebufferReadBufferEXT"

-- glFramebufferRenderbuffer ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferRenderbuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferRenderbuffer.xhtml OpenGL 4.x>.
glFramebufferRenderbuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @renderbuffertarget@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glFramebufferRenderbuffer v1 v2 v3 v4 = liftIO $ dyn296 ptr_glFramebufferRenderbuffer v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferRenderbuffer #-}
ptr_glFramebufferRenderbuffer :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glFramebufferRenderbuffer = unsafePerformIO $ getCommand "glFramebufferRenderbuffer"

-- glFramebufferRenderbufferEXT ------------------------------------------------

-- | This command is an alias for 'glFramebufferRenderbuffer'.
glFramebufferRenderbufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @renderbuffertarget@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glFramebufferRenderbufferEXT v1 v2 v3 v4 = liftIO $ dyn296 ptr_glFramebufferRenderbufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferRenderbufferEXT #-}
ptr_glFramebufferRenderbufferEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glFramebufferRenderbufferEXT = unsafePerformIO $ getCommand "glFramebufferRenderbufferEXT"

-- glFramebufferRenderbufferOES ------------------------------------------------

glFramebufferRenderbufferOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @renderbuffertarget@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glFramebufferRenderbufferOES v1 v2 v3 v4 = liftIO $ dyn296 ptr_glFramebufferRenderbufferOES v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferRenderbufferOES #-}
ptr_glFramebufferRenderbufferOES :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glFramebufferRenderbufferOES = unsafePerformIO $ getCommand "glFramebufferRenderbufferOES"

-- glFramebufferSampleLocationsfvARB -------------------------------------------

glFramebufferSampleLocationsfvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glFramebufferSampleLocationsfvARB v1 v2 v3 v4 = liftIO $ dyn297 ptr_glFramebufferSampleLocationsfvARB v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferSampleLocationsfvARB #-}
ptr_glFramebufferSampleLocationsfvARB :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glFramebufferSampleLocationsfvARB = unsafePerformIO $ getCommand "glFramebufferSampleLocationsfvARB"

-- glFramebufferSampleLocationsfvNV --------------------------------------------

glFramebufferSampleLocationsfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glFramebufferSampleLocationsfvNV v1 v2 v3 v4 = liftIO $ dyn297 ptr_glFramebufferSampleLocationsfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferSampleLocationsfvNV #-}
ptr_glFramebufferSampleLocationsfvNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glFramebufferSampleLocationsfvNV = unsafePerformIO $ getCommand "glFramebufferSampleLocationsfvNV"

-- glFramebufferSamplePositionsfvAMD -------------------------------------------

glFramebufferSamplePositionsfvAMD
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLuint -- ^ @numsamples@.
  -> GLuint -- ^ @pixelindex@.
  -> Ptr GLfloat -- ^ @values@.
  -> m ()
glFramebufferSamplePositionsfvAMD v1 v2 v3 v4 = liftIO $ dyn298 ptr_glFramebufferSamplePositionsfvAMD v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferSamplePositionsfvAMD #-}
ptr_glFramebufferSamplePositionsfvAMD :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLfloat -> IO ())
ptr_glFramebufferSamplePositionsfvAMD = unsafePerformIO $ getCommand "glFramebufferSamplePositionsfvAMD"

-- glFramebufferTexture --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture v1 v2 v3 v4 = liftIO $ dyn299 ptr_glFramebufferTexture v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTexture #-}
ptr_glFramebufferTexture :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture = unsafePerformIO $ getCommand "glFramebufferTexture"

-- glFramebufferTexture1D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture1D v1 v2 v3 v4 v5 = liftIO $ dyn300 ptr_glFramebufferTexture1D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture1D #-}
ptr_glFramebufferTexture1D :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture1D = unsafePerformIO $ getCommand "glFramebufferTexture1D"

-- glFramebufferTexture1DEXT ---------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture1D'.
glFramebufferTexture1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture1DEXT v1 v2 v3 v4 v5 = liftIO $ dyn300 ptr_glFramebufferTexture1DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture1DEXT #-}
ptr_glFramebufferTexture1DEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture1DEXT = unsafePerformIO $ getCommand "glFramebufferTexture1DEXT"

-- glFramebufferTexture2D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture2D v1 v2 v3 v4 v5 = liftIO $ dyn300 ptr_glFramebufferTexture2D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture2D #-}
ptr_glFramebufferTexture2D :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture2D = unsafePerformIO $ getCommand "glFramebufferTexture2D"

-- glFramebufferTexture2DDownsampleIMG -----------------------------------------

glFramebufferTexture2DDownsampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xscale@.
  -> GLint -- ^ @yscale@.
  -> m ()
glFramebufferTexture2DDownsampleIMG v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn301 ptr_glFramebufferTexture2DDownsampleIMG v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glFramebufferTexture2DDownsampleIMG #-}
ptr_glFramebufferTexture2DDownsampleIMG :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture2DDownsampleIMG = unsafePerformIO $ getCommand "glFramebufferTexture2DDownsampleIMG"

-- glFramebufferTexture2DEXT ---------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture2D'.
glFramebufferTexture2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture2DEXT v1 v2 v3 v4 v5 = liftIO $ dyn300 ptr_glFramebufferTexture2DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture2DEXT #-}
ptr_glFramebufferTexture2DEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture2DEXT = unsafePerformIO $ getCommand "glFramebufferTexture2DEXT"

-- glFramebufferTexture2DMultisampleEXT ----------------------------------------

glFramebufferTexture2DMultisampleEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLsizei -- ^ @samples@.
  -> m ()
glFramebufferTexture2DMultisampleEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn302 ptr_glFramebufferTexture2DMultisampleEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture2DMultisampleEXT #-}
ptr_glFramebufferTexture2DMultisampleEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTexture2DMultisampleEXT = unsafePerformIO $ getCommand "glFramebufferTexture2DMultisampleEXT"

-- glFramebufferTexture2DMultisampleIMG ----------------------------------------

glFramebufferTexture2DMultisampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLsizei -- ^ @samples@.
  -> m ()
glFramebufferTexture2DMultisampleIMG v1 v2 v3 v4 v5 v6 = liftIO $ dyn302 ptr_glFramebufferTexture2DMultisampleIMG v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture2DMultisampleIMG #-}
ptr_glFramebufferTexture2DMultisampleIMG :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTexture2DMultisampleIMG = unsafePerformIO $ getCommand "glFramebufferTexture2DMultisampleIMG"

-- glFramebufferTexture2DOES ---------------------------------------------------

glFramebufferTexture2DOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture2DOES v1 v2 v3 v4 v5 = liftIO $ dyn300 ptr_glFramebufferTexture2DOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture2DOES #-}
ptr_glFramebufferTexture2DOES :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture2DOES = unsafePerformIO $ getCommand "glFramebufferTexture2DOES"

-- glFramebufferTexture3D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @zoffset@.
  -> m ()
glFramebufferTexture3D v1 v2 v3 v4 v5 v6 = liftIO $ dyn303 ptr_glFramebufferTexture3D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture3D #-}
ptr_glFramebufferTexture3D :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture3D = unsafePerformIO $ getCommand "glFramebufferTexture3D"

-- glFramebufferTexture3DEXT ---------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture3D'.
glFramebufferTexture3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @zoffset@.
  -> m ()
glFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn303 ptr_glFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture3DEXT #-}
ptr_glFramebufferTexture3DEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture3DEXT = unsafePerformIO $ getCommand "glFramebufferTexture3DEXT"

-- glFramebufferTexture3DOES ---------------------------------------------------

glFramebufferTexture3DOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @zoffset@.
  -> m ()
glFramebufferTexture3DOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn303 ptr_glFramebufferTexture3DOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture3DOES #-}
ptr_glFramebufferTexture3DOES :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture3DOES = unsafePerformIO $ getCommand "glFramebufferTexture3DOES"

-- glFramebufferTextureARB -----------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture'.
glFramebufferTextureARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureARB v1 v2 v3 v4 = liftIO $ dyn299 ptr_glFramebufferTextureARB v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTextureARB #-}
ptr_glFramebufferTextureARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTextureARB = unsafePerformIO $ getCommand "glFramebufferTextureARB"

-- glFramebufferTextureEXT -----------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture'.
glFramebufferTextureEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureEXT v1 v2 v3 v4 = liftIO $ dyn299 ptr_glFramebufferTextureEXT v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTextureEXT #-}
ptr_glFramebufferTextureEXT :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTextureEXT = unsafePerformIO $ getCommand "glFramebufferTextureEXT"

-- glFramebufferTextureFaceARB -------------------------------------------------

glFramebufferTextureFaceARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @face@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glFramebufferTextureFaceARB v1 v2 v3 v4 v5 = liftIO $ dyn304 ptr_glFramebufferTextureFaceARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureFaceARB #-}
ptr_glFramebufferTextureFaceARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
ptr_glFramebufferTextureFaceARB = unsafePerformIO $ getCommand "glFramebufferTextureFaceARB"

-- glFramebufferTextureFaceEXT -------------------------------------------------

-- | This command is an alias for 'glFramebufferTextureFaceARB'.
glFramebufferTextureFaceEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @face@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glFramebufferTextureFaceEXT v1 v2 v3 v4 v5 = liftIO $ dyn304 ptr_glFramebufferTextureFaceEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureFaceEXT #-}
ptr_glFramebufferTextureFaceEXT :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
ptr_glFramebufferTextureFaceEXT = unsafePerformIO $ getCommand "glFramebufferTextureFaceEXT"

-- glFramebufferTextureLayer ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTextureLayer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTextureLayer.xhtml OpenGL 4.x>.
glFramebufferTextureLayer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureLayer v1 v2 v3 v4 v5 = liftIO $ dyn305 ptr_glFramebufferTextureLayer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureLayer #-}
ptr_glFramebufferTextureLayer :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayer = unsafePerformIO $ getCommand "glFramebufferTextureLayer"

-- glFramebufferTextureLayerARB ------------------------------------------------

-- | This command is an alias for 'glFramebufferTextureLayer'.
glFramebufferTextureLayerARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureLayerARB v1 v2 v3 v4 v5 = liftIO $ dyn305 ptr_glFramebufferTextureLayerARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureLayerARB #-}
ptr_glFramebufferTextureLayerARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayerARB = unsafePerformIO $ getCommand "glFramebufferTextureLayerARB"

-- glFramebufferTextureLayerDownsampleIMG --------------------------------------

glFramebufferTextureLayerDownsampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> GLint -- ^ @xscale@.
  -> GLint -- ^ @yscale@.
  -> m ()
glFramebufferTextureLayerDownsampleIMG v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn306 ptr_glFramebufferTextureLayerDownsampleIMG v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glFramebufferTextureLayerDownsampleIMG #-}
ptr_glFramebufferTextureLayerDownsampleIMG :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayerDownsampleIMG = unsafePerformIO $ getCommand "glFramebufferTextureLayerDownsampleIMG"

-- glFramebufferTextureLayerEXT ------------------------------------------------

-- | This command is an alias for 'glFramebufferTextureLayer'.
glFramebufferTextureLayerEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureLayerEXT v1 v2 v3 v4 v5 = liftIO $ dyn305 ptr_glFramebufferTextureLayerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureLayerEXT #-}
ptr_glFramebufferTextureLayerEXT :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayerEXT = unsafePerformIO $ getCommand "glFramebufferTextureLayerEXT"

-- glFramebufferTextureMultisampleMultiviewOVR ---------------------------------

glFramebufferTextureMultisampleMultiviewOVR
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLsizei -- ^ @samples@.
  -> GLint -- ^ @baseViewIndex@.
  -> GLsizei -- ^ @numViews@.
  -> m ()
glFramebufferTextureMultisampleMultiviewOVR v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn307 ptr_glFramebufferTextureMultisampleMultiviewOVR v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glFramebufferTextureMultisampleMultiviewOVR #-}
ptr_glFramebufferTextureMultisampleMultiviewOVR :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTextureMultisampleMultiviewOVR = unsafePerformIO $ getCommand "glFramebufferTextureMultisampleMultiviewOVR"

-- glFramebufferTextureMultiviewOVR --------------------------------------------

glFramebufferTextureMultiviewOVR
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @baseViewIndex@.
  -> GLsizei -- ^ @numViews@.
  -> m ()
glFramebufferTextureMultiviewOVR v1 v2 v3 v4 v5 v6 = liftIO $ dyn308 ptr_glFramebufferTextureMultiviewOVR v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTextureMultiviewOVR #-}
ptr_glFramebufferTextureMultiviewOVR :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTextureMultiviewOVR = unsafePerformIO $ getCommand "glFramebufferTextureMultiviewOVR"

-- glFramebufferTextureOES -----------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture'.
glFramebufferTextureOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureOES v1 v2 v3 v4 = liftIO $ dyn299 ptr_glFramebufferTextureOES v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTextureOES #-}
ptr_glFramebufferTextureOES :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTextureOES = unsafePerformIO $ getCommand "glFramebufferTextureOES"

-- glFreeObjectBufferATI -------------------------------------------------------

glFreeObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m ()
glFreeObjectBufferATI v1 = liftIO $ dyn3 ptr_glFreeObjectBufferATI v1

{-# NOINLINE ptr_glFreeObjectBufferATI #-}
ptr_glFreeObjectBufferATI :: FunPtr (GLuint -> IO ())
ptr_glFreeObjectBufferATI = unsafePerformIO $ getCommand "glFreeObjectBufferATI"

-- glFrontFace -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glFrontFace.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glFrontFace.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFrontFace.xhtml OpenGL 4.x>.
glFrontFace
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [FrontFaceDirection](Graphics-GL-Groups.html#FrontFaceDirection).
  -> m ()
glFrontFace v1 = liftIO $ dyn5 ptr_glFrontFace v1

{-# NOINLINE ptr_glFrontFace #-}
ptr_glFrontFace :: FunPtr (GLenum -> IO ())
ptr_glFrontFace = unsafePerformIO $ getCommand "glFrontFace"

-- glFrustum -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFrustum.xml OpenGL 2.x>.
glFrustum
  :: MonadIO m
  => GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glFrustum v1 v2 v3 v4 v5 v6 = liftIO $ dyn309 ptr_glFrustum v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustum #-}
ptr_glFrustum :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glFrustum = unsafePerformIO $ getCommand "glFrustum"

-- glFrustumf ------------------------------------------------------------------

glFrustumf
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glFrustumf v1 v2 v3 v4 v5 v6 = liftIO $ dyn103 ptr_glFrustumf v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumf #-}
ptr_glFrustumf :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glFrustumf = unsafePerformIO $ getCommand "glFrustumf"

-- glFrustumfOES ---------------------------------------------------------------

glFrustumfOES
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glFrustumfOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn103 ptr_glFrustumfOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumfOES #-}
ptr_glFrustumfOES :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glFrustumfOES = unsafePerformIO $ getCommand "glFrustumfOES"

-- glFrustumx ------------------------------------------------------------------

glFrustumx
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glFrustumx v1 v2 v3 v4 v5 v6 = liftIO $ dyn310 ptr_glFrustumx v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumx #-}
ptr_glFrustumx :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glFrustumx = unsafePerformIO $ getCommand "glFrustumx"

-- glFrustumxOES ---------------------------------------------------------------

glFrustumxOES
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glFrustumxOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn310 ptr_glFrustumxOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumxOES #-}
ptr_glFrustumxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glFrustumxOES = unsafePerformIO $ getCommand "glFrustumxOES"

-- glGenAsyncMarkersSGIX -------------------------------------------------------

glGenAsyncMarkersSGIX
  :: MonadIO m
  => GLsizei -- ^ @range@.
  -> m GLuint
glGenAsyncMarkersSGIX v1 = liftIO $ dyn311 ptr_glGenAsyncMarkersSGIX v1

{-# NOINLINE ptr_glGenAsyncMarkersSGIX #-}
ptr_glGenAsyncMarkersSGIX :: FunPtr (GLsizei -> IO GLuint)
ptr_glGenAsyncMarkersSGIX = unsafePerformIO $ getCommand "glGenAsyncMarkersSGIX"

-- glGenBuffers ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenBuffers.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGenBuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenBuffers.xhtml OpenGL 4.x>.
glGenBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenBuffers v1 v2 = liftIO $ dyn200 ptr_glGenBuffers v1 v2

{-# NOINLINE ptr_glGenBuffers #-}
ptr_glGenBuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenBuffers = unsafePerformIO $ getCommand "glGenBuffers"

-- glGenBuffersARB -------------------------------------------------------------

-- | This command is an alias for 'glGenBuffers'.
glGenBuffersARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenBuffersARB v1 v2 = liftIO $ dyn200 ptr_glGenBuffersARB v1 v2

{-# NOINLINE ptr_glGenBuffersARB #-}
ptr_glGenBuffersARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenBuffersARB = unsafePerformIO $ getCommand "glGenBuffersARB"

