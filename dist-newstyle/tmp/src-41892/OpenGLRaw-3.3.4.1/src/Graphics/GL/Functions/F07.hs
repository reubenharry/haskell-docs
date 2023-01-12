{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F07
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

module Graphics.GL.Functions.F07 (
  glDrawTexxOES,
  glDrawTexxvOES,
  glDrawTransformFeedback,
  glDrawTransformFeedbackEXT,
  glDrawTransformFeedbackInstanced,
  glDrawTransformFeedbackInstancedEXT,
  glDrawTransformFeedbackNV,
  glDrawTransformFeedbackStream,
  glDrawTransformFeedbackStreamInstanced,
  glDrawVkImageNV,
  glEGLImageTargetRenderbufferStorageOES,
  glEGLImageTargetTexStorageEXT,
  glEGLImageTargetTexture2DOES,
  glEGLImageTargetTextureStorageEXT,
  glEdgeFlag,
  glEdgeFlagFormatNV,
  glEdgeFlagPointer,
  glEdgeFlagPointerEXT,
  glEdgeFlagPointerListIBM,
  glEdgeFlagv,
  glElementPointerAPPLE,
  glElementPointerATI,
  glEnable,
  glEnableClientState,
  glEnableClientStateIndexedEXT,
  glEnableClientStateiEXT,
  glEnableDriverControlQCOM,
  glEnableIndexedEXT,
  glEnableVariantClientStateEXT,
  glEnableVertexArrayAttrib,
  glEnableVertexArrayAttribEXT,
  glEnableVertexArrayEXT,
  glEnableVertexAttribAPPLE,
  glEnableVertexAttribArray,
  glEnableVertexAttribArrayARB,
  glEnablei,
  glEnableiEXT,
  glEnableiNV,
  glEnableiOES,
  glEnd,
  glEndConditionalRender,
  glEndConditionalRenderNV,
  glEndConditionalRenderNVX,
  glEndFragmentShaderATI,
  glEndList,
  glEndOcclusionQueryNV,
  glEndPerfMonitorAMD,
  glEndPerfQueryINTEL,
  glEndQuery,
  glEndQueryARB,
  glEndQueryEXT,
  glEndQueryIndexed,
  glEndTilingQCOM,
  glEndTransformFeedback,
  glEndTransformFeedbackEXT,
  glEndTransformFeedbackNV,
  glEndVertexShaderEXT,
  glEndVideoCaptureNV,
  glEvalCoord1d,
  glEvalCoord1dv,
  glEvalCoord1f,
  glEvalCoord1fv,
  glEvalCoord1xOES,
  glEvalCoord1xvOES,
  glEvalCoord2d,
  glEvalCoord2dv,
  glEvalCoord2f,
  glEvalCoord2fv,
  glEvalCoord2xOES,
  glEvalCoord2xvOES,
  glEvalMapsNV,
  glEvalMesh1,
  glEvalMesh2,
  glEvalPoint1,
  glEvalPoint2,
  glEvaluateDepthValuesARB,
  glExecuteProgramNV,
  glExtGetBufferPointervQCOM,
  glExtGetBuffersQCOM,
  glExtGetFramebuffersQCOM,
  glExtGetProgramBinarySourceQCOM,
  glExtGetProgramsQCOM,
  glExtGetRenderbuffersQCOM,
  glExtGetShadersQCOM,
  glExtGetTexLevelParameterivQCOM,
  glExtGetTexSubImageQCOM,
  glExtGetTexturesQCOM,
  glExtIsProgramBinaryQCOM,
  glExtTexObjectStateOverrideiQCOM,
  glExtractComponentEXT,
  glFeedbackBuffer,
  glFeedbackBufferxOES,
  glFenceSync,
  glFenceSyncAPPLE,
  glFinalCombinerInputNV,
  glFinish,
  glFinishAsyncSGIX,
  glFinishFenceAPPLE,
  glFinishFenceNV,
  glFinishObjectAPPLE
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glDrawTexxOES ---------------------------------------------------------------

-- | The vector equivalent of this command is 'glDrawTexxvOES'.
glDrawTexxOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> GLfixed -- ^ @width@.
  -> GLfixed -- ^ @height@.
  -> m ()
glDrawTexxOES v1 v2 v3 v4 v5 = liftIO $ dyn264 ptr_glDrawTexxOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexxOES #-}
ptr_glDrawTexxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glDrawTexxOES = unsafePerformIO $ getCommand "glDrawTexxOES"

-- glDrawTexxvOES --------------------------------------------------------------

glDrawTexxvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @5@ elements of type @GLfixed@.
  -> m ()
glDrawTexxvOES v1 = liftIO $ dyn114 ptr_glDrawTexxvOES v1

{-# NOINLINE ptr_glDrawTexxvOES #-}
ptr_glDrawTexxvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glDrawTexxvOES = unsafePerformIO $ getCommand "glDrawTexxvOES"

-- glDrawTransformFeedback -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedback.xhtml OpenGL 4.x>.
glDrawTransformFeedback
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> m ()
glDrawTransformFeedback v1 v2 = liftIO $ dyn19 ptr_glDrawTransformFeedback v1 v2

{-# NOINLINE ptr_glDrawTransformFeedback #-}
ptr_glDrawTransformFeedback :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDrawTransformFeedback = unsafePerformIO $ getCommand "glDrawTransformFeedback"

-- glDrawTransformFeedbackEXT --------------------------------------------------

-- | This command is an alias for 'glDrawTransformFeedback'.
glDrawTransformFeedbackEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> m ()
glDrawTransformFeedbackEXT v1 v2 = liftIO $ dyn19 ptr_glDrawTransformFeedbackEXT v1 v2

{-# NOINLINE ptr_glDrawTransformFeedbackEXT #-}
ptr_glDrawTransformFeedbackEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDrawTransformFeedbackEXT = unsafePerformIO $ getCommand "glDrawTransformFeedbackEXT"

-- glDrawTransformFeedbackInstanced --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackInstanced.xhtml OpenGL 4.x>.
glDrawTransformFeedbackInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawTransformFeedbackInstanced v1 v2 v3 = liftIO $ dyn265 ptr_glDrawTransformFeedbackInstanced v1 v2 v3

{-# NOINLINE ptr_glDrawTransformFeedbackInstanced #-}
ptr_glDrawTransformFeedbackInstanced :: FunPtr (GLenum -> GLuint -> GLsizei -> IO ())
ptr_glDrawTransformFeedbackInstanced = unsafePerformIO $ getCommand "glDrawTransformFeedbackInstanced"

-- glDrawTransformFeedbackInstancedEXT -----------------------------------------

-- | This command is an alias for 'glDrawTransformFeedbackInstanced'.
glDrawTransformFeedbackInstancedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawTransformFeedbackInstancedEXT v1 v2 v3 = liftIO $ dyn265 ptr_glDrawTransformFeedbackInstancedEXT v1 v2 v3

{-# NOINLINE ptr_glDrawTransformFeedbackInstancedEXT #-}
ptr_glDrawTransformFeedbackInstancedEXT :: FunPtr (GLenum -> GLuint -> GLsizei -> IO ())
ptr_glDrawTransformFeedbackInstancedEXT = unsafePerformIO $ getCommand "glDrawTransformFeedbackInstancedEXT"

-- glDrawTransformFeedbackNV ---------------------------------------------------

-- | This command is an alias for 'glDrawTransformFeedback'.
glDrawTransformFeedbackNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> m ()
glDrawTransformFeedbackNV v1 v2 = liftIO $ dyn19 ptr_glDrawTransformFeedbackNV v1 v2

{-# NOINLINE ptr_glDrawTransformFeedbackNV #-}
ptr_glDrawTransformFeedbackNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDrawTransformFeedbackNV = unsafePerformIO $ getCommand "glDrawTransformFeedbackNV"

-- glDrawTransformFeedbackStream -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackStream.xhtml OpenGL 4.x>.
glDrawTransformFeedbackStream
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> GLuint -- ^ @stream@.
  -> m ()
glDrawTransformFeedbackStream v1 v2 v3 = liftIO $ dyn20 ptr_glDrawTransformFeedbackStream v1 v2 v3

{-# NOINLINE ptr_glDrawTransformFeedbackStream #-}
ptr_glDrawTransformFeedbackStream :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glDrawTransformFeedbackStream = unsafePerformIO $ getCommand "glDrawTransformFeedbackStream"

-- glDrawTransformFeedbackStreamInstanced --------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackStreamInstanced.xhtml OpenGL 4.x>.
glDrawTransformFeedbackStreamInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> GLuint -- ^ @stream@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawTransformFeedbackStreamInstanced v1 v2 v3 v4 = liftIO $ dyn257 ptr_glDrawTransformFeedbackStreamInstanced v1 v2 v3 v4

{-# NOINLINE ptr_glDrawTransformFeedbackStreamInstanced #-}
ptr_glDrawTransformFeedbackStreamInstanced :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> IO ())
ptr_glDrawTransformFeedbackStreamInstanced = unsafePerformIO $ getCommand "glDrawTransformFeedbackStreamInstanced"

-- glDrawVkImageNV -------------------------------------------------------------

glDrawVkImageNV
  :: MonadIO m
  => GLuint64 -- ^ @vkImage@.
  -> GLuint -- ^ @sampler@.
  -> GLfloat -- ^ @x0@.
  -> GLfloat -- ^ @y0@.
  -> GLfloat -- ^ @x1@.
  -> GLfloat -- ^ @y1@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @s0@.
  -> GLfloat -- ^ @t0@.
  -> GLfloat -- ^ @s1@.
  -> GLfloat -- ^ @t1@.
  -> m ()
glDrawVkImageNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn266 ptr_glDrawVkImageNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glDrawVkImageNV #-}
ptr_glDrawVkImageNV :: FunPtr (GLuint64 -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glDrawVkImageNV = unsafePerformIO $ getCommand "glDrawVkImageNV"

-- glEGLImageTargetRenderbufferStorageOES --------------------------------------

glEGLImageTargetRenderbufferStorageOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLeglImageOES -- ^ @image@.
  -> m ()
glEGLImageTargetRenderbufferStorageOES v1 v2 = liftIO $ dyn267 ptr_glEGLImageTargetRenderbufferStorageOES v1 v2

{-# NOINLINE ptr_glEGLImageTargetRenderbufferStorageOES #-}
ptr_glEGLImageTargetRenderbufferStorageOES :: FunPtr (GLenum -> GLeglImageOES -> IO ())
ptr_glEGLImageTargetRenderbufferStorageOES = unsafePerformIO $ getCommand "glEGLImageTargetRenderbufferStorageOES"

-- glEGLImageTargetTexStorageEXT -----------------------------------------------

glEGLImageTargetTexStorageEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLeglImageOES -- ^ @image@.
  -> Ptr GLint -- ^ @attrib_list@.
  -> m ()
glEGLImageTargetTexStorageEXT v1 v2 v3 = liftIO $ dyn268 ptr_glEGLImageTargetTexStorageEXT v1 v2 v3

{-# NOINLINE ptr_glEGLImageTargetTexStorageEXT #-}
ptr_glEGLImageTargetTexStorageEXT :: FunPtr (GLenum -> GLeglImageOES -> Ptr GLint -> IO ())
ptr_glEGLImageTargetTexStorageEXT = unsafePerformIO $ getCommand "glEGLImageTargetTexStorageEXT"

-- glEGLImageTargetTexture2DOES ------------------------------------------------

glEGLImageTargetTexture2DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLeglImageOES -- ^ @image@.
  -> m ()
glEGLImageTargetTexture2DOES v1 v2 = liftIO $ dyn267 ptr_glEGLImageTargetTexture2DOES v1 v2

{-# NOINLINE ptr_glEGLImageTargetTexture2DOES #-}
ptr_glEGLImageTargetTexture2DOES :: FunPtr (GLenum -> GLeglImageOES -> IO ())
ptr_glEGLImageTargetTexture2DOES = unsafePerformIO $ getCommand "glEGLImageTargetTexture2DOES"

-- glEGLImageTargetTextureStorageEXT -------------------------------------------

glEGLImageTargetTextureStorageEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLeglImageOES -- ^ @image@.
  -> Ptr GLint -- ^ @attrib_list@.
  -> m ()
glEGLImageTargetTextureStorageEXT v1 v2 v3 = liftIO $ dyn269 ptr_glEGLImageTargetTextureStorageEXT v1 v2 v3

{-# NOINLINE ptr_glEGLImageTargetTextureStorageEXT #-}
ptr_glEGLImageTargetTextureStorageEXT :: FunPtr (GLuint -> GLeglImageOES -> Ptr GLint -> IO ())
ptr_glEGLImageTargetTextureStorageEXT = unsafePerformIO $ getCommand "glEGLImageTargetTextureStorageEXT"

-- glEdgeFlag ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlag.xml OpenGL 2.x>. The vector equivalent of this command is 'glEdgeFlagv'.
glEdgeFlag
  :: MonadIO m
  => GLboolean -- ^ @flag@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glEdgeFlag v1 = liftIO $ dyn198 ptr_glEdgeFlag v1

{-# NOINLINE ptr_glEdgeFlag #-}
ptr_glEdgeFlag :: FunPtr (GLboolean -> IO ())
ptr_glEdgeFlag = unsafePerformIO $ getCommand "glEdgeFlag"

-- glEdgeFlagFormatNV ----------------------------------------------------------

glEdgeFlagFormatNV
  :: MonadIO m
  => GLsizei -- ^ @stride@.
  -> m ()
glEdgeFlagFormatNV v1 = liftIO $ dyn270 ptr_glEdgeFlagFormatNV v1

{-# NOINLINE ptr_glEdgeFlagFormatNV #-}
ptr_glEdgeFlagFormatNV :: FunPtr (GLsizei -> IO ())
ptr_glEdgeFlagFormatNV = unsafePerformIO $ getCommand "glEdgeFlagFormatNV"

-- glEdgeFlagPointer -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlagPointer.xml OpenGL 2.x>.
glEdgeFlagPointer
  :: MonadIO m
  => GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(stride)@ elements of type @a@.
  -> m ()
glEdgeFlagPointer v1 v2 = liftIO $ dyn271 ptr_glEdgeFlagPointer v1 v2

{-# NOINLINE ptr_glEdgeFlagPointer #-}
ptr_glEdgeFlagPointer :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glEdgeFlagPointer = unsafePerformIO $ getCommand "glEdgeFlagPointer"

-- glEdgeFlagPointerEXT --------------------------------------------------------

glEdgeFlagPointerEXT
  :: MonadIO m
  => GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLboolean -- ^ @pointer@ pointing to @COMPSIZE(stride,count)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glEdgeFlagPointerEXT v1 v2 v3 = liftIO $ dyn272 ptr_glEdgeFlagPointerEXT v1 v2 v3

{-# NOINLINE ptr_glEdgeFlagPointerEXT #-}
ptr_glEdgeFlagPointerEXT :: FunPtr (GLsizei -> GLsizei -> Ptr GLboolean -> IO ())
ptr_glEdgeFlagPointerEXT = unsafePerformIO $ getCommand "glEdgeFlagPointerEXT"

-- glEdgeFlagPointerListIBM ----------------------------------------------------

glEdgeFlagPointerListIBM
  :: MonadIO m
  => GLint -- ^ @stride@.
  -> Ptr (Ptr GLboolean) -- ^ @pointer@ pointing to @COMPSIZE(stride)@ elements of type @Ptr BooleanPointer@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glEdgeFlagPointerListIBM v1 v2 v3 = liftIO $ dyn273 ptr_glEdgeFlagPointerListIBM v1 v2 v3

{-# NOINLINE ptr_glEdgeFlagPointerListIBM #-}
ptr_glEdgeFlagPointerListIBM :: FunPtr (GLint -> Ptr (Ptr GLboolean) -> GLint -> IO ())
ptr_glEdgeFlagPointerListIBM = unsafePerformIO $ getCommand "glEdgeFlagPointerListIBM"

-- glEdgeFlagv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlag.xml OpenGL 2.x>.
glEdgeFlagv
  :: MonadIO m
  => Ptr GLboolean -- ^ @flag@ pointing to @1@ element of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glEdgeFlagv v1 = liftIO $ dyn274 ptr_glEdgeFlagv v1

{-# NOINLINE ptr_glEdgeFlagv #-}
ptr_glEdgeFlagv :: FunPtr (Ptr GLboolean -> IO ())
ptr_glEdgeFlagv = unsafePerformIO $ getCommand "glEdgeFlagv"

-- glElementPointerAPPLE -------------------------------------------------------

glElementPointerAPPLE
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ElementPointerTypeATI](Graphics-GL-Groups.html#ElementPointerTypeATI).
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type)@ elements of type @a@.
  -> m ()
glElementPointerAPPLE v1 v2 = liftIO $ dyn238 ptr_glElementPointerAPPLE v1 v2

{-# NOINLINE ptr_glElementPointerAPPLE #-}
ptr_glElementPointerAPPLE :: FunPtr (GLenum -> Ptr a -> IO ())
ptr_glElementPointerAPPLE = unsafePerformIO $ getCommand "glElementPointerAPPLE"

-- glElementPointerATI ---------------------------------------------------------

glElementPointerATI
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ElementPointerTypeATI](Graphics-GL-Groups.html#ElementPointerTypeATI).
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type)@ elements of type @a@.
  -> m ()
glElementPointerATI v1 v2 = liftIO $ dyn238 ptr_glElementPointerATI v1 v2

{-# NOINLINE ptr_glElementPointerATI #-}
ptr_glElementPointerATI :: FunPtr (GLenum -> Ptr a -> IO ())
ptr_glElementPointerATI = unsafePerformIO $ getCommand "glElementPointerATI"

-- glEnable --------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnable.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glEnable
  :: MonadIO m
  => GLenum -- ^ @cap@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glEnable v1 = liftIO $ dyn5 ptr_glEnable v1

{-# NOINLINE ptr_glEnable #-}
ptr_glEnable :: FunPtr (GLenum -> IO ())
ptr_glEnable = unsafePerformIO $ getCommand "glEnable"

-- glEnableClientState ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableClientState.xml OpenGL 2.x>.
glEnableClientState
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glEnableClientState v1 = liftIO $ dyn5 ptr_glEnableClientState v1

{-# NOINLINE ptr_glEnableClientState #-}
ptr_glEnableClientState :: FunPtr (GLenum -> IO ())
ptr_glEnableClientState = unsafePerformIO $ getCommand "glEnableClientState"

-- glEnableClientStateIndexedEXT -----------------------------------------------

glEnableClientStateIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableClientStateIndexedEXT v1 v2 = liftIO $ dyn19 ptr_glEnableClientStateIndexedEXT v1 v2

{-# NOINLINE ptr_glEnableClientStateIndexedEXT #-}
ptr_glEnableClientStateIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableClientStateIndexedEXT = unsafePerformIO $ getCommand "glEnableClientStateIndexedEXT"

-- glEnableClientStateiEXT -----------------------------------------------------

glEnableClientStateiEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableClientStateiEXT v1 v2 = liftIO $ dyn19 ptr_glEnableClientStateiEXT v1 v2

{-# NOINLINE ptr_glEnableClientStateiEXT #-}
ptr_glEnableClientStateiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableClientStateiEXT = unsafePerformIO $ getCommand "glEnableClientStateiEXT"

-- glEnableDriverControlQCOM ---------------------------------------------------

glEnableDriverControlQCOM
  :: MonadIO m
  => GLuint -- ^ @driverControl@.
  -> m ()
glEnableDriverControlQCOM v1 = liftIO $ dyn3 ptr_glEnableDriverControlQCOM v1

{-# NOINLINE ptr_glEnableDriverControlQCOM #-}
ptr_glEnableDriverControlQCOM :: FunPtr (GLuint -> IO ())
ptr_glEnableDriverControlQCOM = unsafePerformIO $ getCommand "glEnableDriverControlQCOM"

-- glEnableIndexedEXT ----------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableIndexedEXT v1 v2 = liftIO $ dyn19 ptr_glEnableIndexedEXT v1 v2

{-# NOINLINE ptr_glEnableIndexedEXT #-}
ptr_glEnableIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableIndexedEXT = unsafePerformIO $ getCommand "glEnableIndexedEXT"

-- glEnableVariantClientStateEXT -----------------------------------------------

glEnableVariantClientStateEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glEnableVariantClientStateEXT v1 = liftIO $ dyn3 ptr_glEnableVariantClientStateEXT v1

{-# NOINLINE ptr_glEnableVariantClientStateEXT #-}
ptr_glEnableVariantClientStateEXT :: FunPtr (GLuint -> IO ())
ptr_glEnableVariantClientStateEXT = unsafePerformIO $ getCommand "glEnableVariantClientStateEXT"

-- glEnableVertexArrayAttrib ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glEnableVertexArrayAttrib
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableVertexArrayAttrib v1 v2 = liftIO $ dyn4 ptr_glEnableVertexArrayAttrib v1 v2

{-# NOINLINE ptr_glEnableVertexArrayAttrib #-}
ptr_glEnableVertexArrayAttrib :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glEnableVertexArrayAttrib = unsafePerformIO $ getCommand "glEnableVertexArrayAttrib"

-- glEnableVertexArrayAttribEXT ------------------------------------------------

glEnableVertexArrayAttribEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableVertexArrayAttribEXT v1 v2 = liftIO $ dyn4 ptr_glEnableVertexArrayAttribEXT v1 v2

{-# NOINLINE ptr_glEnableVertexArrayAttribEXT #-}
ptr_glEnableVertexArrayAttribEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glEnableVertexArrayAttribEXT = unsafePerformIO $ getCommand "glEnableVertexArrayAttribEXT"

-- glEnableVertexArrayEXT ------------------------------------------------------

glEnableVertexArrayEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glEnableVertexArrayEXT v1 v2 = liftIO $ dyn18 ptr_glEnableVertexArrayEXT v1 v2

{-# NOINLINE ptr_glEnableVertexArrayEXT #-}
ptr_glEnableVertexArrayEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glEnableVertexArrayEXT = unsafePerformIO $ getCommand "glEnableVertexArrayEXT"

-- glEnableVertexAttribAPPLE ---------------------------------------------------

glEnableVertexAttribAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> m ()
glEnableVertexAttribAPPLE v1 v2 = liftIO $ dyn18 ptr_glEnableVertexAttribAPPLE v1 v2

{-# NOINLINE ptr_glEnableVertexAttribAPPLE #-}
ptr_glEnableVertexAttribAPPLE :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glEnableVertexAttribAPPLE = unsafePerformIO $ getCommand "glEnableVertexAttribAPPLE"

-- glEnableVertexAttribArray ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableVertexAttribArray.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnableVertexAttribArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glEnableVertexAttribArray
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glEnableVertexAttribArray v1 = liftIO $ dyn3 ptr_glEnableVertexAttribArray v1

{-# NOINLINE ptr_glEnableVertexAttribArray #-}
ptr_glEnableVertexAttribArray :: FunPtr (GLuint -> IO ())
ptr_glEnableVertexAttribArray = unsafePerformIO $ getCommand "glEnableVertexAttribArray"

-- glEnableVertexAttribArrayARB ------------------------------------------------

-- | This command is an alias for 'glEnableVertexAttribArray'.
glEnableVertexAttribArrayARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glEnableVertexAttribArrayARB v1 = liftIO $ dyn3 ptr_glEnableVertexAttribArrayARB v1

{-# NOINLINE ptr_glEnableVertexAttribArrayARB #-}
ptr_glEnableVertexAttribArrayARB :: FunPtr (GLuint -> IO ())
ptr_glEnableVertexAttribArrayARB = unsafePerformIO $ getCommand "glEnableVertexAttribArrayARB"

-- glEnablei -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glEnablei
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnablei v1 v2 = liftIO $ dyn19 ptr_glEnablei v1 v2

{-# NOINLINE ptr_glEnablei #-}
ptr_glEnablei :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnablei = unsafePerformIO $ getCommand "glEnablei"

-- glEnableiEXT ----------------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableiEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableiEXT v1 v2 = liftIO $ dyn19 ptr_glEnableiEXT v1 v2

{-# NOINLINE ptr_glEnableiEXT #-}
ptr_glEnableiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableiEXT = unsafePerformIO $ getCommand "glEnableiEXT"

-- glEnableiNV -----------------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableiNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableiNV v1 v2 = liftIO $ dyn19 ptr_glEnableiNV v1 v2

{-# NOINLINE ptr_glEnableiNV #-}
ptr_glEnableiNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableiNV = unsafePerformIO $ getCommand "glEnableiNV"

-- glEnableiOES ----------------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableiOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableiOES v1 v2 = liftIO $ dyn19 ptr_glEnableiOES v1 v2

{-# NOINLINE ptr_glEnableiOES #-}
ptr_glEnableiOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableiOES = unsafePerformIO $ getCommand "glEnableiOES"

-- glEnd -----------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml OpenGL 2.x>.
glEnd
  :: MonadIO m
  => m ()
glEnd = liftIO $ dyn11 ptr_glEnd

{-# NOINLINE ptr_glEnd #-}
ptr_glEnd :: FunPtr (IO ())
ptr_glEnd = unsafePerformIO $ getCommand "glEnd"

-- glEndConditionalRender ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginConditionalRender.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginConditionalRender.xhtml OpenGL 4.x>.
glEndConditionalRender
  :: MonadIO m
  => m ()
glEndConditionalRender = liftIO $ dyn11 ptr_glEndConditionalRender

{-# NOINLINE ptr_glEndConditionalRender #-}
ptr_glEndConditionalRender :: FunPtr (IO ())
ptr_glEndConditionalRender = unsafePerformIO $ getCommand "glEndConditionalRender"

-- glEndConditionalRenderNV ----------------------------------------------------

-- | This command is an alias for 'glEndConditionalRender'.
glEndConditionalRenderNV
  :: MonadIO m
  => m ()
glEndConditionalRenderNV = liftIO $ dyn11 ptr_glEndConditionalRenderNV

{-# NOINLINE ptr_glEndConditionalRenderNV #-}
ptr_glEndConditionalRenderNV :: FunPtr (IO ())
ptr_glEndConditionalRenderNV = unsafePerformIO $ getCommand "glEndConditionalRenderNV"

-- glEndConditionalRenderNVX ---------------------------------------------------

-- | This command is an alias for 'glEndConditionalRender'.
glEndConditionalRenderNVX
  :: MonadIO m
  => m ()
glEndConditionalRenderNVX = liftIO $ dyn11 ptr_glEndConditionalRenderNVX

{-# NOINLINE ptr_glEndConditionalRenderNVX #-}
ptr_glEndConditionalRenderNVX :: FunPtr (IO ())
ptr_glEndConditionalRenderNVX = unsafePerformIO $ getCommand "glEndConditionalRenderNVX"

-- glEndFragmentShaderATI ------------------------------------------------------

glEndFragmentShaderATI
  :: MonadIO m
  => m ()
glEndFragmentShaderATI = liftIO $ dyn11 ptr_glEndFragmentShaderATI

{-# NOINLINE ptr_glEndFragmentShaderATI #-}
ptr_glEndFragmentShaderATI :: FunPtr (IO ())
ptr_glEndFragmentShaderATI = unsafePerformIO $ getCommand "glEndFragmentShaderATI"

-- glEndList -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNewList.xml OpenGL 2.x>.
glEndList
  :: MonadIO m
  => m ()
glEndList = liftIO $ dyn11 ptr_glEndList

{-# NOINLINE ptr_glEndList #-}
ptr_glEndList :: FunPtr (IO ())
ptr_glEndList = unsafePerformIO $ getCommand "glEndList"

-- glEndOcclusionQueryNV -------------------------------------------------------

glEndOcclusionQueryNV
  :: MonadIO m
  => m ()
glEndOcclusionQueryNV = liftIO $ dyn11 ptr_glEndOcclusionQueryNV

{-# NOINLINE ptr_glEndOcclusionQueryNV #-}
ptr_glEndOcclusionQueryNV :: FunPtr (IO ())
ptr_glEndOcclusionQueryNV = unsafePerformIO $ getCommand "glEndOcclusionQueryNV"

-- glEndPerfMonitorAMD ---------------------------------------------------------

glEndPerfMonitorAMD
  :: MonadIO m
  => GLuint -- ^ @monitor@.
  -> m ()
glEndPerfMonitorAMD v1 = liftIO $ dyn3 ptr_glEndPerfMonitorAMD v1

{-# NOINLINE ptr_glEndPerfMonitorAMD #-}
ptr_glEndPerfMonitorAMD :: FunPtr (GLuint -> IO ())
ptr_glEndPerfMonitorAMD = unsafePerformIO $ getCommand "glEndPerfMonitorAMD"

-- glEndPerfQueryINTEL ---------------------------------------------------------

glEndPerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> m ()
glEndPerfQueryINTEL v1 = liftIO $ dyn3 ptr_glEndPerfQueryINTEL v1

{-# NOINLINE ptr_glEndPerfQueryINTEL #-}
ptr_glEndPerfQueryINTEL :: FunPtr (GLuint -> IO ())
ptr_glEndPerfQueryINTEL = unsafePerformIO $ getCommand "glEndPerfQueryINTEL"

-- glEndQuery ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBeginQuery.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginQuery.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginQuery.xhtml OpenGL 4.x>.
glEndQuery
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> m ()
glEndQuery v1 = liftIO $ dyn5 ptr_glEndQuery v1

{-# NOINLINE ptr_glEndQuery #-}
ptr_glEndQuery :: FunPtr (GLenum -> IO ())
ptr_glEndQuery = unsafePerformIO $ getCommand "glEndQuery"

-- glEndQueryARB ---------------------------------------------------------------

-- | This command is an alias for 'glEndQuery'.
glEndQueryARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> m ()
glEndQueryARB v1 = liftIO $ dyn5 ptr_glEndQueryARB v1

{-# NOINLINE ptr_glEndQueryARB #-}
ptr_glEndQueryARB :: FunPtr (GLenum -> IO ())
ptr_glEndQueryARB = unsafePerformIO $ getCommand "glEndQueryARB"

-- glEndQueryEXT ---------------------------------------------------------------

glEndQueryEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> m ()
glEndQueryEXT v1 = liftIO $ dyn5 ptr_glEndQueryEXT v1

{-# NOINLINE ptr_glEndQueryEXT #-}
ptr_glEndQueryEXT :: FunPtr (GLenum -> IO ())
ptr_glEndQueryEXT = unsafePerformIO $ getCommand "glEndQueryEXT"

-- glEndQueryIndexed -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBeginQueryIndexed.xhtml OpenGL 4.x>.
glEndQueryIndexed
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLuint -- ^ @index@.
  -> m ()
glEndQueryIndexed v1 v2 = liftIO $ dyn19 ptr_glEndQueryIndexed v1 v2

{-# NOINLINE ptr_glEndQueryIndexed #-}
ptr_glEndQueryIndexed :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEndQueryIndexed = unsafePerformIO $ getCommand "glEndQueryIndexed"

-- glEndTilingQCOM -------------------------------------------------------------

glEndTilingQCOM
  :: MonadIO m
  => GLbitfield -- ^ @preserveMask@ of type [BufferBitQCOM](Graphics-GL-Groups.html#BufferBitQCOM).
  -> m ()
glEndTilingQCOM v1 = liftIO $ dyn75 ptr_glEndTilingQCOM v1

{-# NOINLINE ptr_glEndTilingQCOM #-}
ptr_glEndTilingQCOM :: FunPtr (GLbitfield -> IO ())
ptr_glEndTilingQCOM = unsafePerformIO $ getCommand "glEndTilingQCOM"

-- glEndTransformFeedback ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginTransformFeedback.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginTransformFeedback.xhtml OpenGL 4.x>.
glEndTransformFeedback
  :: MonadIO m
  => m ()
glEndTransformFeedback = liftIO $ dyn11 ptr_glEndTransformFeedback

{-# NOINLINE ptr_glEndTransformFeedback #-}
ptr_glEndTransformFeedback :: FunPtr (IO ())
ptr_glEndTransformFeedback = unsafePerformIO $ getCommand "glEndTransformFeedback"

-- glEndTransformFeedbackEXT ---------------------------------------------------

-- | This command is an alias for 'glEndTransformFeedback'.
glEndTransformFeedbackEXT
  :: MonadIO m
  => m ()
glEndTransformFeedbackEXT = liftIO $ dyn11 ptr_glEndTransformFeedbackEXT

{-# NOINLINE ptr_glEndTransformFeedbackEXT #-}
ptr_glEndTransformFeedbackEXT :: FunPtr (IO ())
ptr_glEndTransformFeedbackEXT = unsafePerformIO $ getCommand "glEndTransformFeedbackEXT"

-- glEndTransformFeedbackNV ----------------------------------------------------

-- | This command is an alias for 'glEndTransformFeedback'.
glEndTransformFeedbackNV
  :: MonadIO m
  => m ()
glEndTransformFeedbackNV = liftIO $ dyn11 ptr_glEndTransformFeedbackNV

{-# NOINLINE ptr_glEndTransformFeedbackNV #-}
ptr_glEndTransformFeedbackNV :: FunPtr (IO ())
ptr_glEndTransformFeedbackNV = unsafePerformIO $ getCommand "glEndTransformFeedbackNV"

-- glEndVertexShaderEXT --------------------------------------------------------

glEndVertexShaderEXT
  :: MonadIO m
  => m ()
glEndVertexShaderEXT = liftIO $ dyn11 ptr_glEndVertexShaderEXT

{-# NOINLINE ptr_glEndVertexShaderEXT #-}
ptr_glEndVertexShaderEXT :: FunPtr (IO ())
ptr_glEndVertexShaderEXT = unsafePerformIO $ getCommand "glEndVertexShaderEXT"

-- glEndVideoCaptureNV ---------------------------------------------------------

glEndVideoCaptureNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> m ()
glEndVideoCaptureNV v1 = liftIO $ dyn3 ptr_glEndVideoCaptureNV v1

{-# NOINLINE ptr_glEndVideoCaptureNV #-}
ptr_glEndVideoCaptureNV :: FunPtr (GLuint -> IO ())
ptr_glEndVideoCaptureNV = unsafePerformIO $ getCommand "glEndVideoCaptureNV"

-- glEvalCoord1d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord1dv'.
glEvalCoord1d
  :: MonadIO m
  => GLdouble -- ^ @u@ of type @CoordD@.
  -> m ()
glEvalCoord1d v1 = liftIO $ dyn84 ptr_glEvalCoord1d v1

{-# NOINLINE ptr_glEvalCoord1d #-}
ptr_glEvalCoord1d :: FunPtr (GLdouble -> IO ())
ptr_glEvalCoord1d = unsafePerformIO $ getCommand "glEvalCoord1d"

-- glEvalCoord1dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord1dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @u@ pointing to @1@ element of type @CoordD@.
  -> m ()
glEvalCoord1dv v1 = liftIO $ dyn42 ptr_glEvalCoord1dv v1

{-# NOINLINE ptr_glEvalCoord1dv #-}
ptr_glEvalCoord1dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glEvalCoord1dv = unsafePerformIO $ getCommand "glEvalCoord1dv"

-- glEvalCoord1f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord1fv'.
glEvalCoord1f
  :: MonadIO m
  => GLfloat -- ^ @u@ of type @CoordF@.
  -> m ()
glEvalCoord1f v1 = liftIO $ dyn85 ptr_glEvalCoord1f v1

{-# NOINLINE ptr_glEvalCoord1f #-}
ptr_glEvalCoord1f :: FunPtr (GLfloat -> IO ())
ptr_glEvalCoord1f = unsafePerformIO $ getCommand "glEvalCoord1f"

-- glEvalCoord1fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord1fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @u@ pointing to @1@ element of type @CoordF@.
  -> m ()
glEvalCoord1fv v1 = liftIO $ dyn44 ptr_glEvalCoord1fv v1

{-# NOINLINE ptr_glEvalCoord1fv #-}
ptr_glEvalCoord1fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glEvalCoord1fv = unsafePerformIO $ getCommand "glEvalCoord1fv"

-- glEvalCoord1xOES ------------------------------------------------------------

glEvalCoord1xOES
  :: MonadIO m
  => GLfixed -- ^ @u@.
  -> m ()
glEvalCoord1xOES v1 = liftIO $ dyn87 ptr_glEvalCoord1xOES v1

{-# NOINLINE ptr_glEvalCoord1xOES #-}
ptr_glEvalCoord1xOES :: FunPtr (GLfixed -> IO ())
ptr_glEvalCoord1xOES = unsafePerformIO $ getCommand "glEvalCoord1xOES"

-- glEvalCoord1xvOES -----------------------------------------------------------

glEvalCoord1xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @1@ element of type @GLfixed@.
  -> m ()
glEvalCoord1xvOES v1 = liftIO $ dyn114 ptr_glEvalCoord1xvOES v1

{-# NOINLINE ptr_glEvalCoord1xvOES #-}
ptr_glEvalCoord1xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glEvalCoord1xvOES = unsafePerformIO $ getCommand "glEvalCoord1xvOES"

-- glEvalCoord2d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord2dv'.
glEvalCoord2d
  :: MonadIO m
  => GLdouble -- ^ @u@ of type @CoordD@.
  -> GLdouble -- ^ @v@ of type @CoordD@.
  -> m ()
glEvalCoord2d v1 v2 = liftIO $ dyn225 ptr_glEvalCoord2d v1 v2

{-# NOINLINE ptr_glEvalCoord2d #-}
ptr_glEvalCoord2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glEvalCoord2d = unsafePerformIO $ getCommand "glEvalCoord2d"

-- glEvalCoord2dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @u@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glEvalCoord2dv v1 = liftIO $ dyn42 ptr_glEvalCoord2dv v1

{-# NOINLINE ptr_glEvalCoord2dv #-}
ptr_glEvalCoord2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glEvalCoord2dv = unsafePerformIO $ getCommand "glEvalCoord2dv"

-- glEvalCoord2f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord2fv'.
glEvalCoord2f
  :: MonadIO m
  => GLfloat -- ^ @u@ of type @CoordF@.
  -> GLfloat -- ^ @v@ of type @CoordF@.
  -> m ()
glEvalCoord2f v1 v2 = liftIO $ dyn230 ptr_glEvalCoord2f v1 v2

{-# NOINLINE ptr_glEvalCoord2f #-}
ptr_glEvalCoord2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glEvalCoord2f = unsafePerformIO $ getCommand "glEvalCoord2f"

-- glEvalCoord2fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @u@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glEvalCoord2fv v1 = liftIO $ dyn44 ptr_glEvalCoord2fv v1

{-# NOINLINE ptr_glEvalCoord2fv #-}
ptr_glEvalCoord2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glEvalCoord2fv = unsafePerformIO $ getCommand "glEvalCoord2fv"

-- glEvalCoord2xOES ------------------------------------------------------------

glEvalCoord2xOES
  :: MonadIO m
  => GLfixed -- ^ @u@.
  -> GLfixed -- ^ @v@.
  -> m ()
glEvalCoord2xOES v1 v2 = liftIO $ dyn232 ptr_glEvalCoord2xOES v1 v2

{-# NOINLINE ptr_glEvalCoord2xOES #-}
ptr_glEvalCoord2xOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glEvalCoord2xOES = unsafePerformIO $ getCommand "glEvalCoord2xOES"

-- glEvalCoord2xvOES -----------------------------------------------------------

glEvalCoord2xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glEvalCoord2xvOES v1 = liftIO $ dyn114 ptr_glEvalCoord2xvOES v1

{-# NOINLINE ptr_glEvalCoord2xvOES #-}
ptr_glEvalCoord2xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glEvalCoord2xvOES = unsafePerformIO $ getCommand "glEvalCoord2xvOES"

-- glEvalMapsNV ----------------------------------------------------------------

glEvalMapsNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EvalTargetNV](Graphics-GL-Groups.html#EvalTargetNV).
  -> GLenum -- ^ @mode@ of type [EvalMapsModeNV](Graphics-GL-Groups.html#EvalMapsModeNV).
  -> m ()
glEvalMapsNV v1 v2 = liftIO $ dyn54 ptr_glEvalMapsNV v1 v2

{-# NOINLINE ptr_glEvalMapsNV #-}
ptr_glEvalMapsNV :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glEvalMapsNV = unsafePerformIO $ getCommand "glEvalMapsNV"

-- glEvalMesh1 -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalMesh.xml OpenGL 2.x>.
glEvalMesh1
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MeshMode1](Graphics-GL-Groups.html#MeshMode1).
  -> GLint -- ^ @i1@ of type @CheckedInt32@.
  -> GLint -- ^ @i2@ of type @CheckedInt32@.
  -> m ()
glEvalMesh1 v1 v2 v3 = liftIO $ dyn275 ptr_glEvalMesh1 v1 v2 v3

{-# NOINLINE ptr_glEvalMesh1 #-}
ptr_glEvalMesh1 :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glEvalMesh1 = unsafePerformIO $ getCommand "glEvalMesh1"

-- glEvalMesh2 -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalMesh.xml OpenGL 2.x>.
glEvalMesh2
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MeshMode2](Graphics-GL-Groups.html#MeshMode2).
  -> GLint -- ^ @i1@ of type @CheckedInt32@.
  -> GLint -- ^ @i2@ of type @CheckedInt32@.
  -> GLint -- ^ @j1@ of type @CheckedInt32@.
  -> GLint -- ^ @j2@ of type @CheckedInt32@.
  -> m ()
glEvalMesh2 v1 v2 v3 v4 v5 = liftIO $ dyn276 ptr_glEvalMesh2 v1 v2 v3 v4 v5

{-# NOINLINE ptr_glEvalMesh2 #-}
ptr_glEvalMesh2 :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glEvalMesh2 = unsafePerformIO $ getCommand "glEvalMesh2"

-- glEvalPoint1 ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalPoint.xml OpenGL 2.x>.
glEvalPoint1
  :: MonadIO m
  => GLint -- ^ @i@.
  -> m ()
glEvalPoint1 v1 = liftIO $ dyn13 ptr_glEvalPoint1 v1

{-# NOINLINE ptr_glEvalPoint1 #-}
ptr_glEvalPoint1 :: FunPtr (GLint -> IO ())
ptr_glEvalPoint1 = unsafePerformIO $ getCommand "glEvalPoint1"

-- glEvalPoint2 ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalPoint.xml OpenGL 2.x>.
glEvalPoint2
  :: MonadIO m
  => GLint -- ^ @i@ of type @CheckedInt32@.
  -> GLint -- ^ @j@ of type @CheckedInt32@.
  -> m ()
glEvalPoint2 v1 v2 = liftIO $ dyn277 ptr_glEvalPoint2 v1 v2

{-# NOINLINE ptr_glEvalPoint2 #-}
ptr_glEvalPoint2 :: FunPtr (GLint -> GLint -> IO ())
ptr_glEvalPoint2 = unsafePerformIO $ getCommand "glEvalPoint2"

-- glEvaluateDepthValuesARB ----------------------------------------------------

glEvaluateDepthValuesARB
  :: MonadIO m
  => m ()
glEvaluateDepthValuesARB = liftIO $ dyn11 ptr_glEvaluateDepthValuesARB

{-# NOINLINE ptr_glEvaluateDepthValuesARB #-}
ptr_glEvaluateDepthValuesARB :: FunPtr (IO ())
ptr_glEvaluateDepthValuesARB = unsafePerformIO $ getCommand "glEvaluateDepthValuesARB"

-- glExecuteProgramNV ----------------------------------------------------------

glExecuteProgramNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @id@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glExecuteProgramNV v1 v2 v3 = liftIO $ dyn278 ptr_glExecuteProgramNV v1 v2 v3

{-# NOINLINE ptr_glExecuteProgramNV #-}
ptr_glExecuteProgramNV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glExecuteProgramNV = unsafePerformIO $ getCommand "glExecuteProgramNV"

-- glExtGetBufferPointervQCOM --------------------------------------------------

glExtGetBufferPointervQCOM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glExtGetBufferPointervQCOM v1 v2 = liftIO $ dyn279 ptr_glExtGetBufferPointervQCOM v1 v2

{-# NOINLINE ptr_glExtGetBufferPointervQCOM #-}
ptr_glExtGetBufferPointervQCOM :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glExtGetBufferPointervQCOM = unsafePerformIO $ getCommand "glExtGetBufferPointervQCOM"

-- glExtGetBuffersQCOM ---------------------------------------------------------

glExtGetBuffersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @buffers@ pointing to @maxBuffers@ elements of type @GLuint@.
  -> GLint -- ^ @maxBuffers@.
  -> Ptr GLint -- ^ @numBuffers@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetBuffersQCOM v1 v2 v3 = liftIO $ dyn280 ptr_glExtGetBuffersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetBuffersQCOM #-}
ptr_glExtGetBuffersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetBuffersQCOM = unsafePerformIO $ getCommand "glExtGetBuffersQCOM"

-- glExtGetFramebuffersQCOM ----------------------------------------------------

glExtGetFramebuffersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @framebuffers@ pointing to @maxFramebuffers@ elements of type @GLuint@.
  -> GLint -- ^ @maxFramebuffers@.
  -> Ptr GLint -- ^ @numFramebuffers@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetFramebuffersQCOM v1 v2 v3 = liftIO $ dyn280 ptr_glExtGetFramebuffersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetFramebuffersQCOM #-}
ptr_glExtGetFramebuffersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetFramebuffersQCOM = unsafePerformIO $ getCommand "glExtGetFramebuffersQCOM"

-- glExtGetProgramBinarySourceQCOM ---------------------------------------------

glExtGetProgramBinarySourceQCOM
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> Ptr GLchar -- ^ @source@.
  -> Ptr GLint -- ^ @length@.
  -> m ()
glExtGetProgramBinarySourceQCOM v1 v2 v3 v4 = liftIO $ dyn281 ptr_glExtGetProgramBinarySourceQCOM v1 v2 v3 v4

{-# NOINLINE ptr_glExtGetProgramBinarySourceQCOM #-}
ptr_glExtGetProgramBinarySourceQCOM :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> IO ())
ptr_glExtGetProgramBinarySourceQCOM = unsafePerformIO $ getCommand "glExtGetProgramBinarySourceQCOM"

-- glExtGetProgramsQCOM --------------------------------------------------------

glExtGetProgramsQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @programs@ pointing to @maxPrograms@ elements of type @GLuint@.
  -> GLint -- ^ @maxPrograms@.
  -> Ptr GLint -- ^ @numPrograms@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetProgramsQCOM v1 v2 v3 = liftIO $ dyn280 ptr_glExtGetProgramsQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetProgramsQCOM #-}
ptr_glExtGetProgramsQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetProgramsQCOM = unsafePerformIO $ getCommand "glExtGetProgramsQCOM"

-- glExtGetRenderbuffersQCOM ---------------------------------------------------

glExtGetRenderbuffersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @renderbuffers@ pointing to @maxRenderbuffers@ elements of type @GLuint@.
  -> GLint -- ^ @maxRenderbuffers@.
  -> Ptr GLint -- ^ @numRenderbuffers@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetRenderbuffersQCOM v1 v2 v3 = liftIO $ dyn280 ptr_glExtGetRenderbuffersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetRenderbuffersQCOM #-}
ptr_glExtGetRenderbuffersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetRenderbuffersQCOM = unsafePerformIO $ getCommand "glExtGetRenderbuffersQCOM"

-- glExtGetShadersQCOM ---------------------------------------------------------

glExtGetShadersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @shaders@ pointing to @maxShaders@ elements of type @GLuint@.
  -> GLint -- ^ @maxShaders@.
  -> Ptr GLint -- ^ @numShaders@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetShadersQCOM v1 v2 v3 = liftIO $ dyn280 ptr_glExtGetShadersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetShadersQCOM #-}
ptr_glExtGetShadersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetShadersQCOM = unsafePerformIO $ getCommand "glExtGetShadersQCOM"

-- glExtGetTexLevelParameterivQCOM ---------------------------------------------

glExtGetTexLevelParameterivQCOM
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @face@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glExtGetTexLevelParameterivQCOM v1 v2 v3 v4 v5 = liftIO $ dyn282 ptr_glExtGetTexLevelParameterivQCOM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glExtGetTexLevelParameterivQCOM #-}
ptr_glExtGetTexLevelParameterivQCOM :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glExtGetTexLevelParameterivQCOM = unsafePerformIO $ getCommand "glExtGetTexLevelParameterivQCOM"

-- glExtGetTexSubImageQCOM -----------------------------------------------------

glExtGetTexSubImageQCOM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @texels@.
  -> m ()
glExtGetTexSubImageQCOM v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn283 ptr_glExtGetTexSubImageQCOM v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glExtGetTexSubImageQCOM #-}
ptr_glExtGetTexSubImageQCOM :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glExtGetTexSubImageQCOM = unsafePerformIO $ getCommand "glExtGetTexSubImageQCOM"

-- glExtGetTexturesQCOM --------------------------------------------------------

glExtGetTexturesQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @textures@.
  -> GLint -- ^ @maxTextures@.
  -> Ptr GLint -- ^ @numTextures@.
  -> m ()
glExtGetTexturesQCOM v1 v2 v3 = liftIO $ dyn280 ptr_glExtGetTexturesQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetTexturesQCOM #-}
ptr_glExtGetTexturesQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetTexturesQCOM = unsafePerformIO $ getCommand "glExtGetTexturesQCOM"

-- glExtIsProgramBinaryQCOM ----------------------------------------------------

glExtIsProgramBinaryQCOM
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glExtIsProgramBinaryQCOM v1 = liftIO $ dyn284 ptr_glExtIsProgramBinaryQCOM v1

{-# NOINLINE ptr_glExtIsProgramBinaryQCOM #-}
ptr_glExtIsProgramBinaryQCOM :: FunPtr (GLuint -> IO GLboolean)
ptr_glExtIsProgramBinaryQCOM = unsafePerformIO $ getCommand "glExtIsProgramBinaryQCOM"

-- glExtTexObjectStateOverrideiQCOM --------------------------------------------

glExtTexObjectStateOverrideiQCOM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glExtTexObjectStateOverrideiQCOM v1 v2 v3 = liftIO $ dyn66 ptr_glExtTexObjectStateOverrideiQCOM v1 v2 v3

{-# NOINLINE ptr_glExtTexObjectStateOverrideiQCOM #-}
ptr_glExtTexObjectStateOverrideiQCOM :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glExtTexObjectStateOverrideiQCOM = unsafePerformIO $ getCommand "glExtTexObjectStateOverrideiQCOM"

-- glExtractComponentEXT -------------------------------------------------------

glExtractComponentEXT
  :: MonadIO m
  => GLuint -- ^ @res@.
  -> GLuint -- ^ @src@.
  -> GLuint -- ^ @num@.
  -> m ()
glExtractComponentEXT v1 v2 v3 = liftIO $ dyn109 ptr_glExtractComponentEXT v1 v2 v3

{-# NOINLINE ptr_glExtractComponentEXT #-}
ptr_glExtractComponentEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glExtractComponentEXT = unsafePerformIO $ getCommand "glExtractComponentEXT"

-- glFeedbackBuffer ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFeedbackBuffer.xml OpenGL 2.x>.
glFeedbackBuffer
  :: MonadIO m
  => GLsizei -- ^ @size@.
  -> GLenum -- ^ @type@ of type [FeedbackType](Graphics-GL-Groups.html#FeedbackType).
  -> Ptr GLfloat -- ^ @buffer@ pointing to @size@ elements of type @FeedbackElement@.
  -> m ()
glFeedbackBuffer v1 v2 v3 = liftIO $ dyn285 ptr_glFeedbackBuffer v1 v2 v3

{-# NOINLINE ptr_glFeedbackBuffer #-}
ptr_glFeedbackBuffer :: FunPtr (GLsizei -> GLenum -> Ptr GLfloat -> IO ())
ptr_glFeedbackBuffer = unsafePerformIO $ getCommand "glFeedbackBuffer"

-- glFeedbackBufferxOES --------------------------------------------------------

glFeedbackBufferxOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> GLenum -- ^ @type@.
  -> Ptr GLfixed -- ^ @buffer@ pointing to @n@ elements of type @GLfixed@.
  -> m ()
glFeedbackBufferxOES v1 v2 v3 = liftIO $ dyn286 ptr_glFeedbackBufferxOES v1 v2 v3

{-# NOINLINE ptr_glFeedbackBufferxOES #-}
ptr_glFeedbackBufferxOES :: FunPtr (GLsizei -> GLenum -> Ptr GLfixed -> IO ())
ptr_glFeedbackBufferxOES = unsafePerformIO $ getCommand "glFeedbackBufferxOES"

-- glFenceSync -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFenceSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFenceSync.xhtml OpenGL 4.x>.
glFenceSync
  :: MonadIO m
  => GLenum -- ^ @condition@ of type [SyncCondition](Graphics-GL-Groups.html#SyncCondition).
  -> GLbitfield -- ^ @flags@.
  -> m GLsync -- ^ of type @sync@.
glFenceSync v1 v2 = liftIO $ dyn287 ptr_glFenceSync v1 v2

{-# NOINLINE ptr_glFenceSync #-}
ptr_glFenceSync :: FunPtr (GLenum -> GLbitfield -> IO GLsync)
ptr_glFenceSync = unsafePerformIO $ getCommand "glFenceSync"

-- glFenceSyncAPPLE ------------------------------------------------------------

-- | This command is an alias for 'glFenceSync'.
glFenceSyncAPPLE
  :: MonadIO m
  => GLenum -- ^ @condition@ of type [SyncCondition](Graphics-GL-Groups.html#SyncCondition).
  -> GLbitfield -- ^ @flags@.
  -> m GLsync
glFenceSyncAPPLE v1 v2 = liftIO $ dyn287 ptr_glFenceSyncAPPLE v1 v2

{-# NOINLINE ptr_glFenceSyncAPPLE #-}
ptr_glFenceSyncAPPLE :: FunPtr (GLenum -> GLbitfield -> IO GLsync)
ptr_glFenceSyncAPPLE = unsafePerformIO $ getCommand "glFenceSyncAPPLE"

-- glFinalCombinerInputNV ------------------------------------------------------

glFinalCombinerInputNV
  :: MonadIO m
  => GLenum -- ^ @variable@ of type [CombinerVariableNV](Graphics-GL-Groups.html#CombinerVariableNV).
  -> GLenum -- ^ @input@ of type [CombinerRegisterNV](Graphics-GL-Groups.html#CombinerRegisterNV).
  -> GLenum -- ^ @mapping@ of type [CombinerMappingNV](Graphics-GL-Groups.html#CombinerMappingNV).
  -> GLenum -- ^ @componentUsage@ of type [CombinerComponentUsageNV](Graphics-GL-Groups.html#CombinerComponentUsageNV).
  -> m ()
glFinalCombinerInputNV v1 v2 v3 v4 = liftIO $ dyn56 ptr_glFinalCombinerInputNV v1 v2 v3 v4

{-# NOINLINE ptr_glFinalCombinerInputNV #-}
ptr_glFinalCombinerInputNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glFinalCombinerInputNV = unsafePerformIO $ getCommand "glFinalCombinerInputNV"

-- glFinish --------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glFinish.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glFinish.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFinish.xhtml OpenGL 4.x>.
glFinish
  :: MonadIO m
  => m ()
glFinish = liftIO $ dyn11 ptr_glFinish

{-# NOINLINE ptr_glFinish #-}
ptr_glFinish :: FunPtr (IO ())
ptr_glFinish = unsafePerformIO $ getCommand "glFinish"

-- glFinishAsyncSGIX -----------------------------------------------------------

glFinishAsyncSGIX
  :: MonadIO m
  => Ptr GLuint -- ^ @markerp@ pointing to @1@ element of type @GLuint@.
  -> m GLint
glFinishAsyncSGIX v1 = liftIO $ dyn288 ptr_glFinishAsyncSGIX v1

{-# NOINLINE ptr_glFinishAsyncSGIX #-}
ptr_glFinishAsyncSGIX :: FunPtr (Ptr GLuint -> IO GLint)
ptr_glFinishAsyncSGIX = unsafePerformIO $ getCommand "glFinishAsyncSGIX"

-- glFinishFenceAPPLE ----------------------------------------------------------

glFinishFenceAPPLE
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m ()
glFinishFenceAPPLE v1 = liftIO $ dyn3 ptr_glFinishFenceAPPLE v1

{-# NOINLINE ptr_glFinishFenceAPPLE #-}
ptr_glFinishFenceAPPLE :: FunPtr (GLuint -> IO ())
ptr_glFinishFenceAPPLE = unsafePerformIO $ getCommand "glFinishFenceAPPLE"

-- glFinishFenceNV -------------------------------------------------------------

glFinishFenceNV
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m ()
glFinishFenceNV v1 = liftIO $ dyn3 ptr_glFinishFenceNV v1

{-# NOINLINE ptr_glFinishFenceNV #-}
ptr_glFinishFenceNV :: FunPtr (GLuint -> IO ())
ptr_glFinishFenceNV = unsafePerformIO $ getCommand "glFinishFenceNV"

-- glFinishObjectAPPLE ---------------------------------------------------------

glFinishObjectAPPLE
  :: MonadIO m
  => GLenum -- ^ @object@ of type [ObjectTypeAPPLE](Graphics-GL-Groups.html#ObjectTypeAPPLE).
  -> GLint -- ^ @name@.
  -> m ()
glFinishObjectAPPLE v1 v2 = liftIO $ dyn58 ptr_glFinishObjectAPPLE v1 v2

{-# NOINLINE ptr_glFinishObjectAPPLE #-}
ptr_glFinishObjectAPPLE :: FunPtr (GLenum -> GLint -> IO ())
ptr_glFinishObjectAPPLE = unsafePerformIO $ getCommand "glFinishObjectAPPLE"

