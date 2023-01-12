{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F15
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

module Graphics.GL.Functions.F15 (
  glInvalidateTexImage,
  glInvalidateTexSubImage,
  glIsAsyncMarkerSGIX,
  glIsBuffer,
  glIsBufferARB,
  glIsBufferResidentNV,
  glIsCommandListNV,
  glIsEnabled,
  glIsEnabledIndexedEXT,
  glIsEnabledi,
  glIsEnablediEXT,
  glIsEnablediNV,
  glIsEnablediOES,
  glIsFenceAPPLE,
  glIsFenceNV,
  glIsFramebuffer,
  glIsFramebufferEXT,
  glIsFramebufferOES,
  glIsImageHandleResidentARB,
  glIsImageHandleResidentNV,
  glIsList,
  glIsMemoryObjectEXT,
  glIsNameAMD,
  glIsNamedBufferResidentNV,
  glIsNamedStringARB,
  glIsObjectBufferATI,
  glIsOcclusionQueryNV,
  glIsPathNV,
  glIsPointInFillPathNV,
  glIsPointInStrokePathNV,
  glIsProgram,
  glIsProgramARB,
  glIsProgramNV,
  glIsProgramPipeline,
  glIsProgramPipelineEXT,
  glIsQuery,
  glIsQueryARB,
  glIsQueryEXT,
  glIsRenderbuffer,
  glIsRenderbufferEXT,
  glIsRenderbufferOES,
  glIsSampler,
  glIsSemaphoreEXT,
  glIsShader,
  glIsStateNV,
  glIsSync,
  glIsSyncAPPLE,
  glIsTexture,
  glIsTextureEXT,
  glIsTextureHandleResidentARB,
  glIsTextureHandleResidentNV,
  glIsTransformFeedback,
  glIsTransformFeedbackNV,
  glIsVariantEnabledEXT,
  glIsVertexArray,
  glIsVertexArrayAPPLE,
  glIsVertexArrayOES,
  glIsVertexAttribEnabledAPPLE,
  glLGPUCopyImageSubDataNVX,
  glLGPUInterlockNVX,
  glLGPUNamedBufferSubDataNVX,
  glLabelObjectEXT,
  glLightEnviSGIX,
  glLightModelf,
  glLightModelfv,
  glLightModeli,
  glLightModeliv,
  glLightModelx,
  glLightModelxOES,
  glLightModelxv,
  glLightModelxvOES,
  glLightf,
  glLightfv,
  glLighti,
  glLightiv,
  glLightx,
  glLightxOES,
  glLightxv,
  glLightxvOES,
  glLineStipple,
  glLineWidth,
  glLineWidthx,
  glLineWidthxOES,
  glLinkProgram,
  glLinkProgramARB,
  glListBase,
  glListDrawCommandsStatesClientNV,
  glListParameterfSGIX,
  glListParameterfvSGIX,
  glListParameteriSGIX,
  glListParameterivSGIX,
  glLoadIdentity,
  glLoadIdentityDeformationMapSGIX,
  glLoadMatrixd,
  glLoadMatrixf,
  glLoadMatrixx,
  glLoadMatrixxOES,
  glLoadName,
  glLoadPaletteFromModelViewMatrixOES,
  glLoadProgramNV
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glInvalidateTexImage --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateTexImage.xhtml OpenGL 4.x>.
glInvalidateTexImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glInvalidateTexImage v1 v2 = liftIO $ dyn499 ptr_glInvalidateTexImage v1 v2

{-# NOINLINE ptr_glInvalidateTexImage #-}
ptr_glInvalidateTexImage :: FunPtr (GLuint -> GLint -> IO ())
ptr_glInvalidateTexImage = unsafePerformIO $ getCommand "glInvalidateTexImage"

-- glInvalidateTexSubImage -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateTexSubImage.xhtml OpenGL 4.x>.
glInvalidateTexSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glInvalidateTexSubImage v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn500 ptr_glInvalidateTexSubImage v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glInvalidateTexSubImage #-}
ptr_glInvalidateTexSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glInvalidateTexSubImage = unsafePerformIO $ getCommand "glInvalidateTexSubImage"

-- glIsAsyncMarkerSGIX ---------------------------------------------------------

glIsAsyncMarkerSGIX
  :: MonadIO m
  => GLuint -- ^ @marker@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsAsyncMarkerSGIX v1 = liftIO $ dyn284 ptr_glIsAsyncMarkerSGIX v1

{-# NOINLINE ptr_glIsAsyncMarkerSGIX #-}
ptr_glIsAsyncMarkerSGIX :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsAsyncMarkerSGIX = unsafePerformIO $ getCommand "glIsAsyncMarkerSGIX"

-- glIsBuffer ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsBuffer.xhtml OpenGL 4.x>.
glIsBuffer
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsBuffer v1 = liftIO $ dyn284 ptr_glIsBuffer v1

{-# NOINLINE ptr_glIsBuffer #-}
ptr_glIsBuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsBuffer = unsafePerformIO $ getCommand "glIsBuffer"

-- glIsBufferARB ---------------------------------------------------------------

-- | This command is an alias for 'glIsBuffer'.
glIsBufferARB
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsBufferARB v1 = liftIO $ dyn284 ptr_glIsBufferARB v1

{-# NOINLINE ptr_glIsBufferARB #-}
ptr_glIsBufferARB :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsBufferARB = unsafePerformIO $ getCommand "glIsBufferARB"

-- glIsBufferResidentNV --------------------------------------------------------

glIsBufferResidentNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsBufferResidentNV v1 = liftIO $ dyn501 ptr_glIsBufferResidentNV v1

{-# NOINLINE ptr_glIsBufferResidentNV #-}
ptr_glIsBufferResidentNV :: FunPtr (GLenum -> IO GLboolean)
ptr_glIsBufferResidentNV = unsafePerformIO $ getCommand "glIsBufferResidentNV"

-- glIsCommandListNV -----------------------------------------------------------

glIsCommandListNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsCommandListNV v1 = liftIO $ dyn284 ptr_glIsCommandListNV v1

{-# NOINLINE ptr_glIsCommandListNV #-}
ptr_glIsCommandListNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsCommandListNV = unsafePerformIO $ getCommand "glIsCommandListNV"

-- glIsEnabled -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsEnabled.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsEnabled.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsEnabled.xhtml OpenGL 4.x>.
glIsEnabled
  :: MonadIO m
  => GLenum -- ^ @cap@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnabled v1 = liftIO $ dyn501 ptr_glIsEnabled v1

{-# NOINLINE ptr_glIsEnabled #-}
ptr_glIsEnabled :: FunPtr (GLenum -> IO GLboolean)
ptr_glIsEnabled = unsafePerformIO $ getCommand "glIsEnabled"

-- glIsEnabledIndexedEXT -------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnabledIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnabledIndexedEXT v1 v2 = liftIO $ dyn502 ptr_glIsEnabledIndexedEXT v1 v2

{-# NOINLINE ptr_glIsEnabledIndexedEXT #-}
ptr_glIsEnabledIndexedEXT :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnabledIndexedEXT = unsafePerformIO $ getCommand "glIsEnabledIndexedEXT"

-- glIsEnabledi ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsEnabled.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsEnabled.xhtml OpenGL 4.x>.
glIsEnabledi
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnabledi v1 v2 = liftIO $ dyn502 ptr_glIsEnabledi v1 v2

{-# NOINLINE ptr_glIsEnabledi #-}
ptr_glIsEnabledi :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnabledi = unsafePerformIO $ getCommand "glIsEnabledi"

-- glIsEnablediEXT -------------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnablediEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnablediEXT v1 v2 = liftIO $ dyn502 ptr_glIsEnablediEXT v1 v2

{-# NOINLINE ptr_glIsEnablediEXT #-}
ptr_glIsEnablediEXT :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnablediEXT = unsafePerformIO $ getCommand "glIsEnablediEXT"

-- glIsEnablediNV --------------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnablediNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnablediNV v1 v2 = liftIO $ dyn502 ptr_glIsEnablediNV v1 v2

{-# NOINLINE ptr_glIsEnablediNV #-}
ptr_glIsEnablediNV :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnablediNV = unsafePerformIO $ getCommand "glIsEnablediNV"

-- glIsEnablediOES -------------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnablediOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnablediOES v1 v2 = liftIO $ dyn502 ptr_glIsEnablediOES v1 v2

{-# NOINLINE ptr_glIsEnablediOES #-}
ptr_glIsEnablediOES :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnablediOES = unsafePerformIO $ getCommand "glIsEnablediOES"

-- glIsFenceAPPLE --------------------------------------------------------------

glIsFenceAPPLE
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFenceAPPLE v1 = liftIO $ dyn284 ptr_glIsFenceAPPLE v1

{-# NOINLINE ptr_glIsFenceAPPLE #-}
ptr_glIsFenceAPPLE :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFenceAPPLE = unsafePerformIO $ getCommand "glIsFenceAPPLE"

-- glIsFenceNV -----------------------------------------------------------------

glIsFenceNV
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFenceNV v1 = liftIO $ dyn284 ptr_glIsFenceNV v1

{-# NOINLINE ptr_glIsFenceNV #-}
ptr_glIsFenceNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFenceNV = unsafePerformIO $ getCommand "glIsFenceNV"

-- glIsFramebuffer -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsFramebuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsFramebuffer.xhtml OpenGL 4.x>.
glIsFramebuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFramebuffer v1 = liftIO $ dyn284 ptr_glIsFramebuffer v1

{-# NOINLINE ptr_glIsFramebuffer #-}
ptr_glIsFramebuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFramebuffer = unsafePerformIO $ getCommand "glIsFramebuffer"

-- glIsFramebufferEXT ----------------------------------------------------------

-- | This command is an alias for 'glIsFramebuffer'.
glIsFramebufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFramebufferEXT v1 = liftIO $ dyn284 ptr_glIsFramebufferEXT v1

{-# NOINLINE ptr_glIsFramebufferEXT #-}
ptr_glIsFramebufferEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFramebufferEXT = unsafePerformIO $ getCommand "glIsFramebufferEXT"

-- glIsFramebufferOES ----------------------------------------------------------

glIsFramebufferOES
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFramebufferOES v1 = liftIO $ dyn284 ptr_glIsFramebufferOES v1

{-# NOINLINE ptr_glIsFramebufferOES #-}
ptr_glIsFramebufferOES :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFramebufferOES = unsafePerformIO $ getCommand "glIsFramebufferOES"

-- glIsImageHandleResidentARB --------------------------------------------------

glIsImageHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsImageHandleResidentARB v1 = liftIO $ dyn503 ptr_glIsImageHandleResidentARB v1

{-# NOINLINE ptr_glIsImageHandleResidentARB #-}
ptr_glIsImageHandleResidentARB :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsImageHandleResidentARB = unsafePerformIO $ getCommand "glIsImageHandleResidentARB"

-- glIsImageHandleResidentNV ---------------------------------------------------

glIsImageHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsImageHandleResidentNV v1 = liftIO $ dyn503 ptr_glIsImageHandleResidentNV v1

{-# NOINLINE ptr_glIsImageHandleResidentNV #-}
ptr_glIsImageHandleResidentNV :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsImageHandleResidentNV = unsafePerformIO $ getCommand "glIsImageHandleResidentNV"

-- glIsList --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsList.xml OpenGL 2.x>.
glIsList
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsList v1 = liftIO $ dyn284 ptr_glIsList v1

{-# NOINLINE ptr_glIsList #-}
ptr_glIsList :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsList = unsafePerformIO $ getCommand "glIsList"

-- glIsMemoryObjectEXT ---------------------------------------------------------

glIsMemoryObjectEXT
  :: MonadIO m
  => GLuint -- ^ @memoryObject@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsMemoryObjectEXT v1 = liftIO $ dyn284 ptr_glIsMemoryObjectEXT v1

{-# NOINLINE ptr_glIsMemoryObjectEXT #-}
ptr_glIsMemoryObjectEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsMemoryObjectEXT = unsafePerformIO $ getCommand "glIsMemoryObjectEXT"

-- glIsNameAMD -----------------------------------------------------------------

glIsNameAMD
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @name@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsNameAMD v1 v2 = liftIO $ dyn502 ptr_glIsNameAMD v1 v2

{-# NOINLINE ptr_glIsNameAMD #-}
ptr_glIsNameAMD :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsNameAMD = unsafePerformIO $ getCommand "glIsNameAMD"

-- glIsNamedBufferResidentNV ---------------------------------------------------

glIsNamedBufferResidentNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsNamedBufferResidentNV v1 = liftIO $ dyn284 ptr_glIsNamedBufferResidentNV v1

{-# NOINLINE ptr_glIsNamedBufferResidentNV #-}
ptr_glIsNamedBufferResidentNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsNamedBufferResidentNV = unsafePerformIO $ getCommand "glIsNamedBufferResidentNV"

-- glIsNamedStringARB ----------------------------------------------------------

glIsNamedStringARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsNamedStringARB v1 v2 = liftIO $ dyn504 ptr_glIsNamedStringARB v1 v2

{-# NOINLINE ptr_glIsNamedStringARB #-}
ptr_glIsNamedStringARB :: FunPtr (GLint -> Ptr GLchar -> IO GLboolean)
ptr_glIsNamedStringARB = unsafePerformIO $ getCommand "glIsNamedStringARB"

-- glIsObjectBufferATI ---------------------------------------------------------

glIsObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsObjectBufferATI v1 = liftIO $ dyn284 ptr_glIsObjectBufferATI v1

{-# NOINLINE ptr_glIsObjectBufferATI #-}
ptr_glIsObjectBufferATI :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsObjectBufferATI = unsafePerformIO $ getCommand "glIsObjectBufferATI"

-- glIsOcclusionQueryNV --------------------------------------------------------

glIsOcclusionQueryNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsOcclusionQueryNV v1 = liftIO $ dyn284 ptr_glIsOcclusionQueryNV v1

{-# NOINLINE ptr_glIsOcclusionQueryNV #-}
ptr_glIsOcclusionQueryNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsOcclusionQueryNV = unsafePerformIO $ getCommand "glIsOcclusionQueryNV"

-- glIsPathNV ------------------------------------------------------------------

glIsPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsPathNV v1 = liftIO $ dyn284 ptr_glIsPathNV v1

{-# NOINLINE ptr_glIsPathNV #-}
ptr_glIsPathNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsPathNV = unsafePerformIO $ getCommand "glIsPathNV"

-- glIsPointInFillPathNV -------------------------------------------------------

glIsPointInFillPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsPointInFillPathNV v1 v2 v3 v4 = liftIO $ dyn505 ptr_glIsPointInFillPathNV v1 v2 v3 v4

{-# NOINLINE ptr_glIsPointInFillPathNV #-}
ptr_glIsPointInFillPathNV :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> IO GLboolean)
ptr_glIsPointInFillPathNV = unsafePerformIO $ getCommand "glIsPointInFillPathNV"

-- glIsPointInStrokePathNV -----------------------------------------------------

glIsPointInStrokePathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsPointInStrokePathNV v1 v2 v3 = liftIO $ dyn506 ptr_glIsPointInStrokePathNV v1 v2 v3

{-# NOINLINE ptr_glIsPointInStrokePathNV #-}
ptr_glIsPointInStrokePathNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO GLboolean)
ptr_glIsPointInStrokePathNV = unsafePerformIO $ getCommand "glIsPointInStrokePathNV"

-- glIsProgram -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsProgram.xhtml OpenGL 4.x>.
glIsProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgram v1 = liftIO $ dyn284 ptr_glIsProgram v1

{-# NOINLINE ptr_glIsProgram #-}
ptr_glIsProgram :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgram = unsafePerformIO $ getCommand "glIsProgram"

-- glIsProgramARB --------------------------------------------------------------

glIsProgramARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgramARB v1 = liftIO $ dyn284 ptr_glIsProgramARB v1

{-# NOINLINE ptr_glIsProgramARB #-}
ptr_glIsProgramARB :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramARB = unsafePerformIO $ getCommand "glIsProgramARB"

-- glIsProgramNV ---------------------------------------------------------------

-- | This command is an alias for 'glIsProgramARB'.
glIsProgramNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgramNV v1 = liftIO $ dyn284 ptr_glIsProgramNV v1

{-# NOINLINE ptr_glIsProgramNV #-}
ptr_glIsProgramNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramNV = unsafePerformIO $ getCommand "glIsProgramNV"

-- glIsProgramPipeline ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glIsProgramPipeline.xhtml OpenGL 4.x>.
glIsProgramPipeline
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgramPipeline v1 = liftIO $ dyn284 ptr_glIsProgramPipeline v1

{-# NOINLINE ptr_glIsProgramPipeline #-}
ptr_glIsProgramPipeline :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramPipeline = unsafePerformIO $ getCommand "glIsProgramPipeline"

-- glIsProgramPipelineEXT ------------------------------------------------------

glIsProgramPipelineEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgramPipelineEXT v1 = liftIO $ dyn284 ptr_glIsProgramPipelineEXT v1

{-# NOINLINE ptr_glIsProgramPipelineEXT #-}
ptr_glIsProgramPipelineEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramPipelineEXT = unsafePerformIO $ getCommand "glIsProgramPipelineEXT"

-- glIsQuery -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsQuery.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsQuery.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsQuery.xhtml OpenGL 4.x>.
glIsQuery
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsQuery v1 = liftIO $ dyn284 ptr_glIsQuery v1

{-# NOINLINE ptr_glIsQuery #-}
ptr_glIsQuery :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsQuery = unsafePerformIO $ getCommand "glIsQuery"

-- glIsQueryARB ----------------------------------------------------------------

-- | This command is an alias for 'glIsQuery'.
glIsQueryARB
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsQueryARB v1 = liftIO $ dyn284 ptr_glIsQueryARB v1

{-# NOINLINE ptr_glIsQueryARB #-}
ptr_glIsQueryARB :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsQueryARB = unsafePerformIO $ getCommand "glIsQueryARB"

-- glIsQueryEXT ----------------------------------------------------------------

glIsQueryEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsQueryEXT v1 = liftIO $ dyn284 ptr_glIsQueryEXT v1

{-# NOINLINE ptr_glIsQueryEXT #-}
ptr_glIsQueryEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsQueryEXT = unsafePerformIO $ getCommand "glIsQueryEXT"

-- glIsRenderbuffer ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsRenderbuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsRenderbuffer.xhtml OpenGL 4.x>.
glIsRenderbuffer
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsRenderbuffer v1 = liftIO $ dyn284 ptr_glIsRenderbuffer v1

{-# NOINLINE ptr_glIsRenderbuffer #-}
ptr_glIsRenderbuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsRenderbuffer = unsafePerformIO $ getCommand "glIsRenderbuffer"

-- glIsRenderbufferEXT ---------------------------------------------------------

-- | This command is an alias for 'glIsRenderbuffer'.
glIsRenderbufferEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsRenderbufferEXT v1 = liftIO $ dyn284 ptr_glIsRenderbufferEXT v1

{-# NOINLINE ptr_glIsRenderbufferEXT #-}
ptr_glIsRenderbufferEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsRenderbufferEXT = unsafePerformIO $ getCommand "glIsRenderbufferEXT"

-- glIsRenderbufferOES ---------------------------------------------------------

glIsRenderbufferOES
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsRenderbufferOES v1 = liftIO $ dyn284 ptr_glIsRenderbufferOES v1

{-# NOINLINE ptr_glIsRenderbufferOES #-}
ptr_glIsRenderbufferOES :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsRenderbufferOES = unsafePerformIO $ getCommand "glIsRenderbufferOES"

-- glIsSampler -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsSampler.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsSampler.xhtml OpenGL 4.x>.
glIsSampler
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsSampler v1 = liftIO $ dyn284 ptr_glIsSampler v1

{-# NOINLINE ptr_glIsSampler #-}
ptr_glIsSampler :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsSampler = unsafePerformIO $ getCommand "glIsSampler"

-- glIsSemaphoreEXT ------------------------------------------------------------

glIsSemaphoreEXT
  :: MonadIO m
  => GLuint -- ^ @semaphore@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsSemaphoreEXT v1 = liftIO $ dyn284 ptr_glIsSemaphoreEXT v1

{-# NOINLINE ptr_glIsSemaphoreEXT #-}
ptr_glIsSemaphoreEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsSemaphoreEXT = unsafePerformIO $ getCommand "glIsSemaphoreEXT"

-- glIsShader ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsShader.xhtml OpenGL 4.x>.
glIsShader
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsShader v1 = liftIO $ dyn284 ptr_glIsShader v1

{-# NOINLINE ptr_glIsShader #-}
ptr_glIsShader :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsShader = unsafePerformIO $ getCommand "glIsShader"

-- glIsStateNV -----------------------------------------------------------------

glIsStateNV
  :: MonadIO m
  => GLuint -- ^ @state@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsStateNV v1 = liftIO $ dyn284 ptr_glIsStateNV v1

{-# NOINLINE ptr_glIsStateNV #-}
ptr_glIsStateNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsStateNV = unsafePerformIO $ getCommand "glIsStateNV"

-- glIsSync --------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsSync.xhtml OpenGL 4.x>.
glIsSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsSync v1 = liftIO $ dyn507 ptr_glIsSync v1

{-# NOINLINE ptr_glIsSync #-}
ptr_glIsSync :: FunPtr (GLsync -> IO GLboolean)
ptr_glIsSync = unsafePerformIO $ getCommand "glIsSync"

-- glIsSyncAPPLE ---------------------------------------------------------------

-- | This command is an alias for 'glIsSync'.
glIsSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsSyncAPPLE v1 = liftIO $ dyn507 ptr_glIsSyncAPPLE v1

{-# NOINLINE ptr_glIsSyncAPPLE #-}
ptr_glIsSyncAPPLE :: FunPtr (GLsync -> IO GLboolean)
ptr_glIsSyncAPPLE = unsafePerformIO $ getCommand "glIsSyncAPPLE"

-- glIsTexture -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsTexture.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsTexture.xhtml OpenGL 4.x>.
glIsTexture
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTexture v1 = liftIO $ dyn284 ptr_glIsTexture v1

{-# NOINLINE ptr_glIsTexture #-}
ptr_glIsTexture :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTexture = unsafePerformIO $ getCommand "glIsTexture"

-- glIsTextureEXT --------------------------------------------------------------

glIsTextureEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTextureEXT v1 = liftIO $ dyn284 ptr_glIsTextureEXT v1

{-# NOINLINE ptr_glIsTextureEXT #-}
ptr_glIsTextureEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTextureEXT = unsafePerformIO $ getCommand "glIsTextureEXT"

-- glIsTextureHandleResidentARB ------------------------------------------------

glIsTextureHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTextureHandleResidentARB v1 = liftIO $ dyn503 ptr_glIsTextureHandleResidentARB v1

{-# NOINLINE ptr_glIsTextureHandleResidentARB #-}
ptr_glIsTextureHandleResidentARB :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsTextureHandleResidentARB = unsafePerformIO $ getCommand "glIsTextureHandleResidentARB"

-- glIsTextureHandleResidentNV -------------------------------------------------

glIsTextureHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTextureHandleResidentNV v1 = liftIO $ dyn503 ptr_glIsTextureHandleResidentNV v1

{-# NOINLINE ptr_glIsTextureHandleResidentNV #-}
ptr_glIsTextureHandleResidentNV :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsTextureHandleResidentNV = unsafePerformIO $ getCommand "glIsTextureHandleResidentNV"

-- glIsTransformFeedback -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glIsTransformFeedback.xhtml OpenGL 4.x>.
glIsTransformFeedback
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTransformFeedback v1 = liftIO $ dyn284 ptr_glIsTransformFeedback v1

{-# NOINLINE ptr_glIsTransformFeedback #-}
ptr_glIsTransformFeedback :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTransformFeedback = unsafePerformIO $ getCommand "glIsTransformFeedback"

-- glIsTransformFeedbackNV -----------------------------------------------------

-- | This command is an alias for 'glIsTransformFeedback'.
glIsTransformFeedbackNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTransformFeedbackNV v1 = liftIO $ dyn284 ptr_glIsTransformFeedbackNV v1

{-# NOINLINE ptr_glIsTransformFeedbackNV #-}
ptr_glIsTransformFeedbackNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTransformFeedbackNV = unsafePerformIO $ getCommand "glIsTransformFeedbackNV"

-- glIsVariantEnabledEXT -------------------------------------------------------

glIsVariantEnabledEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @cap@ of type [VariantCapEXT](Graphics-GL-Groups.html#VariantCapEXT).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVariantEnabledEXT v1 v2 = liftIO $ dyn508 ptr_glIsVariantEnabledEXT v1 v2

{-# NOINLINE ptr_glIsVariantEnabledEXT #-}
ptr_glIsVariantEnabledEXT :: FunPtr (GLuint -> GLenum -> IO GLboolean)
ptr_glIsVariantEnabledEXT = unsafePerformIO $ getCommand "glIsVariantEnabledEXT"

-- glIsVertexArray -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsVertexArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsVertexArray.xhtml OpenGL 4.x>.
glIsVertexArray
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVertexArray v1 = liftIO $ dyn284 ptr_glIsVertexArray v1

{-# NOINLINE ptr_glIsVertexArray #-}
ptr_glIsVertexArray :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsVertexArray = unsafePerformIO $ getCommand "glIsVertexArray"

-- glIsVertexArrayAPPLE --------------------------------------------------------

-- | This command is an alias for 'glIsVertexArray'.
glIsVertexArrayAPPLE
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVertexArrayAPPLE v1 = liftIO $ dyn284 ptr_glIsVertexArrayAPPLE v1

{-# NOINLINE ptr_glIsVertexArrayAPPLE #-}
ptr_glIsVertexArrayAPPLE :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsVertexArrayAPPLE = unsafePerformIO $ getCommand "glIsVertexArrayAPPLE"

-- glIsVertexArrayOES ----------------------------------------------------------

-- | This command is an alias for 'glIsVertexArray'.
glIsVertexArrayOES
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVertexArrayOES v1 = liftIO $ dyn284 ptr_glIsVertexArrayOES v1

{-# NOINLINE ptr_glIsVertexArrayOES #-}
ptr_glIsVertexArrayOES :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsVertexArrayOES = unsafePerformIO $ getCommand "glIsVertexArrayOES"

-- glIsVertexAttribEnabledAPPLE ------------------------------------------------

glIsVertexAttribEnabledAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVertexAttribEnabledAPPLE v1 v2 = liftIO $ dyn508 ptr_glIsVertexAttribEnabledAPPLE v1 v2

{-# NOINLINE ptr_glIsVertexAttribEnabledAPPLE #-}
ptr_glIsVertexAttribEnabledAPPLE :: FunPtr (GLuint -> GLenum -> IO GLboolean)
ptr_glIsVertexAttribEnabledAPPLE = unsafePerformIO $ getCommand "glIsVertexAttribEnabledAPPLE"

-- glLGPUCopyImageSubDataNVX ---------------------------------------------------

glLGPUCopyImageSubDataNVX
  :: MonadIO m
  => GLuint -- ^ @sourceGpu@.
  -> GLbitfield -- ^ @destinationGpuMask@.
  -> GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@.
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srxY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@.
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glLGPUCopyImageSubDataNVX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 = liftIO $ dyn509 ptr_glLGPUCopyImageSubDataNVX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17

{-# NOINLINE ptr_glLGPUCopyImageSubDataNVX #-}
ptr_glLGPUCopyImageSubDataNVX :: FunPtr (GLuint -> GLbitfield -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glLGPUCopyImageSubDataNVX = unsafePerformIO $ getCommand "glLGPUCopyImageSubDataNVX"

-- glLGPUInterlockNVX ----------------------------------------------------------

glLGPUInterlockNVX
  :: MonadIO m
  => m ()
glLGPUInterlockNVX = liftIO $ dyn11 ptr_glLGPUInterlockNVX

{-# NOINLINE ptr_glLGPUInterlockNVX #-}
ptr_glLGPUInterlockNVX :: FunPtr (IO ())
ptr_glLGPUInterlockNVX = unsafePerformIO $ getCommand "glLGPUInterlockNVX"

-- glLGPUNamedBufferSubDataNVX -------------------------------------------------

glLGPUNamedBufferSubDataNVX
  :: MonadIO m
  => GLbitfield -- ^ @gpuMask@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@.
  -> m ()
glLGPUNamedBufferSubDataNVX v1 v2 v3 v4 v5 = liftIO $ dyn510 ptr_glLGPUNamedBufferSubDataNVX v1 v2 v3 v4 v5

{-# NOINLINE ptr_glLGPUNamedBufferSubDataNVX #-}
ptr_glLGPUNamedBufferSubDataNVX :: FunPtr (GLbitfield -> GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glLGPUNamedBufferSubDataNVX = unsafePerformIO $ getCommand "glLGPUNamedBufferSubDataNVX"

-- glLabelObjectEXT ------------------------------------------------------------

glLabelObjectEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @object@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@.
  -> m ()
glLabelObjectEXT v1 v2 v3 v4 = liftIO $ dyn511 ptr_glLabelObjectEXT v1 v2 v3 v4

{-# NOINLINE ptr_glLabelObjectEXT #-}
ptr_glLabelObjectEXT :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glLabelObjectEXT = unsafePerformIO $ getCommand "glLabelObjectEXT"

-- glLightEnviSGIX -------------------------------------------------------------

glLightEnviSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightEnvParameterSGIX](Graphics-GL-Groups.html#LightEnvParameterSGIX).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glLightEnviSGIX v1 v2 = liftIO $ dyn58 ptr_glLightEnviSGIX v1 v2

{-# NOINLINE ptr_glLightEnviSGIX #-}
ptr_glLightEnviSGIX :: FunPtr (GLenum -> GLint -> IO ())
ptr_glLightEnviSGIX = unsafePerformIO $ getCommand "glLightEnviSGIX"

-- glLightModelf ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModelf
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> GLfloat -- ^ @param@.
  -> m ()
glLightModelf v1 v2 = liftIO $ dyn0 ptr_glLightModelf v1 v2

{-# NOINLINE ptr_glLightModelf #-}
ptr_glLightModelf :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glLightModelf = unsafePerformIO $ getCommand "glLightModelf"

-- glLightModelfv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModelfv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glLightModelfv v1 v2 = liftIO $ dyn101 ptr_glLightModelfv v1 v2

{-# NOINLINE ptr_glLightModelfv #-}
ptr_glLightModelfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glLightModelfv = unsafePerformIO $ getCommand "glLightModelfv"

-- glLightModeli ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModeli
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> GLint -- ^ @param@.
  -> m ()
glLightModeli v1 v2 = liftIO $ dyn58 ptr_glLightModeli v1 v2

{-# NOINLINE ptr_glLightModeli #-}
ptr_glLightModeli :: FunPtr (GLenum -> GLint -> IO ())
ptr_glLightModeli = unsafePerformIO $ getCommand "glLightModeli"

-- glLightModeliv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModeliv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glLightModeliv v1 v2 = liftIO $ dyn143 ptr_glLightModeliv v1 v2

{-# NOINLINE ptr_glLightModeliv #-}
ptr_glLightModeliv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glLightModeliv = unsafePerformIO $ getCommand "glLightModeliv"

-- glLightModelx ---------------------------------------------------------------

glLightModelx
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glLightModelx v1 v2 = liftIO $ dyn1 ptr_glLightModelx v1 v2

{-# NOINLINE ptr_glLightModelx #-}
ptr_glLightModelx :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glLightModelx = unsafePerformIO $ getCommand "glLightModelx"

-- glLightModelxOES ------------------------------------------------------------

glLightModelxOES
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glLightModelxOES v1 v2 = liftIO $ dyn1 ptr_glLightModelxOES v1 v2

{-# NOINLINE ptr_glLightModelxOES #-}
ptr_glLightModelxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glLightModelxOES = unsafePerformIO $ getCommand "glLightModelxOES"

-- glLightModelxv --------------------------------------------------------------

glLightModelxv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightModelxv v1 v2 = liftIO $ dyn102 ptr_glLightModelxv v1 v2

{-# NOINLINE ptr_glLightModelxv #-}
ptr_glLightModelxv :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glLightModelxv = unsafePerformIO $ getCommand "glLightModelxv"

-- glLightModelxvOES -----------------------------------------------------------

glLightModelxvOES
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightModelxvOES v1 v2 = liftIO $ dyn102 ptr_glLightModelxvOES v1 v2

{-# NOINLINE ptr_glLightModelxvOES #-}
ptr_glLightModelxvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glLightModelxvOES = unsafePerformIO $ getCommand "glLightModelxvOES"

-- glLightf --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLightf
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glLightf v1 v2 v3 = liftIO $ dyn168 ptr_glLightf v1 v2 v3

{-# NOINLINE ptr_glLightf #-}
ptr_glLightf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glLightf = unsafePerformIO $ getCommand "glLightf"

-- glLightfv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLightfv
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glLightfv v1 v2 v3 = liftIO $ dyn139 ptr_glLightfv v1 v2 v3

{-# NOINLINE ptr_glLightfv #-}
ptr_glLightfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glLightfv = unsafePerformIO $ getCommand "glLightfv"

-- glLighti --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLighti
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glLighti v1 v2 v3 = liftIO $ dyn66 ptr_glLighti v1 v2 v3

{-# NOINLINE ptr_glLighti #-}
ptr_glLighti :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glLighti = unsafePerformIO $ getCommand "glLighti"

-- glLightiv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLightiv
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glLightiv v1 v2 v3 = liftIO $ dyn140 ptr_glLightiv v1 v2 v3

{-# NOINLINE ptr_glLightiv #-}
ptr_glLightiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glLightiv = unsafePerformIO $ getCommand "glLightiv"

-- glLightx --------------------------------------------------------------------

glLightx
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glLightx v1 v2 v3 = liftIO $ dyn169 ptr_glLightx v1 v2 v3

{-# NOINLINE ptr_glLightx #-}
ptr_glLightx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glLightx = unsafePerformIO $ getCommand "glLightx"

-- glLightxOES -----------------------------------------------------------------

glLightxOES
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glLightxOES v1 v2 v3 = liftIO $ dyn169 ptr_glLightxOES v1 v2 v3

{-# NOINLINE ptr_glLightxOES #-}
ptr_glLightxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glLightxOES = unsafePerformIO $ getCommand "glLightxOES"

-- glLightxv -------------------------------------------------------------------

glLightxv
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightxv v1 v2 v3 = liftIO $ dyn170 ptr_glLightxv v1 v2 v3

{-# NOINLINE ptr_glLightxv #-}
ptr_glLightxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glLightxv = unsafePerformIO $ getCommand "glLightxv"

-- glLightxvOES ----------------------------------------------------------------

glLightxvOES
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glLightxvOES v1 v2 v3

{-# NOINLINE ptr_glLightxvOES #-}
ptr_glLightxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glLightxvOES = unsafePerformIO $ getCommand "glLightxvOES"

-- glLineStipple ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLineStipple.xml OpenGL 2.x>.
glLineStipple
  :: MonadIO m
  => GLint -- ^ @factor@ of type @CheckedInt32@.
  -> GLushort -- ^ @pattern@ of type @LineStipple@.
  -> m ()
glLineStipple v1 v2 = liftIO $ dyn512 ptr_glLineStipple v1 v2

{-# NOINLINE ptr_glLineStipple #-}
ptr_glLineStipple :: FunPtr (GLint -> GLushort -> IO ())
ptr_glLineStipple = unsafePerformIO $ getCommand "glLineStipple"

-- glLineWidth -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glLineWidth.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glLineWidth.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glLineWidth.xhtml OpenGL 4.x>.
glLineWidth
  :: MonadIO m
  => GLfloat -- ^ @width@ of type @CheckedFloat32@.
  -> m ()
glLineWidth v1 = liftIO $ dyn85 ptr_glLineWidth v1

{-# NOINLINE ptr_glLineWidth #-}
ptr_glLineWidth :: FunPtr (GLfloat -> IO ())
ptr_glLineWidth = unsafePerformIO $ getCommand "glLineWidth"

-- glLineWidthx ----------------------------------------------------------------

glLineWidthx
  :: MonadIO m
  => GLfixed -- ^ @width@.
  -> m ()
glLineWidthx v1 = liftIO $ dyn87 ptr_glLineWidthx v1

{-# NOINLINE ptr_glLineWidthx #-}
ptr_glLineWidthx :: FunPtr (GLfixed -> IO ())
ptr_glLineWidthx = unsafePerformIO $ getCommand "glLineWidthx"

-- glLineWidthxOES -------------------------------------------------------------

glLineWidthxOES
  :: MonadIO m
  => GLfixed -- ^ @width@.
  -> m ()
glLineWidthxOES v1 = liftIO $ dyn87 ptr_glLineWidthxOES v1

{-# NOINLINE ptr_glLineWidthxOES #-}
ptr_glLineWidthxOES :: FunPtr (GLfixed -> IO ())
ptr_glLineWidthxOES = unsafePerformIO $ getCommand "glLineWidthxOES"

-- glLinkProgram ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glLinkProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glLinkProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glLinkProgram.xhtml OpenGL 4.x>.
glLinkProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glLinkProgram v1 = liftIO $ dyn3 ptr_glLinkProgram v1

{-# NOINLINE ptr_glLinkProgram #-}
ptr_glLinkProgram :: FunPtr (GLuint -> IO ())
ptr_glLinkProgram = unsafePerformIO $ getCommand "glLinkProgram"

-- glLinkProgramARB ------------------------------------------------------------

-- | This command is an alias for 'glLinkProgram'.
glLinkProgramARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> m ()
glLinkProgramARB v1 = liftIO $ dyn144 ptr_glLinkProgramARB v1

{-# NOINLINE ptr_glLinkProgramARB #-}
ptr_glLinkProgramARB :: FunPtr (GLhandleARB -> IO ())
ptr_glLinkProgramARB = unsafePerformIO $ getCommand "glLinkProgramARB"

-- glListBase ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glListBase.xml OpenGL 2.x>.
glListBase
  :: MonadIO m
  => GLuint -- ^ @base@ of type @List@.
  -> m ()
glListBase v1 = liftIO $ dyn3 ptr_glListBase v1

{-# NOINLINE ptr_glListBase #-}
ptr_glListBase :: FunPtr (GLuint -> IO ())
ptr_glListBase = unsafePerformIO $ getCommand "glListBase"

-- glListDrawCommandsStatesClientNV --------------------------------------------

glListDrawCommandsStatesClientNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> GLuint -- ^ @segment@.
  -> Ptr (Ptr a) -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> Ptr GLuint -- ^ @states@.
  -> Ptr GLuint -- ^ @fbos@.
  -> GLuint -- ^ @count@.
  -> m ()
glListDrawCommandsStatesClientNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn513 ptr_glListDrawCommandsStatesClientNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glListDrawCommandsStatesClientNV #-}
ptr_glListDrawCommandsStatesClientNV :: FunPtr (GLuint -> GLuint -> Ptr (Ptr a) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
ptr_glListDrawCommandsStatesClientNV = unsafePerformIO $ getCommand "glListDrawCommandsStatesClientNV"

-- glListParameterfSGIX --------------------------------------------------------

glListParameterfSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glListParameterfSGIX v1 v2 v3 = liftIO $ dyn514 ptr_glListParameterfSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameterfSGIX #-}
ptr_glListParameterfSGIX :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
ptr_glListParameterfSGIX = unsafePerformIO $ getCommand "glListParameterfSGIX"

-- glListParameterfvSGIX -------------------------------------------------------

glListParameterfvSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glListParameterfvSGIX v1 v2 v3 = liftIO $ dyn364 ptr_glListParameterfvSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameterfvSGIX #-}
ptr_glListParameterfvSGIX :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glListParameterfvSGIX = unsafePerformIO $ getCommand "glListParameterfvSGIX"

-- glListParameteriSGIX --------------------------------------------------------

glListParameteriSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glListParameteriSGIX v1 v2 v3 = liftIO $ dyn491 ptr_glListParameteriSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameteriSGIX #-}
ptr_glListParameteriSGIX :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glListParameteriSGIX = unsafePerformIO $ getCommand "glListParameteriSGIX"

-- glListParameterivSGIX -------------------------------------------------------

glListParameterivSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glListParameterivSGIX v1 v2 v3 = liftIO $ dyn348 ptr_glListParameterivSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameterivSGIX #-}
ptr_glListParameterivSGIX :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glListParameterivSGIX = unsafePerformIO $ getCommand "glListParameterivSGIX"

-- glLoadIdentity --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadIdentity.xml OpenGL 2.x>.
glLoadIdentity
  :: MonadIO m
  => m ()
glLoadIdentity = liftIO $ dyn11 ptr_glLoadIdentity

{-# NOINLINE ptr_glLoadIdentity #-}
ptr_glLoadIdentity :: FunPtr (IO ())
ptr_glLoadIdentity = unsafePerformIO $ getCommand "glLoadIdentity"

-- glLoadIdentityDeformationMapSGIX --------------------------------------------

glLoadIdentityDeformationMapSGIX
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [FfdMaskSGIX](Graphics-GL-Groups.html#FfdMaskSGIX).
  -> m ()
glLoadIdentityDeformationMapSGIX v1 = liftIO $ dyn75 ptr_glLoadIdentityDeformationMapSGIX v1

{-# NOINLINE ptr_glLoadIdentityDeformationMapSGIX #-}
ptr_glLoadIdentityDeformationMapSGIX :: FunPtr (GLbitfield -> IO ())
ptr_glLoadIdentityDeformationMapSGIX = unsafePerformIO $ getCommand "glLoadIdentityDeformationMapSGIX"

-- glLoadMatrixd ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadMatrix.xml OpenGL 2.x>.
glLoadMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glLoadMatrixd v1 = liftIO $ dyn42 ptr_glLoadMatrixd v1

{-# NOINLINE ptr_glLoadMatrixd #-}
ptr_glLoadMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glLoadMatrixd = unsafePerformIO $ getCommand "glLoadMatrixd"

-- glLoadMatrixf ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadMatrix.xml OpenGL 2.x>.
glLoadMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glLoadMatrixf v1 = liftIO $ dyn44 ptr_glLoadMatrixf v1

{-# NOINLINE ptr_glLoadMatrixf #-}
ptr_glLoadMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glLoadMatrixf = unsafePerformIO $ getCommand "glLoadMatrixf"

-- glLoadMatrixx ---------------------------------------------------------------

glLoadMatrixx
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glLoadMatrixx v1 = liftIO $ dyn114 ptr_glLoadMatrixx v1

{-# NOINLINE ptr_glLoadMatrixx #-}
ptr_glLoadMatrixx :: FunPtr (Ptr GLfixed -> IO ())
ptr_glLoadMatrixx = unsafePerformIO $ getCommand "glLoadMatrixx"

-- glLoadMatrixxOES ------------------------------------------------------------

glLoadMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glLoadMatrixxOES v1 = liftIO $ dyn114 ptr_glLoadMatrixxOES v1

{-# NOINLINE ptr_glLoadMatrixxOES #-}
ptr_glLoadMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glLoadMatrixxOES = unsafePerformIO $ getCommand "glLoadMatrixxOES"

-- glLoadName ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadName.xml OpenGL 2.x>.
glLoadName
  :: MonadIO m
  => GLuint -- ^ @name@ of type @SelectName@.
  -> m ()
glLoadName v1 = liftIO $ dyn3 ptr_glLoadName v1

{-# NOINLINE ptr_glLoadName #-}
ptr_glLoadName :: FunPtr (GLuint -> IO ())
ptr_glLoadName = unsafePerformIO $ getCommand "glLoadName"

-- glLoadPaletteFromModelViewMatrixOES -----------------------------------------

glLoadPaletteFromModelViewMatrixOES
  :: MonadIO m
  => m ()
glLoadPaletteFromModelViewMatrixOES = liftIO $ dyn11 ptr_glLoadPaletteFromModelViewMatrixOES

{-# NOINLINE ptr_glLoadPaletteFromModelViewMatrixOES #-}
ptr_glLoadPaletteFromModelViewMatrixOES :: FunPtr (IO ())
ptr_glLoadPaletteFromModelViewMatrixOES = unsafePerformIO $ getCommand "glLoadPaletteFromModelViewMatrixOES"

-- glLoadProgramNV -------------------------------------------------------------

glLoadProgramNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @program@ pointing to @len@ elements of type @GLubyte@.
  -> m ()
glLoadProgramNV v1 v2 v3 v4 = liftIO $ dyn515 ptr_glLoadProgramNV v1 v2 v3 v4

{-# NOINLINE ptr_glLoadProgramNV #-}
ptr_glLoadProgramNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> IO ())
ptr_glLoadProgramNV = unsafePerformIO $ getCommand "glLoadProgramNV"

