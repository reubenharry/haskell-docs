{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F03
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

module Graphics.GL.Functions.F03 (
  glClearDepthxOES,
  glClearIndex,
  glClearNamedBufferData,
  glClearNamedBufferDataEXT,
  glClearNamedBufferSubData,
  glClearNamedBufferSubDataEXT,
  glClearNamedFramebufferfi,
  glClearNamedFramebufferfv,
  glClearNamedFramebufferiv,
  glClearNamedFramebufferuiv,
  glClearPixelLocalStorageuiEXT,
  glClearStencil,
  glClearTexImage,
  glClearTexImageEXT,
  glClearTexSubImage,
  glClearTexSubImageEXT,
  glClientActiveTexture,
  glClientActiveTextureARB,
  glClientActiveVertexStreamATI,
  glClientAttribDefaultEXT,
  glClientWaitSemaphoreui64NVX,
  glClientWaitSync,
  glClientWaitSyncAPPLE,
  glClipControl,
  glClipControlEXT,
  glClipPlane,
  glClipPlanef,
  glClipPlanefIMG,
  glClipPlanefOES,
  glClipPlanex,
  glClipPlanexIMG,
  glClipPlanexOES,
  glColor3b,
  glColor3bv,
  glColor3d,
  glColor3dv,
  glColor3f,
  glColor3fVertex3fSUN,
  glColor3fVertex3fvSUN,
  glColor3fv,
  glColor3hNV,
  glColor3hvNV,
  glColor3i,
  glColor3iv,
  glColor3s,
  glColor3sv,
  glColor3ub,
  glColor3ubv,
  glColor3ui,
  glColor3uiv,
  glColor3us,
  glColor3usv,
  glColor3xOES,
  glColor3xvOES,
  glColor4b,
  glColor4bv,
  glColor4d,
  glColor4dv,
  glColor4f,
  glColor4fNormal3fVertex3fSUN,
  glColor4fNormal3fVertex3fvSUN,
  glColor4fv,
  glColor4hNV,
  glColor4hvNV,
  glColor4i,
  glColor4iv,
  glColor4s,
  glColor4sv,
  glColor4ub,
  glColor4ubVertex2fSUN,
  glColor4ubVertex2fvSUN,
  glColor4ubVertex3fSUN,
  glColor4ubVertex3fvSUN,
  glColor4ubv,
  glColor4ui,
  glColor4uiv,
  glColor4us,
  glColor4usv,
  glColor4x,
  glColor4xOES,
  glColor4xvOES,
  glColorFormatNV,
  glColorFragmentOp1ATI,
  glColorFragmentOp2ATI,
  glColorFragmentOp3ATI,
  glColorMask,
  glColorMaskIndexedEXT,
  glColorMaski,
  glColorMaskiEXT,
  glColorMaskiOES,
  glColorMaterial,
  glColorP3ui,
  glColorP3uiv,
  glColorP4ui,
  glColorP4uiv,
  glColorPointer,
  glColorPointerEXT,
  glColorPointerListIBM,
  glColorPointervINTEL,
  glColorSubTable
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glClearDepthxOES ------------------------------------------------------------

glClearDepthxOES
  :: MonadIO m
  => GLfixed -- ^ @depth@ of type @ClampedFixed@.
  -> m ()
glClearDepthxOES v1 = liftIO $ dyn87 ptr_glClearDepthxOES v1

{-# NOINLINE ptr_glClearDepthxOES #-}
ptr_glClearDepthxOES :: FunPtr (GLfixed -> IO ())
ptr_glClearDepthxOES = unsafePerformIO $ getCommand "glClearDepthxOES"

-- glClearIndex ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glClearIndex.xml OpenGL 2.x>.
glClearIndex
  :: MonadIO m
  => GLfloat -- ^ @c@ of type @MaskedColorIndexValueF@.
  -> m ()
glClearIndex v1 = liftIO $ dyn85 ptr_glClearIndex v1

{-# NOINLINE ptr_glClearIndex #-}
ptr_glClearIndex :: FunPtr (GLfloat -> IO ())
ptr_glClearIndex = unsafePerformIO $ getCommand "glClearIndex"

-- glClearNamedBufferData ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBufferData.xhtml OpenGL 4.x>.
glClearNamedBufferData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@.
  -> m ()
glClearNamedBufferData v1 v2 v3 v4 v5 = liftIO $ dyn88 ptr_glClearNamedBufferData v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearNamedBufferData #-}
ptr_glClearNamedBufferData :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferData = unsafePerformIO $ getCommand "glClearNamedBufferData"

-- glClearNamedBufferDataEXT ---------------------------------------------------

glClearNamedBufferDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearNamedBufferDataEXT v1 v2 v3 v4 v5 = liftIO $ dyn88 ptr_glClearNamedBufferDataEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearNamedBufferDataEXT #-}
ptr_glClearNamedBufferDataEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferDataEXT = unsafePerformIO $ getCommand "glClearNamedBufferDataEXT"

-- glClearNamedBufferSubData ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBufferSubData.xhtml OpenGL 4.x>.
glClearNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@.
  -> m ()
glClearNamedBufferSubData v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn89 ptr_glClearNamedBufferSubData v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glClearNamedBufferSubData #-}
ptr_glClearNamedBufferSubData :: FunPtr (GLuint -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferSubData = unsafePerformIO $ getCommand "glClearNamedBufferSubData"

-- glClearNamedBufferSubDataEXT ------------------------------------------------

glClearNamedBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizeiptr -- ^ @offset@ of type @BufferSize@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearNamedBufferSubDataEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn90 ptr_glClearNamedBufferSubDataEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glClearNamedBufferSubDataEXT #-}
ptr_glClearNamedBufferSubDataEXT :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferSubDataEXT = unsafePerformIO $ getCommand "glClearNamedBufferSubDataEXT"

-- glClearNamedFramebufferfi ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferfi
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@ of type [Buffer](Graphics-GL-Groups.html#Buffer).
  -> GLint -- ^ @drawbuffer@.
  -> GLfloat -- ^ @depth@.
  -> GLint -- ^ @stencil@.
  -> m ()
glClearNamedFramebufferfi v1 v2 v3 v4 v5 = liftIO $ dyn91 ptr_glClearNamedFramebufferfi v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearNamedFramebufferfi #-}
ptr_glClearNamedFramebufferfi :: FunPtr (GLuint -> GLenum -> GLint -> GLfloat -> GLint -> IO ())
ptr_glClearNamedFramebufferfi = unsafePerformIO $ getCommand "glClearNamedFramebufferfi"

-- glClearNamedFramebufferfv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferfv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@ of type [Buffer](Graphics-GL-Groups.html#Buffer).
  -> GLint -- ^ @drawbuffer@.
  -> Ptr GLfloat -- ^ @value@.
  -> m ()
glClearNamedFramebufferfv v1 v2 v3 v4 = liftIO $ dyn92 ptr_glClearNamedFramebufferfv v1 v2 v3 v4

{-# NOINLINE ptr_glClearNamedFramebufferfv #-}
ptr_glClearNamedFramebufferfv :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLfloat -> IO ())
ptr_glClearNamedFramebufferfv = unsafePerformIO $ getCommand "glClearNamedFramebufferfv"

-- glClearNamedFramebufferiv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferiv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@ of type [Buffer](Graphics-GL-Groups.html#Buffer).
  -> GLint -- ^ @drawbuffer@.
  -> Ptr GLint -- ^ @value@.
  -> m ()
glClearNamedFramebufferiv v1 v2 v3 v4 = liftIO $ dyn93 ptr_glClearNamedFramebufferiv v1 v2 v3 v4

{-# NOINLINE ptr_glClearNamedFramebufferiv #-}
ptr_glClearNamedFramebufferiv :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLint -> IO ())
ptr_glClearNamedFramebufferiv = unsafePerformIO $ getCommand "glClearNamedFramebufferiv"

-- glClearNamedFramebufferuiv --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferuiv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@ of type [Buffer](Graphics-GL-Groups.html#Buffer).
  -> GLint -- ^ @drawbuffer@.
  -> Ptr GLuint -- ^ @value@.
  -> m ()
glClearNamedFramebufferuiv v1 v2 v3 v4 = liftIO $ dyn94 ptr_glClearNamedFramebufferuiv v1 v2 v3 v4

{-# NOINLINE ptr_glClearNamedFramebufferuiv #-}
ptr_glClearNamedFramebufferuiv :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLuint -> IO ())
ptr_glClearNamedFramebufferuiv = unsafePerformIO $ getCommand "glClearNamedFramebufferuiv"

-- glClearPixelLocalStorageuiEXT -----------------------------------------------

glClearPixelLocalStorageuiEXT
  :: MonadIO m
  => GLsizei -- ^ @offset@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @values@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glClearPixelLocalStorageuiEXT v1 v2 v3 = liftIO $ dyn95 ptr_glClearPixelLocalStorageuiEXT v1 v2 v3

{-# NOINLINE ptr_glClearPixelLocalStorageuiEXT #-}
ptr_glClearPixelLocalStorageuiEXT :: FunPtr (GLsizei -> GLsizei -> Ptr GLuint -> IO ())
ptr_glClearPixelLocalStorageuiEXT = unsafePerformIO $ getCommand "glClearPixelLocalStorageuiEXT"

-- glClearStencil --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glClearStencil.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glClearStencil.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearStencil.xhtml OpenGL 4.x>.
glClearStencil
  :: MonadIO m
  => GLint -- ^ @s@ of type @StencilValue@.
  -> m ()
glClearStencil v1 = liftIO $ dyn13 ptr_glClearStencil v1

{-# NOINLINE ptr_glClearStencil #-}
ptr_glClearStencil :: FunPtr (GLint -> IO ())
ptr_glClearStencil = unsafePerformIO $ getCommand "glClearStencil"

-- glClearTexImage -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearTexImage.xhtml OpenGL 4.x>.
glClearTexImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearTexImage v1 v2 v3 v4 v5 = liftIO $ dyn96 ptr_glClearTexImage v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearTexImage #-}
ptr_glClearTexImage :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearTexImage = unsafePerformIO $ getCommand "glClearTexImage"

-- glClearTexImageEXT ----------------------------------------------------------

-- | This command is an alias for 'glClearTexImage'.
glClearTexImageEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearTexImageEXT v1 v2 v3 v4 v5 = liftIO $ dyn96 ptr_glClearTexImageEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearTexImageEXT #-}
ptr_glClearTexImageEXT :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearTexImageEXT = unsafePerformIO $ getCommand "glClearTexImageEXT"

-- glClearTexSubImage ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearTexSubImage.xhtml OpenGL 4.x>.
glClearTexSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearTexSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn97 ptr_glClearTexSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glClearTexSubImage #-}
ptr_glClearTexSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearTexSubImage = unsafePerformIO $ getCommand "glClearTexSubImage"

-- glClearTexSubImageEXT -------------------------------------------------------

-- | This command is an alias for 'glClearTexSubImage'.
glClearTexSubImageEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearTexSubImageEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn97 ptr_glClearTexSubImageEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glClearTexSubImageEXT #-}
ptr_glClearTexSubImageEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearTexSubImageEXT = unsafePerformIO $ getCommand "glClearTexSubImageEXT"

-- glClientActiveTexture -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glClientActiveTexture.xml OpenGL 2.x>.
glClientActiveTexture
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> m ()
glClientActiveTexture v1 = liftIO $ dyn5 ptr_glClientActiveTexture v1

{-# NOINLINE ptr_glClientActiveTexture #-}
ptr_glClientActiveTexture :: FunPtr (GLenum -> IO ())
ptr_glClientActiveTexture = unsafePerformIO $ getCommand "glClientActiveTexture"

-- glClientActiveTextureARB ----------------------------------------------------

-- | This command is an alias for 'glClientActiveTexture'.
glClientActiveTextureARB
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> m ()
glClientActiveTextureARB v1 = liftIO $ dyn5 ptr_glClientActiveTextureARB v1

{-# NOINLINE ptr_glClientActiveTextureARB #-}
ptr_glClientActiveTextureARB :: FunPtr (GLenum -> IO ())
ptr_glClientActiveTextureARB = unsafePerformIO $ getCommand "glClientActiveTextureARB"

-- glClientActiveVertexStreamATI -----------------------------------------------

glClientActiveVertexStreamATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> m ()
glClientActiveVertexStreamATI v1 = liftIO $ dyn5 ptr_glClientActiveVertexStreamATI v1

{-# NOINLINE ptr_glClientActiveVertexStreamATI #-}
ptr_glClientActiveVertexStreamATI :: FunPtr (GLenum -> IO ())
ptr_glClientActiveVertexStreamATI = unsafePerformIO $ getCommand "glClientActiveVertexStreamATI"

-- glClientAttribDefaultEXT ----------------------------------------------------

glClientAttribDefaultEXT
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [ClientAttribMask](Graphics-GL-Groups.html#ClientAttribMask).
  -> m ()
glClientAttribDefaultEXT v1 = liftIO $ dyn75 ptr_glClientAttribDefaultEXT v1

{-# NOINLINE ptr_glClientAttribDefaultEXT #-}
ptr_glClientAttribDefaultEXT :: FunPtr (GLbitfield -> IO ())
ptr_glClientAttribDefaultEXT = unsafePerformIO $ getCommand "glClientAttribDefaultEXT"

-- glClientWaitSemaphoreui64NVX ------------------------------------------------

glClientWaitSemaphoreui64NVX
  :: MonadIO m
  => GLsizei -- ^ @fenceObjectCount@.
  -> Ptr GLuint -- ^ @semaphoreArray@ pointing to @fenceObjectCount@ elements of type @GLuint@.
  -> Ptr GLuint64 -- ^ @fenceValueArray@ pointing to @fenceObjectCount@ elements of type @GLuint64@.
  -> m ()
glClientWaitSemaphoreui64NVX v1 v2 v3 = liftIO $ dyn98 ptr_glClientWaitSemaphoreui64NVX v1 v2 v3

{-# NOINLINE ptr_glClientWaitSemaphoreui64NVX #-}
ptr_glClientWaitSemaphoreui64NVX :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLuint64 -> IO ())
ptr_glClientWaitSemaphoreui64NVX = unsafePerformIO $ getCommand "glClientWaitSemaphoreui64NVX"

-- glClientWaitSync ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glClientWaitSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClientWaitSync.xhtml OpenGL 4.x>.
glClientWaitSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> GLbitfield -- ^ @flags@ of type [SyncObjectMask](Graphics-GL-Groups.html#SyncObjectMask).
  -> GLuint64 -- ^ @timeout@.
  -> m GLenum -- ^ of type [SyncStatus](Graphics-GL-Groups.html#SyncStatus).
glClientWaitSync v1 v2 v3 = liftIO $ dyn99 ptr_glClientWaitSync v1 v2 v3

{-# NOINLINE ptr_glClientWaitSync #-}
ptr_glClientWaitSync :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO GLenum)
ptr_glClientWaitSync = unsafePerformIO $ getCommand "glClientWaitSync"

-- glClientWaitSyncAPPLE -------------------------------------------------------

-- | This command is an alias for 'glClientWaitSync'.
glClientWaitSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> GLbitfield -- ^ @flags@ of type [SyncObjectMask](Graphics-GL-Groups.html#SyncObjectMask).
  -> GLuint64 -- ^ @timeout@.
  -> m GLenum -- ^ of type [SyncStatus](Graphics-GL-Groups.html#SyncStatus).
glClientWaitSyncAPPLE v1 v2 v3 = liftIO $ dyn99 ptr_glClientWaitSyncAPPLE v1 v2 v3

{-# NOINLINE ptr_glClientWaitSyncAPPLE #-}
ptr_glClientWaitSyncAPPLE :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO GLenum)
ptr_glClientWaitSyncAPPLE = unsafePerformIO $ getCommand "glClientWaitSyncAPPLE"

-- glClipControl ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClipControl.xhtml OpenGL 4.x>.
glClipControl
  :: MonadIO m
  => GLenum -- ^ @origin@ of type [ClipControlOrigin](Graphics-GL-Groups.html#ClipControlOrigin).
  -> GLenum -- ^ @depth@ of type [ClipControlDepth](Graphics-GL-Groups.html#ClipControlDepth).
  -> m ()
glClipControl v1 v2 = liftIO $ dyn54 ptr_glClipControl v1 v2

{-# NOINLINE ptr_glClipControl #-}
ptr_glClipControl :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glClipControl = unsafePerformIO $ getCommand "glClipControl"

-- glClipControlEXT ------------------------------------------------------------

-- | This command is an alias for 'glClipControl'.
glClipControlEXT
  :: MonadIO m
  => GLenum -- ^ @origin@.
  -> GLenum -- ^ @depth@.
  -> m ()
glClipControlEXT v1 v2 = liftIO $ dyn54 ptr_glClipControlEXT v1 v2

{-# NOINLINE ptr_glClipControlEXT #-}
ptr_glClipControlEXT :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glClipControlEXT = unsafePerformIO $ getCommand "glClipControlEXT"

-- glClipPlane -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glClipPlane.xml OpenGL 2.x>.
glClipPlane
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLdouble -- ^ @equation@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glClipPlane v1 v2 = liftIO $ dyn100 ptr_glClipPlane v1 v2

{-# NOINLINE ptr_glClipPlane #-}
ptr_glClipPlane :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glClipPlane = unsafePerformIO $ getCommand "glClipPlane"

-- glClipPlanef ----------------------------------------------------------------

glClipPlanef
  :: MonadIO m
  => GLenum -- ^ @p@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfloat -- ^ @eqn@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glClipPlanef v1 v2 = liftIO $ dyn101 ptr_glClipPlanef v1 v2

{-# NOINLINE ptr_glClipPlanef #-}
ptr_glClipPlanef :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glClipPlanef = unsafePerformIO $ getCommand "glClipPlanef"

-- glClipPlanefIMG -------------------------------------------------------------

glClipPlanefIMG
  :: MonadIO m
  => GLenum -- ^ @p@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfloat -- ^ @eqn@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glClipPlanefIMG v1 v2 = liftIO $ dyn101 ptr_glClipPlanefIMG v1 v2

{-# NOINLINE ptr_glClipPlanefIMG #-}
ptr_glClipPlanefIMG :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glClipPlanefIMG = unsafePerformIO $ getCommand "glClipPlanefIMG"

-- glClipPlanefOES -------------------------------------------------------------

glClipPlanefOES
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfloat -- ^ @equation@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glClipPlanefOES v1 v2 = liftIO $ dyn101 ptr_glClipPlanefOES v1 v2

{-# NOINLINE ptr_glClipPlanefOES #-}
ptr_glClipPlanefOES :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glClipPlanefOES = unsafePerformIO $ getCommand "glClipPlanefOES"

-- glClipPlanex ----------------------------------------------------------------

glClipPlanex
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glClipPlanex v1 v2 = liftIO $ dyn102 ptr_glClipPlanex v1 v2

{-# NOINLINE ptr_glClipPlanex #-}
ptr_glClipPlanex :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glClipPlanex = unsafePerformIO $ getCommand "glClipPlanex"

-- glClipPlanexIMG -------------------------------------------------------------

glClipPlanexIMG
  :: MonadIO m
  => GLenum -- ^ @p@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfixed -- ^ @eqn@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glClipPlanexIMG v1 v2 = liftIO $ dyn102 ptr_glClipPlanexIMG v1 v2

{-# NOINLINE ptr_glClipPlanexIMG #-}
ptr_glClipPlanexIMG :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glClipPlanexIMG = unsafePerformIO $ getCommand "glClipPlanexIMG"

-- glClipPlanexOES -------------------------------------------------------------

glClipPlanexOES
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glClipPlanexOES v1 v2 = liftIO $ dyn102 ptr_glClipPlanexOES v1 v2

{-# NOINLINE ptr_glClipPlanexOES #-}
ptr_glClipPlanexOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glClipPlanexOES = unsafePerformIO $ getCommand "glClipPlanexOES"

-- glColor3b -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3bv'.
glColor3b
  :: MonadIO m
  => GLbyte -- ^ @red@ of type @ColorB@.
  -> GLbyte -- ^ @green@ of type @ColorB@.
  -> GLbyte -- ^ @blue@ of type @ColorB@.
  -> m ()
glColor3b v1 v2 v3 = liftIO $ dyn39 ptr_glColor3b v1 v2 v3

{-# NOINLINE ptr_glColor3b #-}
ptr_glColor3b :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glColor3b = unsafePerformIO $ getCommand "glColor3b"

-- glColor3bv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3bv
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @ColorB@.
  -> m ()
glColor3bv v1 = liftIO $ dyn40 ptr_glColor3bv v1

{-# NOINLINE ptr_glColor3bv #-}
ptr_glColor3bv :: FunPtr (Ptr GLbyte -> IO ())
ptr_glColor3bv = unsafePerformIO $ getCommand "glColor3bv"

-- glColor3d -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3dv'.
glColor3d
  :: MonadIO m
  => GLdouble -- ^ @red@ of type @ColorD@.
  -> GLdouble -- ^ @green@ of type @ColorD@.
  -> GLdouble -- ^ @blue@ of type @ColorD@.
  -> m ()
glColor3d v1 v2 v3 = liftIO $ dyn41 ptr_glColor3d v1 v2 v3

{-# NOINLINE ptr_glColor3d #-}
ptr_glColor3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glColor3d = unsafePerformIO $ getCommand "glColor3d"

-- glColor3dv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @ColorD@.
  -> m ()
glColor3dv v1 = liftIO $ dyn42 ptr_glColor3dv v1

{-# NOINLINE ptr_glColor3dv #-}
ptr_glColor3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glColor3dv = unsafePerformIO $ getCommand "glColor3dv"

-- glColor3f -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3fv'.
glColor3f
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> m ()
glColor3f v1 v2 v3 = liftIO $ dyn43 ptr_glColor3f v1 v2 v3

{-# NOINLINE ptr_glColor3f #-}
ptr_glColor3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor3f = unsafePerformIO $ getCommand "glColor3f"

-- glColor3fVertex3fSUN --------------------------------------------------------

glColor3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn103 ptr_glColor3fVertex3fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColor3fVertex3fSUN #-}
ptr_glColor3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor3fVertex3fSUN = unsafePerformIO $ getCommand "glColor3fVertex3fSUN"

-- glColor3fVertex3fvSUN -------------------------------------------------------

glColor3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @c@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glColor3fVertex3fvSUN v1 v2 = liftIO $ dyn104 ptr_glColor3fVertex3fvSUN v1 v2

{-# NOINLINE ptr_glColor3fVertex3fvSUN #-}
ptr_glColor3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glColor3fVertex3fvSUN = unsafePerformIO $ getCommand "glColor3fVertex3fvSUN"

-- glColor3fv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @ColorF@.
  -> m ()
glColor3fv v1 = liftIO $ dyn44 ptr_glColor3fv v1

{-# NOINLINE ptr_glColor3fv #-}
ptr_glColor3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glColor3fv = unsafePerformIO $ getCommand "glColor3fv"

-- glColor3hNV -----------------------------------------------------------------

-- | The vector equivalent of this command is 'glColor3hvNV'.
glColor3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @red@ of type @Half16NV@.
  -> GLhalfNV -- ^ @green@ of type @Half16NV@.
  -> GLhalfNV -- ^ @blue@ of type @Half16NV@.
  -> m ()
glColor3hNV v1 v2 v3 = liftIO $ dyn105 ptr_glColor3hNV v1 v2 v3

{-# NOINLINE ptr_glColor3hNV #-}
ptr_glColor3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glColor3hNV = unsafePerformIO $ getCommand "glColor3hNV"

-- glColor3hvNV ----------------------------------------------------------------

glColor3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glColor3hvNV v1 = liftIO $ dyn106 ptr_glColor3hvNV v1

{-# NOINLINE ptr_glColor3hvNV #-}
ptr_glColor3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glColor3hvNV = unsafePerformIO $ getCommand "glColor3hvNV"

-- glColor3i -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3iv'.
glColor3i
  :: MonadIO m
  => GLint -- ^ @red@ of type @ColorI@.
  -> GLint -- ^ @green@ of type @ColorI@.
  -> GLint -- ^ @blue@ of type @ColorI@.
  -> m ()
glColor3i v1 v2 v3 = liftIO $ dyn45 ptr_glColor3i v1 v2 v3

{-# NOINLINE ptr_glColor3i #-}
ptr_glColor3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glColor3i = unsafePerformIO $ getCommand "glColor3i"

-- glColor3iv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @ColorI@.
  -> m ()
glColor3iv v1 = liftIO $ dyn46 ptr_glColor3iv v1

{-# NOINLINE ptr_glColor3iv #-}
ptr_glColor3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glColor3iv = unsafePerformIO $ getCommand "glColor3iv"

-- glColor3s -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3sv'.
glColor3s
  :: MonadIO m
  => GLshort -- ^ @red@ of type @ColorS@.
  -> GLshort -- ^ @green@ of type @ColorS@.
  -> GLshort -- ^ @blue@ of type @ColorS@.
  -> m ()
glColor3s v1 v2 v3 = liftIO $ dyn47 ptr_glColor3s v1 v2 v3

{-# NOINLINE ptr_glColor3s #-}
ptr_glColor3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glColor3s = unsafePerformIO $ getCommand "glColor3s"

-- glColor3sv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @ColorS@.
  -> m ()
glColor3sv v1 = liftIO $ dyn48 ptr_glColor3sv v1

{-# NOINLINE ptr_glColor3sv #-}
ptr_glColor3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glColor3sv = unsafePerformIO $ getCommand "glColor3sv"

-- glColor3ub ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3ubv'.
glColor3ub
  :: MonadIO m
  => GLubyte -- ^ @red@ of type @ColorUB@.
  -> GLubyte -- ^ @green@ of type @ColorUB@.
  -> GLubyte -- ^ @blue@ of type @ColorUB@.
  -> m ()
glColor3ub v1 v2 v3 = liftIO $ dyn107 ptr_glColor3ub v1 v2 v3

{-# NOINLINE ptr_glColor3ub #-}
ptr_glColor3ub :: FunPtr (GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glColor3ub = unsafePerformIO $ getCommand "glColor3ub"

-- glColor3ubv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3ubv
  :: MonadIO m
  => Ptr GLubyte -- ^ @v@ pointing to @3@ elements of type @ColorUB@.
  -> m ()
glColor3ubv v1 = liftIO $ dyn108 ptr_glColor3ubv v1

{-# NOINLINE ptr_glColor3ubv #-}
ptr_glColor3ubv :: FunPtr (Ptr GLubyte -> IO ())
ptr_glColor3ubv = unsafePerformIO $ getCommand "glColor3ubv"

-- glColor3ui ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3uiv'.
glColor3ui
  :: MonadIO m
  => GLuint -- ^ @red@ of type @ColorUI@.
  -> GLuint -- ^ @green@ of type @ColorUI@.
  -> GLuint -- ^ @blue@ of type @ColorUI@.
  -> m ()
glColor3ui v1 v2 v3 = liftIO $ dyn109 ptr_glColor3ui v1 v2 v3

{-# NOINLINE ptr_glColor3ui #-}
ptr_glColor3ui :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glColor3ui = unsafePerformIO $ getCommand "glColor3ui"

-- glColor3uiv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3uiv
  :: MonadIO m
  => Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @ColorUI@.
  -> m ()
glColor3uiv v1 = liftIO $ dyn110 ptr_glColor3uiv v1

{-# NOINLINE ptr_glColor3uiv #-}
ptr_glColor3uiv :: FunPtr (Ptr GLuint -> IO ())
ptr_glColor3uiv = unsafePerformIO $ getCommand "glColor3uiv"

-- glColor3us ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3usv'.
glColor3us
  :: MonadIO m
  => GLushort -- ^ @red@ of type @ColorUS@.
  -> GLushort -- ^ @green@ of type @ColorUS@.
  -> GLushort -- ^ @blue@ of type @ColorUS@.
  -> m ()
glColor3us v1 v2 v3 = liftIO $ dyn111 ptr_glColor3us v1 v2 v3

{-# NOINLINE ptr_glColor3us #-}
ptr_glColor3us :: FunPtr (GLushort -> GLushort -> GLushort -> IO ())
ptr_glColor3us = unsafePerformIO $ getCommand "glColor3us"

-- glColor3usv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3usv
  :: MonadIO m
  => Ptr GLushort -- ^ @v@ pointing to @3@ elements of type @ColorUS@.
  -> m ()
glColor3usv v1 = liftIO $ dyn112 ptr_glColor3usv v1

{-# NOINLINE ptr_glColor3usv #-}
ptr_glColor3usv :: FunPtr (Ptr GLushort -> IO ())
ptr_glColor3usv = unsafePerformIO $ getCommand "glColor3usv"

-- glColor3xOES ----------------------------------------------------------------

glColor3xOES
  :: MonadIO m
  => GLfixed -- ^ @red@.
  -> GLfixed -- ^ @green@.
  -> GLfixed -- ^ @blue@.
  -> m ()
glColor3xOES v1 v2 v3 = liftIO $ dyn113 ptr_glColor3xOES v1 v2 v3

{-# NOINLINE ptr_glColor3xOES #-}
ptr_glColor3xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glColor3xOES = unsafePerformIO $ getCommand "glColor3xOES"

-- glColor3xvOES ---------------------------------------------------------------

glColor3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @components@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glColor3xvOES v1 = liftIO $ dyn114 ptr_glColor3xvOES v1

{-# NOINLINE ptr_glColor3xvOES #-}
ptr_glColor3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glColor3xvOES = unsafePerformIO $ getCommand "glColor3xvOES"

-- glColor4b -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4bv'.
glColor4b
  :: MonadIO m
  => GLbyte -- ^ @red@ of type @ColorB@.
  -> GLbyte -- ^ @green@ of type @ColorB@.
  -> GLbyte -- ^ @blue@ of type @ColorB@.
  -> GLbyte -- ^ @alpha@ of type @ColorB@.
  -> m ()
glColor4b v1 v2 v3 v4 = liftIO $ dyn115 ptr_glColor4b v1 v2 v3 v4

{-# NOINLINE ptr_glColor4b #-}
ptr_glColor4b :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glColor4b = unsafePerformIO $ getCommand "glColor4b"

-- glColor4bv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4bv
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @ColorB@.
  -> m ()
glColor4bv v1 = liftIO $ dyn40 ptr_glColor4bv v1

{-# NOINLINE ptr_glColor4bv #-}
ptr_glColor4bv :: FunPtr (Ptr GLbyte -> IO ())
ptr_glColor4bv = unsafePerformIO $ getCommand "glColor4bv"

-- glColor4d -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4dv'.
glColor4d
  :: MonadIO m
  => GLdouble -- ^ @red@ of type @ColorD@.
  -> GLdouble -- ^ @green@ of type @ColorD@.
  -> GLdouble -- ^ @blue@ of type @ColorD@.
  -> GLdouble -- ^ @alpha@ of type @ColorD@.
  -> m ()
glColor4d v1 v2 v3 v4 = liftIO $ dyn116 ptr_glColor4d v1 v2 v3 v4

{-# NOINLINE ptr_glColor4d #-}
ptr_glColor4d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glColor4d = unsafePerformIO $ getCommand "glColor4d"

-- glColor4dv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @ColorD@.
  -> m ()
glColor4dv v1 = liftIO $ dyn42 ptr_glColor4dv v1

{-# NOINLINE ptr_glColor4dv #-}
ptr_glColor4dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glColor4dv = unsafePerformIO $ getCommand "glColor4dv"

-- glColor4f -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4fv'.
glColor4f
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> GLfloat -- ^ @alpha@ of type @ColorF@.
  -> m ()
glColor4f v1 v2 v3 v4 = liftIO $ dyn52 ptr_glColor4f v1 v2 v3 v4

{-# NOINLINE ptr_glColor4f #-}
ptr_glColor4f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor4f = unsafePerformIO $ getCommand "glColor4f"

-- glColor4fNormal3fVertex3fSUN ------------------------------------------------

glColor4fNormal3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @r@.
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
glColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn117 ptr_glColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glColor4fNormal3fVertex3fSUN #-}
ptr_glColor4fNormal3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor4fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glColor4fNormal3fVertex3fSUN"

-- glColor4fNormal3fVertex3fvSUN -----------------------------------------------

glColor4fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glColor4fNormal3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn118 ptr_glColor4fNormal3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glColor4fNormal3fVertex3fvSUN #-}
ptr_glColor4fNormal3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glColor4fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glColor4fNormal3fVertex3fvSUN"

-- glColor4fv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @ColorF@.
  -> m ()
glColor4fv v1 = liftIO $ dyn44 ptr_glColor4fv v1

{-# NOINLINE ptr_glColor4fv #-}
ptr_glColor4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glColor4fv = unsafePerformIO $ getCommand "glColor4fv"

-- glColor4hNV -----------------------------------------------------------------

-- | The vector equivalent of this command is 'glColor4hvNV'.
glColor4hNV
  :: MonadIO m
  => GLhalfNV -- ^ @red@ of type @Half16NV@.
  -> GLhalfNV -- ^ @green@ of type @Half16NV@.
  -> GLhalfNV -- ^ @blue@ of type @Half16NV@.
  -> GLhalfNV -- ^ @alpha@ of type @Half16NV@.
  -> m ()
glColor4hNV v1 v2 v3 v4 = liftIO $ dyn119 ptr_glColor4hNV v1 v2 v3 v4

{-# NOINLINE ptr_glColor4hNV #-}
ptr_glColor4hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glColor4hNV = unsafePerformIO $ getCommand "glColor4hNV"

-- glColor4hvNV ----------------------------------------------------------------

glColor4hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glColor4hvNV v1 = liftIO $ dyn106 ptr_glColor4hvNV v1

{-# NOINLINE ptr_glColor4hvNV #-}
ptr_glColor4hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glColor4hvNV = unsafePerformIO $ getCommand "glColor4hvNV"

-- glColor4i -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4iv'.
glColor4i
  :: MonadIO m
  => GLint -- ^ @red@ of type @ColorI@.
  -> GLint -- ^ @green@ of type @ColorI@.
  -> GLint -- ^ @blue@ of type @ColorI@.
  -> GLint -- ^ @alpha@ of type @ColorI@.
  -> m ()
glColor4i v1 v2 v3 v4 = liftIO $ dyn82 ptr_glColor4i v1 v2 v3 v4

{-# NOINLINE ptr_glColor4i #-}
ptr_glColor4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glColor4i = unsafePerformIO $ getCommand "glColor4i"

-- glColor4iv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @ColorI@.
  -> m ()
glColor4iv v1 = liftIO $ dyn46 ptr_glColor4iv v1

{-# NOINLINE ptr_glColor4iv #-}
ptr_glColor4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glColor4iv = unsafePerformIO $ getCommand "glColor4iv"

-- glColor4s -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4sv'.
glColor4s
  :: MonadIO m
  => GLshort -- ^ @red@ of type @ColorS@.
  -> GLshort -- ^ @green@ of type @ColorS@.
  -> GLshort -- ^ @blue@ of type @ColorS@.
  -> GLshort -- ^ @alpha@ of type @ColorS@.
  -> m ()
glColor4s v1 v2 v3 v4 = liftIO $ dyn120 ptr_glColor4s v1 v2 v3 v4

{-# NOINLINE ptr_glColor4s #-}
ptr_glColor4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glColor4s = unsafePerformIO $ getCommand "glColor4s"

-- glColor4sv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @ColorS@.
  -> m ()
glColor4sv v1 = liftIO $ dyn48 ptr_glColor4sv v1

{-# NOINLINE ptr_glColor4sv #-}
ptr_glColor4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glColor4sv = unsafePerformIO $ getCommand "glColor4sv"

-- glColor4ub ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4ubv'.
glColor4ub
  :: MonadIO m
  => GLubyte -- ^ @red@ of type @ColorUB@.
  -> GLubyte -- ^ @green@ of type @ColorUB@.
  -> GLubyte -- ^ @blue@ of type @ColorUB@.
  -> GLubyte -- ^ @alpha@ of type @ColorUB@.
  -> m ()
glColor4ub v1 v2 v3 v4 = liftIO $ dyn121 ptr_glColor4ub v1 v2 v3 v4

{-# NOINLINE ptr_glColor4ub #-}
ptr_glColor4ub :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glColor4ub = unsafePerformIO $ getCommand "glColor4ub"

-- glColor4ubVertex2fSUN -------------------------------------------------------

glColor4ubVertex2fSUN
  :: MonadIO m
  => GLubyte -- ^ @r@.
  -> GLubyte -- ^ @g@.
  -> GLubyte -- ^ @b@.
  -> GLubyte -- ^ @a@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glColor4ubVertex2fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn122 ptr_glColor4ubVertex2fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColor4ubVertex2fSUN #-}
ptr_glColor4ubVertex2fSUN :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> IO ())
ptr_glColor4ubVertex2fSUN = unsafePerformIO $ getCommand "glColor4ubVertex2fSUN"

-- glColor4ubVertex2fvSUN ------------------------------------------------------

glColor4ubVertex2fvSUN
  :: MonadIO m
  => Ptr GLubyte -- ^ @c@ pointing to @4@ elements of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glColor4ubVertex2fvSUN v1 v2 = liftIO $ dyn123 ptr_glColor4ubVertex2fvSUN v1 v2

{-# NOINLINE ptr_glColor4ubVertex2fvSUN #-}
ptr_glColor4ubVertex2fvSUN :: FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glColor4ubVertex2fvSUN = unsafePerformIO $ getCommand "glColor4ubVertex2fvSUN"

-- glColor4ubVertex3fSUN -------------------------------------------------------

glColor4ubVertex3fSUN
  :: MonadIO m
  => GLubyte -- ^ @r@.
  -> GLubyte -- ^ @g@.
  -> GLubyte -- ^ @b@.
  -> GLubyte -- ^ @a@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn124 ptr_glColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glColor4ubVertex3fSUN #-}
ptr_glColor4ubVertex3fSUN :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor4ubVertex3fSUN = unsafePerformIO $ getCommand "glColor4ubVertex3fSUN"

-- glColor4ubVertex3fvSUN ------------------------------------------------------

glColor4ubVertex3fvSUN
  :: MonadIO m
  => Ptr GLubyte -- ^ @c@ pointing to @4@ elements of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glColor4ubVertex3fvSUN v1 v2 = liftIO $ dyn123 ptr_glColor4ubVertex3fvSUN v1 v2

{-# NOINLINE ptr_glColor4ubVertex3fvSUN #-}
ptr_glColor4ubVertex3fvSUN :: FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glColor4ubVertex3fvSUN = unsafePerformIO $ getCommand "glColor4ubVertex3fvSUN"

-- glColor4ubv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4ubv
  :: MonadIO m
  => Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @ColorUB@.
  -> m ()
glColor4ubv v1 = liftIO $ dyn108 ptr_glColor4ubv v1

{-# NOINLINE ptr_glColor4ubv #-}
ptr_glColor4ubv :: FunPtr (Ptr GLubyte -> IO ())
ptr_glColor4ubv = unsafePerformIO $ getCommand "glColor4ubv"

-- glColor4ui ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4uiv'.
glColor4ui
  :: MonadIO m
  => GLuint -- ^ @red@ of type @ColorUI@.
  -> GLuint -- ^ @green@ of type @ColorUI@.
  -> GLuint -- ^ @blue@ of type @ColorUI@.
  -> GLuint -- ^ @alpha@ of type @ColorUI@.
  -> m ()
glColor4ui v1 v2 v3 v4 = liftIO $ dyn83 ptr_glColor4ui v1 v2 v3 v4

{-# NOINLINE ptr_glColor4ui #-}
ptr_glColor4ui :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColor4ui = unsafePerformIO $ getCommand "glColor4ui"

-- glColor4uiv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4uiv
  :: MonadIO m
  => Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @ColorUI@.
  -> m ()
glColor4uiv v1 = liftIO $ dyn110 ptr_glColor4uiv v1

{-# NOINLINE ptr_glColor4uiv #-}
ptr_glColor4uiv :: FunPtr (Ptr GLuint -> IO ())
ptr_glColor4uiv = unsafePerformIO $ getCommand "glColor4uiv"

-- glColor4us ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4usv'.
glColor4us
  :: MonadIO m
  => GLushort -- ^ @red@ of type @ColorUS@.
  -> GLushort -- ^ @green@ of type @ColorUS@.
  -> GLushort -- ^ @blue@ of type @ColorUS@.
  -> GLushort -- ^ @alpha@ of type @ColorUS@.
  -> m ()
glColor4us v1 v2 v3 v4 = liftIO $ dyn125 ptr_glColor4us v1 v2 v3 v4

{-# NOINLINE ptr_glColor4us #-}
ptr_glColor4us :: FunPtr (GLushort -> GLushort -> GLushort -> GLushort -> IO ())
ptr_glColor4us = unsafePerformIO $ getCommand "glColor4us"

-- glColor4usv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4usv
  :: MonadIO m
  => Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @ColorUS@.
  -> m ()
glColor4usv v1 = liftIO $ dyn112 ptr_glColor4usv v1

{-# NOINLINE ptr_glColor4usv #-}
ptr_glColor4usv :: FunPtr (Ptr GLushort -> IO ())
ptr_glColor4usv = unsafePerformIO $ getCommand "glColor4usv"

-- glColor4x -------------------------------------------------------------------

glColor4x
  :: MonadIO m
  => GLfixed -- ^ @red@.
  -> GLfixed -- ^ @green@.
  -> GLfixed -- ^ @blue@.
  -> GLfixed -- ^ @alpha@.
  -> m ()
glColor4x v1 v2 v3 v4 = liftIO $ dyn53 ptr_glColor4x v1 v2 v3 v4

{-# NOINLINE ptr_glColor4x #-}
ptr_glColor4x :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glColor4x = unsafePerformIO $ getCommand "glColor4x"

-- glColor4xOES ----------------------------------------------------------------

glColor4xOES
  :: MonadIO m
  => GLfixed -- ^ @red@.
  -> GLfixed -- ^ @green@.
  -> GLfixed -- ^ @blue@.
  -> GLfixed -- ^ @alpha@.
  -> m ()
glColor4xOES v1 v2 v3 v4 = liftIO $ dyn53 ptr_glColor4xOES v1 v2 v3 v4

{-# NOINLINE ptr_glColor4xOES #-}
ptr_glColor4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glColor4xOES = unsafePerformIO $ getCommand "glColor4xOES"

-- glColor4xvOES ---------------------------------------------------------------

glColor4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @components@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glColor4xvOES v1 = liftIO $ dyn114 ptr_glColor4xvOES v1

{-# NOINLINE ptr_glColor4xvOES #-}
ptr_glColor4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glColor4xvOES = unsafePerformIO $ getCommand "glColor4xvOES"

-- glColorFormatNV -------------------------------------------------------------

glColorFormatNV
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glColorFormatNV v1 v2 v3 = liftIO $ dyn126 ptr_glColorFormatNV v1 v2 v3

{-# NOINLINE ptr_glColorFormatNV #-}
ptr_glColorFormatNV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
ptr_glColorFormatNV = unsafePerformIO $ getCommand "glColorFormatNV"

-- glColorFragmentOp1ATI -------------------------------------------------------

glColorFragmentOp1ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type [FragmentOpATI](Graphics-GL-Groups.html#FragmentOpATI).
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMask@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> m ()
glColorFragmentOp1ATI v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn127 ptr_glColorFragmentOp1ATI v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glColorFragmentOp1ATI #-}
ptr_glColorFragmentOp1ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColorFragmentOp1ATI = unsafePerformIO $ getCommand "glColorFragmentOp1ATI"

-- glColorFragmentOp2ATI -------------------------------------------------------

glColorFragmentOp2ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type [FragmentOpATI](Graphics-GL-Groups.html#FragmentOpATI).
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMask@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> m ()
glColorFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn128 ptr_glColorFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glColorFragmentOp2ATI #-}
ptr_glColorFragmentOp2ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColorFragmentOp2ATI = unsafePerformIO $ getCommand "glColorFragmentOp2ATI"

-- glColorFragmentOp3ATI -------------------------------------------------------

glColorFragmentOp3ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type [FragmentOpATI](Graphics-GL-Groups.html#FragmentOpATI).
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMask@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> GLuint -- ^ @arg3@.
  -> GLuint -- ^ @arg3Rep@.
  -> GLuint -- ^ @arg3Mod@.
  -> m ()
glColorFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn129 ptr_glColorFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glColorFragmentOp3ATI #-}
ptr_glColorFragmentOp3ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColorFragmentOp3ATI = unsafePerformIO $ getCommand "glColorFragmentOp3ATI"

-- glColorMask -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorMask.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glColorMask.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glColorMask.xhtml OpenGL 4.x>.
glColorMask
  :: MonadIO m
  => GLboolean -- ^ @red@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @green@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @blue@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @alpha@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMask v1 v2 v3 v4 = liftIO $ dyn130 ptr_glColorMask v1 v2 v3 v4

{-# NOINLINE ptr_glColorMask #-}
ptr_glColorMask :: FunPtr (GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMask = unsafePerformIO $ getCommand "glColorMask"

-- glColorMaskIndexedEXT -------------------------------------------------------

-- | This command is an alias for 'glColorMaski'.
glColorMaskIndexedEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaskIndexedEXT v1 v2 v3 v4 v5 = liftIO $ dyn131 ptr_glColorMaskIndexedEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaskIndexedEXT #-}
ptr_glColorMaskIndexedEXT :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaskIndexedEXT = unsafePerformIO $ getCommand "glColorMaskIndexedEXT"

-- glColorMaski ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glColorMask.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glColorMask.xhtml OpenGL 4.x>.
glColorMaski
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaski v1 v2 v3 v4 v5 = liftIO $ dyn131 ptr_glColorMaski v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaski #-}
ptr_glColorMaski :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaski = unsafePerformIO $ getCommand "glColorMaski"

-- glColorMaskiEXT -------------------------------------------------------------

-- | This command is an alias for 'glColorMaski'.
glColorMaskiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaskiEXT v1 v2 v3 v4 v5 = liftIO $ dyn131 ptr_glColorMaskiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaskiEXT #-}
ptr_glColorMaskiEXT :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaskiEXT = unsafePerformIO $ getCommand "glColorMaskiEXT"

-- glColorMaskiOES -------------------------------------------------------------

-- | This command is an alias for 'glColorMaski'.
glColorMaskiOES
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaskiOES v1 v2 v3 v4 v5 = liftIO $ dyn131 ptr_glColorMaskiOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaskiOES #-}
ptr_glColorMaskiOES :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaskiOES = unsafePerformIO $ getCommand "glColorMaskiOES"

-- glColorMaterial -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorMaterial.xml OpenGL 2.x>.
glColorMaterial
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type [ColorMaterialParameter](Graphics-GL-Groups.html#ColorMaterialParameter).
  -> m ()
glColorMaterial v1 v2 = liftIO $ dyn54 ptr_glColorMaterial v1 v2

{-# NOINLINE ptr_glColorMaterial #-}
ptr_glColorMaterial :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glColorMaterial = unsafePerformIO $ getCommand "glColorMaterial"

-- glColorP3ui -----------------------------------------------------------------

glColorP3ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLuint -- ^ @color@.
  -> m ()
glColorP3ui v1 v2 = liftIO $ dyn19 ptr_glColorP3ui v1 v2

{-# NOINLINE ptr_glColorP3ui #-}
ptr_glColorP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glColorP3ui = unsafePerformIO $ getCommand "glColorP3ui"

-- glColorP3uiv ----------------------------------------------------------------

glColorP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> Ptr GLuint -- ^ @color@ pointing to @1@ element of type @GLuint@.
  -> m ()
glColorP3uiv v1 v2 = liftIO $ dyn132 ptr_glColorP3uiv v1 v2

{-# NOINLINE ptr_glColorP3uiv #-}
ptr_glColorP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glColorP3uiv = unsafePerformIO $ getCommand "glColorP3uiv"

-- glColorP4ui -----------------------------------------------------------------

glColorP4ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLuint -- ^ @color@.
  -> m ()
glColorP4ui v1 v2 = liftIO $ dyn19 ptr_glColorP4ui v1 v2

{-# NOINLINE ptr_glColorP4ui #-}
ptr_glColorP4ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glColorP4ui = unsafePerformIO $ getCommand "glColorP4ui"

-- glColorP4uiv ----------------------------------------------------------------

glColorP4uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> Ptr GLuint -- ^ @color@ pointing to @1@ element of type @GLuint@.
  -> m ()
glColorP4uiv v1 v2 = liftIO $ dyn132 ptr_glColorP4uiv v1 v2

{-# NOINLINE ptr_glColorP4uiv #-}
ptr_glColorP4uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glColorP4uiv = unsafePerformIO $ getCommand "glColorP4uiv"

-- glColorPointer --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorPointer.xml OpenGL 2.x>.
glColorPointer
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glColorPointer v1 v2 v3 v4 = liftIO $ dyn133 ptr_glColorPointer v1 v2 v3 v4

{-# NOINLINE ptr_glColorPointer #-}
ptr_glColorPointer :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glColorPointer = unsafePerformIO $ getCommand "glColorPointer"

-- glColorPointerEXT -----------------------------------------------------------

glColorPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride,count)@ elements of type @a@.
  -> m ()
glColorPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn134 ptr_glColorPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorPointerEXT #-}
ptr_glColorPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glColorPointerEXT = unsafePerformIO $ getCommand "glColorPointerEXT"

-- glColorPointerListIBM -------------------------------------------------------

glColorPointerListIBM
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glColorPointerListIBM v1 v2 v3 v4 v5 = liftIO $ dyn135 ptr_glColorPointerListIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorPointerListIBM #-}
ptr_glColorPointerListIBM :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glColorPointerListIBM = unsafePerformIO $ getCommand "glColorPointerListIBM"

-- glColorPointervINTEL --------------------------------------------------------

glColorPointervINTEL
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glColorPointervINTEL v1 v2 v3 = liftIO $ dyn136 ptr_glColorPointervINTEL v1 v2 v3

{-# NOINLINE ptr_glColorPointervINTEL #-}
ptr_glColorPointervINTEL :: FunPtr (GLint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glColorPointervINTEL = unsafePerformIO $ getCommand "glColorPointervINTEL"

-- glColorSubTable -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorSubTable.xml OpenGL 2.x>.
glColorSubTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTarget](Graphics-GL-Groups.html#ColorTableTarget).
  -> GLsizei -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type,count)@ elements of type @a@.
  -> m ()
glColorSubTable v1 v2 v3 v4 v5 v6 = liftIO $ dyn137 ptr_glColorSubTable v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorSubTable #-}
ptr_glColorSubTable :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorSubTable = unsafePerformIO $ getCommand "glColorSubTable"

